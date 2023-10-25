// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Runtime.CompilerServices;
using System.Runtime.Serialization;

namespace System.Collections.Generic;


public class Dictionary<TKey, TValue> : IDictionary<TKey, TValue>, IDictionary, IReadOnlyDictionary<TKey, TValue>, ISerializable, IDeserializationCallback where TKey : notnull {

    private int[]? _buckets;
    private Entry[]? _entries;
#if TARGET_64BIT
    private ulong _fastModMultiplier;
#endif
    private int _count;
    private int _freeList;
    private int _freeCount;
    private int _version;
    private IEqualityComparer<TKey>? _comparer;
    private const int StartOfFreeList = -3;

    public Dictionary(int capacity, IEqualityComparer<TKey>? comparer) {
        if (capacity < 0) {
            ThrowHelper.ThrowArgumentOutOfRangeException(ExceptionArgument.capacity);
        }

        if (capacity > 0) {
            Initialize(capacity);
        }

        if (comparer is not null && comparer != EqualityComparer<TKey>.Default) // first check for null to avoid forcing default comparer instantiation unnecessarily
        {
            _comparer = comparer;
        }

        // Special-case EqualityComparer<string>.Default, StringComparer.Ordinal, and StringComparer.OrdinalIgnoreCase.
        // We use a non-randomized comparer for improved perf, falling back to a randomized comparer if the
        // hash buckets become unbalanced.
        if (typeof(TKey) == typeof(string)) {
            IEqualityComparer<string>? stringComparer = NonRandomizedStringEqualityComparer.GetStringComparer(_comparer);
            if (stringComparer is not null) {
                _comparer = (IEqualityComparer<TKey>?)stringComparer;
            }
        }
    }

    public IEqualityComparer<TKey> Comparer {
        get {
            if (typeof(TKey) == typeof(string)) {
                return (IEqualityComparer<TKey>)IInternalStringEqualityComparer.GetUnderlyingEqualityComparer((IEqualityComparer<string?>?)_comparer);
            }
            else {
                return _comparer ?? EqualityComparer<TKey>.Default;
            }
        }
    }

    public int Count => _count - _freeCount;

    public TValue this[TKey key] {
        get {
            ref TValue value = ref FindValue(key);
            if (!Unsafe.IsNullRef(ref value)) {
                return value;
            }

            ThrowHelper.ThrowKeyNotFoundException(key);
            return default;
        }
        set {
            bool modified = TryInsert(key, value, InsertionBehavior.OverwriteExisting);
            Debug.Assert(modified);
        }
    }

    public void Add(TKey key, TValue value) {
        bool modified = TryInsert(key, value, InsertionBehavior.ThrowOnExisting);
        Debug.Assert(modified); // If there was an existing key and the Add failed, an exception will already have been thrown.
    }



    public void Clear() {
        int count = _count;
        if (count > 0) {
            Debug.Assert(_buckets != null, "_buckets should be non-null");
            Debug.Assert(_entries != null, "_entries should be non-null");

            Array.Clear(_buckets);

            _count = 0;
            _freeList = -1;
            _freeCount = 0;
            Array.Clear(_entries, 0, count);
        }
    }

    internal ref TValue FindValue(TKey key) {
        if (key == null) {
            ThrowHelper.ThrowArgumentNullException(ExceptionArgument.key);
        }

        ref Entry entry = ref Unsafe.NullRef<Entry>();
        if (_buckets != null) {
            Debug.Assert(_entries != null, "expected entries to be != null");
            IEqualityComparer<TKey>? comparer = _comparer;
            if (comparer == null) {
                uint hashCode = (uint)key.GetHashCode();
                int i = GetBucket(hashCode);
                Entry[]? entries = _entries;
                uint collisionCount = 0;
                if (typeof(TKey).IsValueType) {
                    // ValueType: Devirtualize with EqualityComparer<TValue>.Default intrinsic

                    i--; // Value in _buckets is 1-based; subtract 1 from i. We do it here so it fuses with the following conditional.
                    do {
                        // Should be a while loop https://github.com/dotnet/runtime/issues/9422
                        // Test in if to drop range check for following array access
                        if ((uint)i >= (uint)entries.Length) {
                            goto ReturnNotFound;
                        }

                        entry = ref entries[i];
                        if (entry.hashCode == hashCode && EqualityComparer<TKey>.Default.Equals(entry.key, key)) {
                            goto ReturnFound;
                        }

                        i = entry.next;

                        collisionCount++;
                    } while (collisionCount <= (uint)entries.Length);

                    // The chain of entries forms a loop; which means a concurrent update has happened.
                    // Break out of the loop and throw, rather than looping forever.
                    goto ConcurrentOperation;
                }
                else {
                    // Object type: Shared Generic, EqualityComparer<TValue>.Default won't devirtualize
                    // https://github.com/dotnet/runtime/issues/10050
                    // So cache in a local rather than get EqualityComparer per loop iteration
                    EqualityComparer<TKey> defaultComparer = EqualityComparer<TKey>.Default;

                    i--; // Value in _buckets is 1-based; subtract 1 from i. We do it here so it fuses with the following conditional.
                    do {
                        // Should be a while loop https://github.com/dotnet/runtime/issues/9422
                        // Test in if to drop range check for following array access
                        if ((uint)i >= (uint)entries.Length) {
                            goto ReturnNotFound;
                        }

                        entry = ref entries[i];
                        if (entry.hashCode == hashCode && defaultComparer.Equals(entry.key, key)) {
                            goto ReturnFound;
                        }

                        i = entry.next;

                        collisionCount++;
                    } while (collisionCount <= (uint)entries.Length);

                    // The chain of entries forms a loop; which means a concurrent update has happened.
                    // Break out of the loop and throw, rather than looping forever.
                    goto ConcurrentOperation;
                }
            }
            else {
                uint hashCode = (uint)comparer.GetHashCode(key);
                int i = GetBucket(hashCode);
                Entry[]? entries = _entries;
                uint collisionCount = 0;
                i--; // Value in _buckets is 1-based; subtract 1 from i. We do it here so it fuses with the following conditional.
                do {
                    // Should be a while loop https://github.com/dotnet/runtime/issues/9422
                    // Test in if to drop range check for following array access
                    if ((uint)i >= (uint)entries.Length) {
                        goto ReturnNotFound;
                    }

                    entry = ref entries[i];
                    if (entry.hashCode == hashCode && comparer.Equals(entry.key, key)) {
                        goto ReturnFound;
                    }

                    i = entry.next;

                    collisionCount++;
                } while (collisionCount <= (uint)entries.Length);

                // The chain of entries forms a loop; which means a concurrent update has happened.
                // Break out of the loop and throw, rather than looping forever.
                goto ConcurrentOperation;
            }
        }

        goto ReturnNotFound;

    ConcurrentOperation:
        ThrowHelper.ThrowInvalidOperationException_ConcurrentOperationsNotSupported();
    ReturnFound:
        ref TValue value = ref entry.value;
    Return:
        return ref value;
    ReturnNotFound:
        value = ref Unsafe.NullRef<TValue>();
        goto Return;
    }

    private int Initialize(int capacity) {
        int size = HashHelpers.GetPrime(capacity);
        int[] buckets = new int[size];
        Entry[] entries = new Entry[size];

        // Assign member variables after both arrays allocated to guard against corruption from OOM if second fails
        _freeList = -1;
#if TARGET_64BIT
        _fastModMultiplier = HashHelpers.GetFastModMultiplier((uint)size);
#endif
        _buckets = buckets;
        _entries = entries;

        return size;
    }

    private bool TryInsert(TKey key, TValue value, InsertionBehavior behavior) {
        // NOTE: this method is mirrored in CollectionsMarshal.GetValueRefOrAddDefault below.
        // If you make any changes here, make sure to keep that version in sync as well.

        if (key == null) {
            ThrowHelper.ThrowArgumentNullException(ExceptionArgument.key);
        }

        if (_buckets == null) {
            Initialize(0);
        }
        Debug.Assert(_buckets != null);

        Entry[]? entries = _entries;
        Debug.Assert(entries != null, "expected entries to be non-null");

        IEqualityComparer<TKey>? comparer = _comparer;
        uint hashCode = (uint)((comparer == null) ? key.GetHashCode() : comparer.GetHashCode(key));

        uint collisionCount = 0;
        ref int bucket = ref GetBucket(hashCode);
        int i = bucket - 1; // Value in _buckets is 1-based

        if (comparer == null) {
            if (typeof(TKey).IsValueType) {
                // ValueType: Devirtualize with EqualityComparer<TValue>.Default intrinsic
                while (true) {
                    // Should be a while loop https://github.com/dotnet/runtime/issues/9422
                    // Test uint in if rather than loop condition to drop range check for following array access
                    if ((uint)i >= (uint)entries.Length) {
                        break;
                    }

                    if (entries[i].hashCode == hashCode && EqualityComparer<TKey>.Default.Equals(entries[i].key, key)) {
                        if (behavior == InsertionBehavior.OverwriteExisting) {
                            entries[i].value = value;
                            return true;
                        }

                        if (behavior == InsertionBehavior.ThrowOnExisting) {
                            ThrowHelper.ThrowAddingDuplicateWithKeyArgumentException(key);
                        }

                        return false;
                    }

                    i = entries[i].next;

                    collisionCount++;
                    if (collisionCount > (uint)entries.Length) {
                        // The chain of entries forms a loop; which means a concurrent update has happened.
                        // Break out of the loop and throw, rather than looping forever.
                        ThrowHelper.ThrowInvalidOperationException_ConcurrentOperationsNotSupported();
                    }
                }
            }
            else {
                // Object type: Shared Generic, EqualityComparer<TValue>.Default won't devirtualize
                // https://github.com/dotnet/runtime/issues/10050
                // So cache in a local rather than get EqualityComparer per loop iteration
                EqualityComparer<TKey> defaultComparer = EqualityComparer<TKey>.Default;
                while (true) {
                    // Should be a while loop https://github.com/dotnet/runtime/issues/9422
                    // Test uint in if rather than loop condition to drop range check for following array access
                    if ((uint)i >= (uint)entries.Length) {
                        break;
                    }

                    if (entries[i].hashCode == hashCode && defaultComparer.Equals(entries[i].key, key)) {
                        if (behavior == InsertionBehavior.OverwriteExisting) {
                            entries[i].value = value;
                            return true;
                        }

                        if (behavior == InsertionBehavior.ThrowOnExisting) {
                            ThrowHelper.ThrowAddingDuplicateWithKeyArgumentException(key);
                        }

                        return false;
                    }

                    i = entries[i].next;

                    collisionCount++;
                    if (collisionCount > (uint)entries.Length) {
                        // The chain of entries forms a loop; which means a concurrent update has happened.
                        // Break out of the loop and throw, rather than looping forever.
                        ThrowHelper.ThrowInvalidOperationException_ConcurrentOperationsNotSupported();
                    }
                }
            }
        }
        else {
            while (true) {
                // Should be a while loop https://github.com/dotnet/runtime/issues/9422
                // Test uint in if rather than loop condition to drop range check for following array access
                if ((uint)i >= (uint)entries.Length) {
                    break;
                }

                if (entries[i].hashCode == hashCode && comparer.Equals(entries[i].key, key)) {
                    if (behavior == InsertionBehavior.OverwriteExisting) {
                        entries[i].value = value;
                        return true;
                    }

                    if (behavior == InsertionBehavior.ThrowOnExisting) {
                        ThrowHelper.ThrowAddingDuplicateWithKeyArgumentException(key);
                    }

                    return false;
                }

                i = entries[i].next;

                collisionCount++;
                if (collisionCount > (uint)entries.Length) {
                    // The chain of entries forms a loop; which means a concurrent update has happened.
                    // Break out of the loop and throw, rather than looping forever.
                    ThrowHelper.ThrowInvalidOperationException_ConcurrentOperationsNotSupported();
                }
            }
        }

        int index;
        if (_freeCount > 0) {
            index = _freeList;
            Debug.Assert((StartOfFreeList - entries[_freeList].next) >= -1, "shouldn't overflow because `next` cannot underflow");
            _freeList = StartOfFreeList - entries[_freeList].next;
            _freeCount--;
        }
        else {
            int count = _count;
            if (count == entries.Length) {
                Resize();
                bucket = ref GetBucket(hashCode);
            }
            index = count;
            _count = count + 1;
            entries = _entries;
        }

        ref Entry entry = ref entries![index];
        entry.hashCode = hashCode;
        entry.next = bucket - 1; // Value in _buckets is 1-based
        entry.key = key;
        entry.value = value;
        bucket = index + 1; // Value in _buckets is 1-based
        _version++;

        // Value types never rehash
        if (!typeof(TKey).IsValueType && collisionCount > HashHelpers.HashCollisionThreshold && comparer is NonRandomizedStringEqualityComparer) {
            // If we hit the collision threshold we'll need to switch to the comparer which is using randomized string hashing
            // i.e. EqualityComparer<string>.Default.
            Resize(entries.Length, true);
        }

        return true;
    }


    private void Resize() => Resize(HashHelpers.ExpandPrime(_count), false);
    private void Resize(int newSize, bool forceNewHashCodes) {
        // Value types never rehash
        Debug.Assert(!forceNewHashCodes || !typeof(TKey).IsValueType);
        Debug.Assert(_entries != null, "_entries should be non-null");
        Debug.Assert(newSize >= _entries.Length);

        Entry[] entries = new Entry[newSize];

        int count = _count;
        Array.Copy(_entries, entries, count);

        if (!typeof(TKey).IsValueType && forceNewHashCodes) {
            Debug.Assert(_comparer is NonRandomizedStringEqualityComparer);
            _comparer = (IEqualityComparer<TKey>)((NonRandomizedStringEqualityComparer)_comparer).GetRandomizedEqualityComparer();

            for (int i = 0; i < count; i++) {
                if (entries[i].next >= -1) {
                    entries[i].hashCode = (uint)_comparer.GetHashCode(entries[i].key);
                }
            }

            if (ReferenceEquals(_comparer, EqualityComparer<TKey>.Default)) {
                _comparer = null;
            }
        }

        // Assign member variables after both arrays allocated to guard against corruption from OOM if second fails
        _buckets = new int[newSize];
#if TARGET_64BIT
        _fastModMultiplier = HashHelpers.GetFastModMultiplier((uint)newSize);
#endif
        for (int i = 0; i < count; i++) {
            if (entries[i].next >= -1) {
                ref int bucket = ref GetBucket(entries[i].hashCode);
                entries[i].next = bucket - 1; // Value in _buckets is 1-based
                bucket = i + 1;
            }
        }

        _entries = entries;
    }

    public bool Remove(TKey key) {
        // The overload Remove(TKey key, out TValue value) is a copy of this method with one additional
        // statement to copy the value for entry being removed into the output parameter.
        // Code has been intentionally duplicated for performance reasons.

        if (key == null) {
            ThrowHelper.ThrowArgumentNullException(ExceptionArgument.key);
        }

        if (_buckets != null) {
            Debug.Assert(_entries != null, "entries should be non-null");
            uint collisionCount = 0;
            uint hashCode = (uint)(_comparer?.GetHashCode(key) ?? key.GetHashCode());
            ref int bucket = ref GetBucket(hashCode);
            Entry[]? entries = _entries;
            int last = -1;
            int i = bucket - 1; // Value in buckets is 1-based
            while (i >= 0) {
                ref Entry entry = ref entries[i];

                if (entry.hashCode == hashCode && (_comparer?.Equals(entry.key, key) ?? EqualityComparer<TKey>.Default.Equals(entry.key, key))) {
                    if (last < 0) {
                        bucket = entry.next + 1; // Value in buckets is 1-based
                    }
                    else {
                        entries[last].next = entry.next;
                    }

                    Debug.Assert((StartOfFreeList - _freeList) < 0, "shouldn't underflow because max hashtable length is MaxPrimeArrayLength = 0x7FEFFFFD(2146435069) _freelist underflow threshold 2147483646");
                    entry.next = StartOfFreeList - _freeList;

                    if (RuntimeHelpers.IsReferenceOrContainsReferences<TKey>()) {
                        entry.key = default!;
                    }

                    if (RuntimeHelpers.IsReferenceOrContainsReferences<TValue>()) {
                        entry.value = default!;
                    }

                    _freeList = i;
                    _freeCount++;
                    return true;
                }

                last = i;
                i = entry.next;

                collisionCount++;
                if (collisionCount > (uint)entries.Length) {
                    // The chain of entries forms a loop; which means a concurrent update has happened.
                    // Break out of the loop and throw, rather than looping forever.
                    ThrowHelper.ThrowInvalidOperationException_ConcurrentOperationsNotSupported();
                }
            }
        }
        return false;
    }


    public bool TryGetValue(TKey key, [MaybeNullWhen(false)] out TValue value) {
        ref TValue valRef = ref FindValue(key);
        if (!Unsafe.IsNullRef(ref valRef)) {
            value = valRef;
            return true;
        }

        value = default;
        return false;
    }

    public bool TryAdd(TKey key, TValue value) => TryInsert(key, value, InsertionBehavior.None);


    /// <summary>
    /// Ensures that the dictionary can hold up to 'capacity' entries without any further expansion of its backing storage
    /// </summary>
    public int EnsureCapacity(int capacity) {
        if (capacity < 0) {
            ThrowHelper.ThrowArgumentOutOfRangeException(ExceptionArgument.capacity);
        }

        int currentCapacity = _entries == null ? 0 : _entries.Length;
        if (currentCapacity >= capacity) {
            return currentCapacity;
        }

        _version++;

        if (_buckets == null) {
            return Initialize(capacity);
        }

        int newSize = HashHelpers.GetPrime(capacity);
        Resize(newSize, forceNewHashCodes: false);
        return newSize;
    }


    /// <summary>
    /// Sets the capacity of this dictionary to hold up 'capacity' entries without any further expansion of its backing storage
    /// </summary>
    /// <remarks>
    /// This method can be used to minimize the memory overhead
    /// once it is known that no new elements will be added.
    /// </remarks>
    public void TrimExcess(int capacity) {
        if (capacity < Count) {
            ThrowHelper.ThrowArgumentOutOfRangeException(ExceptionArgument.capacity);
        }

        int newSize = HashHelpers.GetPrime(capacity);
        Entry[]? oldEntries = _entries;
        int currentCapacity = oldEntries == null ? 0 : oldEntries.Length;
        if (newSize >= currentCapacity) {
            return;
        }

        int oldCount = _count;
        _version++;
        Initialize(newSize);

        Debug.Assert(oldEntries is not null);

        CopyEntries(oldEntries, oldCount);
    }

    private void CopyEntries(Entry[] entries, int count) {
        Debug.Assert(_entries is not null);

        Entry[] newEntries = _entries;
        int newCount = 0;
        for (int i = 0; i < count; i++) {
            uint hashCode = entries[i].hashCode;
            if (entries[i].next >= -1) {
                ref Entry entry = ref newEntries[newCount];
                entry = entries[i];
                ref int bucket = ref GetBucket(hashCode);
                entry.next = bucket - 1; // Value in _buckets is 1-based
                bucket = newCount + 1;
                newCount++;
            }
        }

        _count = newCount;
        _freeCount = 0;
    }



    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private ref int GetBucket(uint hashCode) {
        int[] buckets = _buckets!;
#if TARGET_64BIT
        return ref buckets[HashHelpers.FastMod(hashCode, (uint)buckets.Length, _fastModMultiplier)];
#else
        return ref buckets[hashCode % (uint)buckets.Length];
#endif
    }

    private struct Entry {
        public uint hashCode;
        /// <summary>
        /// 0-based index of next entry in chain: -1 means end of chain
        /// also encodes whether this entry _itself_ is part of the free list by changing sign and subtracting 3,
        /// so -2 means end of free list, -3 means index 0 but on free list, -4 means index 1 but on free list, etc.
        /// </summary>
        public int next;
        public TKey key;     // Key of entry
        public TValue value; // Value of entry
    }
}