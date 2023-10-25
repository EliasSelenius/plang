
// polynomial rolling hash:
u64 string_hash(char* str) {

    // u64 p = 53; // upper case letters
    // u64 p = 31; // lower case letters
    u64 p = 127; // all of ASCII
    u64 m = 1e9 + 9;
    u64 hash = 0;
    u64 p_pow = 1;

    while (*str) {
        // NOTE: I belive the mod is in the loop to avoid overflow
        // hash =  (hash + (*str - 'a' + 1) * p_pow) % m;
        hash =  (hash + *str * p_pow) % m;
        p_pow = (p_pow * p) % m;
        str++;
    }

    return hash;
}

void hash_test() {
    char* strings[] = {
        "aaa", "elias", "elise", "adwaw", "helloworld", "abc", "bca", "foo", "oo", "bar", "br"
    };

    for (u32 i = 0; i < (sizeof(strings)/sizeof(strings[0])); i++) {
        printf("%s -> %llu\n", strings[i], string_hash(strings[i]));
    }
}


typedef struct Hashtable_Entry {
    u32 hash;
    void* data;
} Hashtable_Entry;

typedef struct Hashtable {
    Hashtable_Entry* entries;
    u32 capacity;
} Hashtable;

Hashtable hashtable_create() {
    Hashtable table;

    table.capacity = 32;
    table.entries = malloc(sizeof(Hashtable_Entry) * table.capacity);

    return table;
}

void hashtable_add(Hashtable* table, u32 hash, void* data) {
    u32 index = hash % table->capacity;
    Hashtable_Entry* entry = &table->entries[index];

    if (entry->data != null) {
        printf("Hashtable Collision\n");
        return;
    }


    entry->hash = hash;
    entry->data = data;
}

bool hashtable_contains(Hashtable* table, u32 hash) {

}