#pragma once

#include "types.h"

typedef enum Typekind {

    Typekind_Invalid = 0, // used by validator to signify a type that could not be determined because of an error
    Typekind_Undecided, // used by parser to mean either struct, enum or alias
    Typekind_MustBeInfered,
    Typekind_AmbiguousInteger,
    Typekind_AmbiguousDecimal,

    Typekind_uint8,
    Typekind_uint16,
    Typekind_uint32,
    Typekind_uint64,

    Typekind_int8,
    Typekind_int16,
    Typekind_int32,
    Typekind_int64,

    Typekind_float32,
    Typekind_float64,

    Typekind_void,
    Typekind_char,

    Typekind_Struct,
    Typekind_Enum,
    Typekind_Alias,
    Typekind_Opaque,
    Typekind_FuncPtr
} Typekind;

typedef struct Datatype {
    Typekind kind;
    /*
        ref:
            Typekind_Undecided  -> Identifier of struct/enum/alias
            Typekind_Struct     -> index into structs
            Typekind_Alias      -> index into aliases
            Typekind_Opaque     -> Identifier
    */
    u32 ref;
    u32 numPointers;
} Datatype;

typedef struct AliasType {
    u32 name;
    Datatype aliasedType;
} AliasType;

inline bool typeEquals(Datatype a, Datatype b) {
    return a.kind == b.kind && a.ref == b.ref && a.numPointers == b.numPointers;
}


#define type_invalid     (Datatype) { Typekind_Invalid, 0, 0 }
#define type_void        (Datatype) { Typekind_void, 0, 0 }
#define type_voidPointer (Datatype) { Typekind_void, 0, 1 }
#define type_char        (Datatype) { Typekind_char, 0, 0 }
#define type_charPointer (Datatype) { Typekind_char, 0, 1 }
#define type_int32       (Datatype) { Typekind_int32, 0, 0 }
#define type_uint32      (Datatype) { Typekind_uint32, 0, 0 }
#define type_int64       (Datatype) { Typekind_int64, 0, 0 }
#define type_uint64      (Datatype) { Typekind_uint64, 0, 0 }
#define type_float32     (Datatype) { Typekind_float32, 0, 0 }
#define type_float64     (Datatype) { Typekind_float64, 0, 0 }
#define type_ambiguousInteger (Datatype) { Typekind_AmbiguousInteger, 0, 0 }
#define type_ambiguousDecimal (Datatype) { Typekind_AmbiguousDecimal, 0, 0 }

void initTypenames();
