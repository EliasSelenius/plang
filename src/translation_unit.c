

typedef struct AliasType {
    u32 name;
    Datatype aliasedType;
} AliasType;

typedef struct FuncPtr {
    Datatype returnType;
    u32 argCount;
    Datatype argTypes[];
} FuncPtr;

typedef struct TranslationUnit {
    PlangFunction* functions; // darray
    FuncDeclaration* functionDeclarations; // darray
    PlangStruct* structs; // darray
    VarDecl* globalVariables; // darray
    Constant* constants; // darray

    DynamicBuffer* funcPtrTypes;

    AliasType* aliases; // darray
    Identifier* opaqueTypes; // darray

    u32* stringTableByteOffsets; // darray
    DynamicBuffer* stringTable;

} TranslationUnit;

static TranslationUnit* g_Unit;

static inline char* getIdentifierStringValue(Identifier id) { return (char*)(&g_Unit->stringTable->bytes[id]); }
static inline FuncPtr* getFuncPtr(u32 id) { return (FuncPtr*)&g_Unit->funcPtrTypes->bytes[id]; }

static u32 appendStringToStringtable(StrSpan word) {
    u32 len = darrayLength(g_Unit->stringTableByteOffsets);
    for (u32 i = 0; i < len; i++) {
        u32 byteOffset = g_Unit->stringTableByteOffsets[i];
        char* s = (char*)(&g_Unit->stringTable->bytes[byteOffset]);
        if (spanEquals(word, s)) return byteOffset;
    }

    u32 byteOffset = dyReserve(&g_Unit->stringTable, word.length + 1);
    u8* p = (&g_Unit->stringTable->bytes[byteOffset]);
    for (u32 i = 0; i < word.length; i++) p[i] = word.start[i];
    p[word.length] = '\0';

    darrayAdd(g_Unit->stringTableByteOffsets, byteOffset);
    return byteOffset;
}

static Datatype dealiasType(Datatype type) {
    if (type.kind == Typekind_Alias) {
        Datatype newType = g_Unit->aliases[type.ref].aliasedType;
        newType.numPointers += type.numPointers;
        return dealiasType(newType);
    }
    return type;
}
