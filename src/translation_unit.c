

typedef struct AliasType {
    u32 name;
    Datatype aliasedType;
} AliasType;


typedef struct TranslationUnit {
    Procedure* procedures; // darray
    PlangStruct* structs; // darray
    VarDecl* globalVariables; // darray
    Constant* constants; // darray

    DynamicBuffer* procPtrTypes;

    AliasType* aliases; // darray
    Identifier* opaqueTypes; // darray

    u32* stringTableByteOffsets; // darray
    DynamicBuffer* stringTable;

} TranslationUnit;

static TranslationUnit* g_Unit;

static inline char* getIdentifierStringValue(Identifier id) { return (char*)(&g_Unit->stringTable->bytes[id]); }
static inline ProcPtr* getProcPtr(u32 id) { return (ProcPtr*)&g_Unit->procPtrTypes->bytes[id]; }

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


static Datatype ensureProcPtr(Procedure* proc) {

    u32 argCount = proc->arguments ? darrayLength(proc->arguments) : 0;

    u32 length = g_Unit->procPtrTypes->length;
    u32 procId = 0;
    while (procId < length) {
        ProcPtr* ptr = getProcPtr(procId);

        if (ptr->argCount == argCount && typeEquals(proc->returnType, ptr->returnType)) {
            for (u32 i = 0; i < argCount; i++)
                if (!typeEquals(proc->arguments[i].type, ptr->argTypes[i])) goto skip;
            return (Datatype) {
                .kind = Typekind_ProcPtr,
                .ref = procId,
                .numPointers = 1
            };
        }

        skip: procId += sizeof(ProcPtr) + sizeof(Datatype) * ptr->argCount;
    }


    procId = dyReserve(&g_Unit->procPtrTypes, sizeof(ProcPtr));
    ProcPtr* ptr = getProcPtr(procId);
    ptr->returnType = proc->returnType;
    ptr->argCount = argCount;
    for (u32 i = 0; i < argCount; i++) {
        u32 argRef = dyReserve(&g_Unit->procPtrTypes, sizeof(Datatype));
        *(Datatype*)(&g_Unit->procPtrTypes->bytes[argRef]) = proc->arguments[i].type;
    }


    return (Datatype) {
        .kind = Typekind_ProcPtr,
        .ref = procId,
        .numPointers = 1
    };
}