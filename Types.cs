using System.Linq;
using static Program;
using pcombinator;
using static pcombinator.Parser;

/*
    struct members
        pointers - prefeix with 'struct'
        itself - error unless it is a pointer

*/

static class Types {

    public static Typename voidPtr => new Typename("void", 1);

    public static bool isPrimitive(Typename t) => 
        isInteger(t) ||
        t.structName switch {
            "float" or 
            "double" or
            "bool" or
            "char" => true,
            _ => false
        };


    public static bool isInteger(Typename t) => t.structName switch {
        // TODO: these 'ids' may change to 
        //    int8, int16, uint32 etc...
        // or i8, i16, u32 etc... 
//    signed:    unsigned:
        "sbyte" or "byte"   or 
        "short" or "ushort" or
        "int"   or "uint"   or
        "long"  or "ulong" 
        => true, _ => false
    };
}


class StructDeclaration : Node {

    public StructDeclaration(string name, StructMember[] members) {
        this.members = members;
        this.name = name;

        Global.structs.Add(name, this);
        
        foreach (var member in members) member.structDecl = this;
    }

    public string name;
    public StructMember[] members;

    public StructMember getMember(string name) => members.Where(x => x.name.Equals(name)).FirstOrDefault();

    public override void transpile() {
        hFile.startblock("typedef struct " + name);
        foreach (var m in members) m.transpile();
        hFile.endblock(name + ";");
    }
}

class StructMember : Node {
    public Typename type;
    public string name;
    public StructDeclaration structDecl;
    
    public override void transpile() {
        if (type.isStruct(out StructDeclaration decl)) {
            hFile.addDependencie(decl.file);
        }

        var transpiledTypeName = type.transpile();
        if (type.structName.Equals(structDecl.name)) {
            if (!type.isPointer) error($"struct \"{structDecl.name}\" cannot contain itself, did you mean to make it a pointer?");

            transpiledTypeName = "struct " + transpiledTypeName;
        } 

        hFile.writeline(transpiledTypeName + " " + name + ";");
    }
}


record Typename(string structName, int numPointers) {

    public bool isPointer => numPointers != 0;
    public bool isVoid => structName.Equals("void") && !isPointer;

    public bool isStruct(out StructDeclaration structDecl) {
        return Global.getStruct(structName, out structDecl);
    }

    string ps() {
        string res = "";
        for (int i = 0; i < numPointers; i++) res += "*";
        return res;
    }

    public string transpile() => structName + ps();

    public bool typeEquals(Typename type) => structName.Equals(type.structName) && numPointers == type.numPointers;
}

record FuncPtrType(Typename retType, Typename[] args) {
    
}