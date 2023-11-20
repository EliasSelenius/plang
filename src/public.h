

typedef struct Codebase {

    struct Procedure** procedures; // list
    struct Declaration** global_vars; // list
    struct Declaration** global_consts; // list
    struct Struct** structs; // list
    struct Enum** enums; // list
    struct Typedef** type_defs; // list

} Codebase;

Codebase parse();
void addFile(char* filename, char* extension);
