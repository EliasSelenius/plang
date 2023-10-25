
#include <stdio.h>
#include <stdlib.h>

#include <Windows.h>

#include "src/prelude.h"
#include "src/list.c"

/*
    D:\Documents\repos\plang\minbug.pog
        ..\grax\grax.pog        ->      D:\Documents\repos\grax\grax.pog
        renderer.pog            ->      D:\Documents\repos\grax\renderer.pog

*/


typedef unsigned int uint32;

typedef struct string {
    char* chars;
    uint32 length;
} string;

bool string_equals(string a, string b) {
    if (a.length != b.length) return false;
    for (u32 i = 0; i < a.length; i++) if (a.chars[i] != b.chars[i]) return false;
    return true;
}

string to_string(char* c_str) {
    return (string) {
        .chars = c_str,
        .length = strlen(c_str)
    };
}

typedef struct Path {
    char* source_string;
    string* dirs; // list
    string file_name;
} Path;


Path parse_path(char* str) {

    Path path = {0};
    path.source_string = str;
    path.dirs = list_create(string);

    uint32 i = 0;
    char* start = str;

    bool loop = true;
    while (loop) {
        while (start[i] != '\\' && start[i] != '/') {
            if (start[i] == '\0') loop = false;
            i++;
        }

        string dir = { .chars = start, .length = i };
        list_add(path.dirs, dir);

        start = &start[i + 1];
        i = 0;
    }

    path.file_name = list_pop(path.dirs);

    return path;
}

void mod_path(Path* path, Path* rel_path) {
    foreach (dir, rel_path->dirs) {
        if (string_equals(*dir, to_string(".."))) {
            (void)list_pop(path->dirs);
        } else {
            list_add(path->dirs, *dir);
        }
    }
}

void print_path(Path path) {
    foreach (dir, path.dirs) {
        printf("\"%.*s\" ", dir->length, dir->chars);
    }
    printf("\n");
}

int main(int argc, char* argv[]) {

    {
        char* rel_path = "../file_that_no_exists";
        char buf[1024];
        char* file_name = NULL;
        GetFullPathName(rel_path, sizeof(buf), buf, &file_name);

        printf("\"%s\", \"%s\"\n", file_name, buf);

        return 0;
    }


    // char buffer[1024];
    // GetCurrentDirectory(sizeof(buffer), buffer);

    char* buffer = "D:\\Documents\\repos\\plang\\minbug.pog";


    printf("\"%s\"\n", buffer);
    Path path = parse_path(buffer);
    print_path(path);

    printf("\n%s\n", argv[1]);
    Path rel_path = parse_path(argv[1]);
    print_path(rel_path);

    mod_path(&path, &rel_path);
    print_path(path);

    return 0;
}
