
#include <stdio.h>

typedef struct String {
    unsigned int length;
    char c_str[];
} String;

int main() {

    char* s1 = "daw";
    char s2[] = "daw";

    // let str1 = "Hello";
    String str1 = { 5, "Hello" };

    String* str2 = malloc(sizeof(unsigned int) + 6);
    *str2 = str1;

    return 0;
}