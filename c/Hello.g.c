typedef struct Test {
    float x;
    double y;
} Test;
typedef struct Foo {
    char* string;
} Foo;
typedef struct Bar30 {
    Foo foo;
} Bar30;
int* foo() {
}
Foo* getAfoo() {
    int a = 10;
    Foo* foos;
    return foos;
}
void main() {
    Foo* foo = malloc(sizeof(Foo) * 10);
    foo[0].string = "Hello";
}
