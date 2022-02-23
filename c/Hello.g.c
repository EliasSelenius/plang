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
void main() {
    int a = 5;
    Foo* foo = malloc(sizeof(Foo));
    Bar30** bar = malloc(sizeof(Bar30*));
    char* msg = "Hello, World!";
    bool b = true;
}
