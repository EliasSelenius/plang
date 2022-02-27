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
    int x = 10;
    x *= 2;
    x -= 4;
    x /= 2;
    x += 2;
    Foo* foo;
    if (true) {
        foo = 0;
    } else if ("Hello") {
        foo = malloc(sizeof(Foo));
    } else {
        foo = (10 + 2);
    }
}
