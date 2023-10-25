

#define Include_Recursive

#include "src/macro_enum.h"


#define let(name, expr) typeof(expr) name = (expr)

int main() {
    // printf("Hello, World! %d", 2);

    let (name, 12);
    return 0;
}