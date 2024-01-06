
set flags=-Wall -Wno-unused-variable -Wno-unused-function -g

pushd bin
clang %flags% -c ../src/stub.c ../platform/win32.c
llvm-ar rc pog_compiler.lib stub.o win32.o
clang %flags% ../src/main.c -lpog_compiler -o plang.exe
popd