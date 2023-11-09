
set flags=-Wall -Wno-unused-variable -Wno-unused-function -g

pushd bin
clang %flags% -c ../src/stub.c ../src/platform_win32.c
llvm-ar rc library.lib stub.o platform_win32.o
clang %flags% ../src/main.c -llibrary -o plang.exe
popd