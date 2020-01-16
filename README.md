# Vex

Vex is a compiler for V language written with flex, bison and LLVM. 
It compiles to MIPS, ARM, x86-64, RISC-V and many other architectures.

V language is mainly designed for vectorization (and possibly parallelization) in mind. Therefore, its performance
is expected to be on-par with popular compilers like clang and gcc.

## An example V program

```
int func fib(n:int)
    if n <= 1 then
        return n;
    endif;
    return fib(n-1) + fib(n-2);
endfunc


int func main()
    var result : int;
    result := fib(20);
    print result;
    return result;
endfunc
```

For more examples, see *tests/* directory
## Requirements
To build Vex, you need the following:
* Bison >= 3.4
* Flex >= 2.6.4
* LLVM >= 9
* C++14

## Building
Provided you have the above dependencies and CMake, you can clone this repo and run 'cmake . && make' to build Vex on Linux. Linux version is
tested with gcc 7.4 and clang 9. In Windows you can build the project with CMake, assuming you have clang on Windows. (Not tested with MSVC or MinGW) Windows version is tested with Visual Studio using clang-cl compiler.
After building, you can run ./vex -h to see help menu.

## Currently supported features
* A simple type system with an error reporting system
* Vector operations
* AST printing
* LLVM IR printing
* Scanner & parser debugging
* Different levels of optimizations, including aggressive ones

## Supported types
* Integers(`int`)
* Real numbers(`real`)
* `void` return type for functions
* `vector` of ints/reals

As indicated in the specs, first class types are passed by value to functions; however vector types are passed by value **when**
the current scope has a declaration of it. That is, vectors which are function parameters are passed by reference to other functions.

## Planned features
* Arrays
* Structs
* V interpreter
* Threading
* Arbitrary compile time execution
