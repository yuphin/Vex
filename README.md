# Vex

Vex is a compiler for V language written with flex, bison and LLVM. 
It compiles to MIPS, ARM, x86-64, RISC-V and many other architectures.

V language is mainly designed for vectorization (and possibly parallelization) in mind. Therefore, it's performance
is expected to be on-par with popular compilers like clang and gcc.

## An example V program

```
int func fib(n:int)
    if n <= 1 then
        return 1;
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

As indicated in the specs, first class types are passed by value to functions; however vector types are passed by value **if**
they are casted to different types(eg. int to real), otherwise they are passed by reference. Type narrowing in V is currently forbidden.

## Planned features
* Arrays
* Structs
* V interpreter
* Threading
* Arbitrary compile time execution
