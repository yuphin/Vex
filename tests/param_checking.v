real func bar(arg1: int)
    return 1;
endfunc

real func foo(arg1: real)
    return 1;
endfunc

real func fn(arg1 : int[3])
    return 1;
endfunc 

real func fn2(arg1 : int[])
    return 1;
endfunc 

real func fn3(arg1 : real[2])
    return 1;
endfunc 

real func fn4(arg1 : int[2])
    return 1;
endfunc 

int func main()
    var call1 : int, call2: real;
    var arg1 : int[2];
    var arg2 : real[2];
    % error due to type narrowing to LHS
    call1 := bar(call1);
    % error due to argument type narrowing
    call2 := bar(call2);
    % error due to array size incompatibility
    call2 := fn(arg1);
    % no problem due to unspecified array size
    call2 := fn2(arg1);
    % no problem since we can convert from int[2] to real[2]
    call2 := fn3(arg1);
    % error since we can't convert from real[2] to int[2]
    call2 := fn3(arg1);
endfunc