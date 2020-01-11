
real func fn1(arg1 : real[2])
    var tmp:real;
    tmp := arg1[1];
    arg1[1] := arg1[2];
    arg1[2] := tmp;
    return 1;
endfunc

real func fn2(arg1 : int[2])
    var tmp:int;
    tmp := arg1[1];
    arg1[1] := arg1[2];
    arg1[2] := tmp;
    return 1;
endfunc 

real func main()
    var call2: real;
    var arg1 : int[2];
    read arg1[1],arg1[2];
    % call by value since arg1 is converted from int vector to real vector
    call2 := fn1(arg1);
    print arg1[1],arg1[2];
    % call by reference since arg1 and fn1 param are the same, the result is swapped
    call2 := fn2(arg1);
    print arg1[1],arg1[2];
    return 0;
endfunc