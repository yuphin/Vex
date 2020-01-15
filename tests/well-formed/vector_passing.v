
real func fn1(arg1 : real[2])
    var tmp:real;
    tmp := arg1[1];
    arg1[1] := arg1[2];
    arg1[2] := tmp;
    print arg1[1],arg1[2];
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
    call2 := fn1(arg1);
    % should be the same
    print arg1[1],arg1[2];
    call2 := fn2(arg1);
    % should be the same
    print arg1[1],arg1[2];
    return 0;
endfunc