int func main()
    var i: int;
    var x: int[3];

    i := 1;
    x[1] := 0;
    x[2] := 0;
    x[3] := 0;

    print x[1],x[2],x[3];
    x[i] := 5;
    x[i+1] := 10;
    print x[1],x[2],x[3];
   return 0;
endfunc

