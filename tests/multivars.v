% this is a comment

var var1 : real, var2: int, var3: int[], var4: real[];

real func foo()
    var var1:real;

    var2 := 10000;
    var1 := 1;

    while var1 < var2 do
        print var1 + var2;
        read var4,var3;
    endwhile;
    return var2;
endfunc

int func bar()
    var  var1:int,var3 :int[4], var4:real[];

    print foo();

    return var4;
endfunc

int func test()
    return 0;
endfunc

int func main()
    var var1 : real;
    var1 := 3.14523141;
endfunc