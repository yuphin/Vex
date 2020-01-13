
int func foo(ch3:int[])
    var var1: int;

    ch3[1] := 10;
    ch3[2] := 11;
    ch3[3] := 12;
    print ch3[1],ch3[2],ch3[3];
endfunc

int func bar(ch2:int[])
    var var1 : int;
    var1 := foo(ch2);
    print ch2[1],ch2[2],ch2[3];
    return 0;
endfunc

int func main(var2:real[])
    var var3 : int;
    var var1 : int[4];
    var3 := 6;
    %print var3;
    read var1[1],var1[2],var1[3];

    var3 := bar(var1);
    return 0;
endfunc


