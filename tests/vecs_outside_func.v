var var1 : int[4];
int func foo(ch3:int[])
    var var1: int;

    ch3[1] := 10;
    ch3[2] := 11;
    ch3[3] := 12;
    print "In foo:",ch3[1],ch3[2],ch3[3];
endfunc

int func bar(ch2:int[])
    print "In bar before foo:",ch2[1],ch2[2],ch2[3];
    % should change since ch2 is not declared in this scope so pass by reference
    foo(ch2);
    print "In bar after foo:",ch2[1],ch2[2],ch2[3];
    return 0;
endfunc

int func main(var2:real[])
    var var3 : int;
    %print var3;
    read var1[1],var1[2],var1[3];
    print "In main before bar:",var1[1],var1[2],var1[3];
    var3 := bar(var1);
    % should be the same since var1 is declared in this scope or in global scope
    print "In main after bar:",var1[1],var1[2],var1[3];
    return 0;
endfunc


