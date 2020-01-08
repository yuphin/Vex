


int func bar(ch2:int[])
    var var1 : int;
    %var1 := ch2[1];
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


