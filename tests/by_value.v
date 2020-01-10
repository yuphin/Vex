
int func bar(var1: int, var2: real)
    var1 := -999;
    var2 := 1.64;
    print var1,var2;
endfunc

int func main(main_arg:real)
    var var1 : int, var2 : real;
    var var3 :int;
    var1 := 50;
    var2 := 70;
    print var1,var2;
    var3 := bar(var1,var2);
    print var1,var2;

    % the result should be
    % 50 70.0
    % -999 1.64
    % 50 70.0
endfunc