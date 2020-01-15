void func bar()
    var test1 : int[1];
    test1[1] := 6;
endfunc

void func main(main_arg:real)
    var var1: int;
    bar();
    print "I called bar() and will return void";
endfunc