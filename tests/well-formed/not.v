int func main(main_arg:real)
    var test1 : int;
    var test2 : real;
    var test3 : real[1];
    test1 := 0;
    test2 := 1;
    test3[1] := test2;

    if not test1 then
        print "test1 is 0";
    else
        print "test 1 is not 0"; 
    endif;
    
    if not test3[1] then
        print "test3[1] is 0";
    else
        print "test3[1] is not 0, it is ", test3[1]; 
    endif;
endfunc