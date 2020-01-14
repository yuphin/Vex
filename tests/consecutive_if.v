
real func main(main_arg:real)
    var var1 : real;
    var var2 : real; 
    var var3: real;
    var1 := -5;
    var2 := 0;

    if var1 > 0 then
        return 1;
    else 
        if var2 then
            var2 := var1* 1413;
            print "Var2:",var2;
            return var2;
        else 
            print "Var2(else part):",var2;
            return var2;
        endif;
    endif;
 
    for var3 := 1 to 1000 by 1
        var3 := var3*3 + 4;
        print "Var3",var3;
        return var3;
    endfor;

    
    if var2 > 0 then
        print 2;
        return 2;
    else 
        print "Var1",var1;
        return var1;
    endif;
    print 1;
    return 1;
endfunc
