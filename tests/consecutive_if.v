
int func main(main_arg:real)
    var var1 : real;
    var var2 : int; 
    var var3: int;
    var1 := -5;
    var2 := 0;

    if var1 > 0 then
        return 1;
    else 
        var2 := var1* 1413;
    endif;
 
    for var3 := 1 to 1000 by 1
        var3 := var3*3 + 4;
        return var3;
    endfor;

    
    if var2 > 0 then
        return 2;
    else 
        return var1;
    endif;

    return 1;
endfunc
