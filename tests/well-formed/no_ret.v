real func foo()
    var var1 : int;
    var1 := 5;
    return var1;
endfunc

int func test()
    var var1 : int;
    var1 := -1;
    if var1 > 0 then
        var1 := 6;
        %return 5;
    else
        return 10;
    endif;
endfunc

int func if_only()
    var var1 : int;
    var1 := -1;
    if var1 > 0 then
        return 2;
    endif;
endfunc


int func loops()
    var var1 : int;
    var var2: real;
    for var1 := 10 to 200 by 2
            if 1 then 
                return 1;
            endif;
            for var2 := 0 to  2*var2
                return 6;
            endfor;
    endfor;
endfunc


real func while_loop()
    var var1 :int;
    var1 := 1;
    while var1 do
        while var1 do
            return 7.0;
        endwhile;
        return 6.6;
        while var1 do 
            return 6.5;
        endwhile;
    endwhile;
endfunc

real func plain_real()
    var var1 : real[1000];
    read var1[1];
endfunc

int func main()

    var var1 : real;
    % should read val then print 0.0
    print plain_real();
    % should print 7
    print while_loop();
    % should print 1
    print loops();
    % should print 0
    print if_only();
    % should print 10
    print test();
    % should print 5
    print foo();
    var1 := 6;
endfunc
