% declaration lists
var var1 : int;
var var3 : real;
var new_var : int[10], next_var : real[50];

int func bar()
    return 1;
endfunc
int func foo()
    var var3: real;
    var new_var3 : int[9];

    % if statement

    if var3 = new_var and bar() then
        return 2;
    else
        read next_var,var3;
        print next_var;
        return 0;
    endif;

    % for statement
    for var3 := 1 to 1000 by 1
        var3 := var3*3 + 4;
        print var3;
    endfor;
    
endfunc

int func main(par1: int, par2: real, par3: real[141])
    var var1 : real;
    var1 := 3.14523141;
endfunc
