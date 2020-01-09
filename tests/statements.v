% declaration lists
var var1 : int;
var var3 : real;
var new_var : int[10], next_var : real[50];

int func bar(var1: real[])
    var var2: real;
    var2 := var1[1];
    print var2,var1[1];
    if var1[2] then
        return 2;
    endif;
    return 1;
endfunc
int func foo()
    var var3: real;
    var new_var3 : real[9];

    % if statement

    read next_var[1],var3;
    if next_var[1] or bar(next_var) then
        return 2;
    else
        print next_var[1];
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
