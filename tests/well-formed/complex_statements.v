% declaration lists
var var1 : int;
var var3 : real;
var new_var : int[10], next_var : real[50];

int func bar(var1: real[])
    var var2: real;
    var2 := var1[1];
    print var2,var1[1];
    if var1[1] then
        return 2;
    endif;
    return 0;
endfunc
int func foo()
    var var3: real;
    var new_var3 : real[9];

    % if statement

    % try inputs: 0 0
    read next_var[1],var3;
    if next_var[1] or bar(next_var) then
        return 2;
    else
        read next_var[2],new_var3[1];
        if next_var[2] and new_var3[1] then
            print new_var3[1];
        endif;
        return 0;
    endif;
    % this should be eliminated by ASTChecker
    % for statement
    for var3 := 1 to 1000 by 1
        var3 := var3*3 + 4;
        print var3;
    endfor;
    
    
endfunc

int func main(par1: int, par2: real, par3: real[141])
    var var1 : real;
    print foo();
    var1 := 3.14523141;
endfunc
