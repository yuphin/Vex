
int func main(main_arg:real)
    var v1 : int;
    var v2 : int;
    var v3 : int;
    var v4 : real;
    v1 := 2;
    v2 := 4161;
    v3 := 13414;
    v4 := 3.14;
    if v1 > v2 then
        v1 := v1+1;
        return v1;
    else
        if v3 or v2 then
           v4 := 1;
        else
            return 5;
       endif; 
    endif; 



    if v1 and v2 then
        return 1;
        v3 := -v1-v2;
    else
        return 5;
         v3 := -v1-v2;
    endif; 

    %This gets removed since we
    %are guaranteed to return 1 or 5 before this point
    % see -emit-ast for details
    return 1;

endfunc
