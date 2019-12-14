% missing type
var var1: real, 
var2, var3: int[], var4: real[];

% missing type
int func foo(var1: real, var2)
    return 0;
endfunc


real func bar()
    if var1 then
        if var1 then
            % no then
            if var1
                if var1 then
                    if var1 then
                        if var1 then
                        % no then
                            if var1 
                                while not var1 do
                                    % illegal for statement
                                    for  gibberish  (var3[0] + var4[0]) to 141*var2;
                                    endfor;
                                endwhile;
                            else
                                return 0;
                            endif;
                        endif;
                    endif;
                endif;
            endif;
        endif;
    endif;
endfunc


int func main()
    return bar();
endfunc