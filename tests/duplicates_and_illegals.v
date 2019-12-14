


% duplicate symbol name
var name1 : int, name1: real;

int func newFunc()
    % missing ':=' 
    for name1 to 140000
        print name1;
    endfor;
endfunc

int func funcnamevar1()
    var var2: real , var3 : real;
    % missing ; at the end of endfor
    for var2 := 1413413.3252 to var3 by 1.0
        print var2;
    endfor

    % lexpression instead of expression
    % also var1 doesn't exist
    for var1 := 10 to var5 < 10000 by 2
        read name1;
        print name1;
    endfor;
    
  

    while 1 do
        return 0;
    endwhile;
endfunc

% unknown symbol
int func main()
    return bar();
endfunc



