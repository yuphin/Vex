int func fib(n:int)
    if n <= 1 then
        return n;
    endif;
    return fib(n-1) + fib(n-2);
endfunc


int func main()
    var result : int;
    result := fib(10);
    print result;
    return result;
endfunc