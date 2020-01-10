int func main()
   var var1: real;
   var var2 : int;

   var2 := 100;
   for var1 := 1 to  2*var1 -1
        for var2 := 0 to  2*var2
            for var1 := 0 to  2*var2
                for var2 := 0 to  2*var2
                    if var1 = 2 then 
                        return 6;
                    endif;
                    var1 := 2*var2;
                    return 1;
                endfor;
            endfor;
        endfor;
   endfor;

   while var1 do 
        print var1;
            for var1 := 0 to 2*var2 +1
                return 99;
                if var1 = 2 then 
                    return 2;
                endif;
                var1 := 2*var2;
                return var1;
            endfor;
        return 4;
    endwhile;
    return 5;
endfunc
