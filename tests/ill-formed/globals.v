var var1: real;
var var2 : int;
int func main()
  
   var2 := 1441;
   for var1 := 0 to  2*var2
      var2 := var2 + 1;
   endfor;

   while var1 > 0 do 
        var1 := var1 - 1;
        var2 := 2*var1; 
   endwhile;

    return 0;
endfunc
