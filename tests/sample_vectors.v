   var var4 : real[3];
int func main()
   var var1: real;
   var var2 : int;
   var var3 : int[3];

   var2 := 1441;
   var3 := var3 * var4;
   for var1 := 0 to  2*var2
      var3  := var3 / var4;
      var2 := var2 + 1;
   endfor;

   while var1 > 0 do 
        var4 := var3* var3 + var4;
        var1 := var1 - 1;
        var2 := 2*var1; 
   endwhile;

    return 0 ;
endfunc
