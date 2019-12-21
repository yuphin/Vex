int func main()
   var var1: real;
   var var2 : int;
   var kek : int[3];
   var lul : real[3];
   var2 := 1441;
   for var1 := 0 to  2*var2
      kek = kek / lul;
      var2 := var2 + 1;
   endfor;

   while var1 > 0 do 
        lul = kek* kek + lul;
        var1 := var1 - 1;
        var2 := 2*var1; 
   endwhile;

    return 0;
endfunc
