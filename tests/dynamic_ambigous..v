% from the groups

int func main()
   var i: int;
   var x: int[3];

   i := 1;
   x[1] := 0;
   x[2] := 0;
   x[3] := 0;

   for x[i] := 1 to 3
      print x[1],x[2],x[3],i;

      i := i + 1;
   endfor;
      print x[1],x[2],x[3],i;

   return 0;
endfunc

