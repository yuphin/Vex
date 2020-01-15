% from the google group

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
   % note that this is NOT an undefined behaviour
   % because x[i] is initially 1, which is saved into a register
   % then we increment x[i] by one, but at that point i was already
   % increased by 1, so we increment x[2] this time and so on so forth..

   return 0;
endfunc

