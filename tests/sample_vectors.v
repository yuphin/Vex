var var4 : real[3];

real func bar(ar1:int[], ar2: real[3],vr1:real,vr2:int,ar3:real[])
    print vr1;
    print vr2;
    print ar2[1],ar2[2],ar2[3];
    print ar3[1],ar3[2],ar3[3];
    return ch[1];
endfunc
int func main()
	var var1: real;
    var var2 : int;
    var var3 : int[3];
    var var5 : real[3];
    var2 := 5;
    read var3[1],var3[2],var3[3];
    read var4[1],var4[2],var4[3];
    read var5[1],var5[2],var5[3];
    var3 := var3 * var4;
    
    for var1 := 0 to  2*var2
    	var3  := var3 * var4;
		print var3[1],var3[2],var3[3];
   endfor;

   print var1;
   while var1 > 0 do 
        var4 := var3* (var3 + var4) + var5 ;
        var1 := var1 - 1;
        var2 := 2*var1; 
   endwhile;

   print var3[1],var3[2],var3[3];

   var1 := bar(var3,var4,var1,var2,var5);
endfunc
