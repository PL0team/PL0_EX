// test for short-circuit calculation
var i,j;
{ //begin
  // you can use (j != 0) for "j"
 if(i>j && j && j > 10 && i && (i>20) || i>100 || j >= 100 && i <= 40 && (j <= 20 || i >= -10))
 		i = 2;
} //end
$

//A related codes  for test!
/*
		0 JMP	0	1
    1 INT	0	5
    2 LOD	0	3
    3 LOD	0	4
    4 JLE	0	15 // <=
    5 LOD	0	4
    6 JZ	0	15 // equal to 0
    7 LOD	0	4
    8 LIT	0	10
    9 JLE	0	15
   10 LOD	0	3
   11 JZ	0	15
   12 LOD	0	3
   13 LIT	0	20
   14 JG	0	31 // > 
   15 LOD	0	3
   16 LIT	0	100
   17 JG	0	31
   18 LOD	0	4
   19 LIT	0	100
   20 JL	0	34  // <
   21 LOD	0	3
   22 LIT	0	40
   23 JG	0	34 // >
   24 LOD	0	4
   25 LIT	0	20
   26 JLE	0	31 // <=
   27 LOD	0	3
   28 LIT	0	10
   29 NEG	0	1  // -10
   30 JL	0	34
   31 LIT	0	2
   32 STO	0	3  // keep value on top of stack!
   33 POP	0	0  // so, pop up when value on top of stack does NOT need at all! 
   34 RET	1	0
*/




