'x(2)'(A,B):-'x(2)'(A,B),'x[1]'(A,B).
'x(2)'(A,B):-'x(1)'(A,B),'x(1)'(A,B).
'x(1)'(A,B):-'x(0)'(A,C),'x(0)'(C,B).
'x(1)'(A,B):-'d(1)'(A,C),'x[0]'(C,B),A>0.
'd(1)'(A,B):-'f(1)'(A,B).
'f(1)'(A,B):-'f(1)'(A,B).
'x(0)'(A,B):-A>B.
'x(0)'(A,B):-A>C,'x(0)'(C,B).
'x[1]'(A,B):-'x(1)'(A,B).
'x[1]'(A,B):-'x(0)'(A,B).
'x[0]'(A,B):-'x(0)'(A,B).