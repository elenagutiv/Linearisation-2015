%% ELP selects in turns minimally non-linear clauses, starting with those of index k and
%% once every non-linear clause  of index k is linearised, continuing with those of index k+1.

'p(1)'(A,B):-'y(1)'(A,B),'y[0]'(A,B).
'd(1)'(A,B):-'p(1)'(A,B),'y[0]'(A,B).
'z(2)'(A,B):- 'y(1)'(A,C),'y(1)'(C,B).
'y(1)'(A,B):- 'x(1)'(A,C),'x(0)'(C,B).
'x(1)'(A,B):- 'y(1)'(A,B),'x(0)'(A,B).
'x(1)'(A,B):- 'x(0)'(A,C),'x(0)'(C,B).
'x(0)'(A,B):-A>0,'x(0)'(A,B).
'x(0)'(A,B):-A>0,B>0.
'y[0]'(A,B):-'y(0)'(A,B).
'd(1)'(A,B):-'d(1)'(A,B).
'p(1)'(A,B):-'p(1)'(A,B).