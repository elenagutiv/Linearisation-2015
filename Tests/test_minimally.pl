'path(1)'(A,B) :- 'a(1)'(A,B).
'path(0)'(A,B) :- 'a(0)'(A,B).
'path(1)'(A,B) :- 'a(1)'(A,C), 'path[0]'(C,B).
'path(1)'(A,B) :- 'path(1)'(C,B), 'a[0]'(A,C).
'path(1)'(A,B) :- 'a(0)'(A,C), 'path(0)'(C,B).
'double(1)'(A,B) :- 'path(1)'(A,B), 'path[0]'(B,A).
'double(1)'(A,B) :- 'path(1)'(B,A), 'path[0]'(A,B).
'double(1)'(A,B) :- 'path(0)'(A,B), 'path(0)'(B,A).
'a(0)'(1,2) :- true.
'a(0)'(2,3) :- true.
'a(0)'(3,2) :- true.
'a[1]'(A,B) :- 'a(1)'(A,B).
'a[1]'(A,B) :- 'a(0)'(A,B).
'a[0]'(A,B) :- 'a(0)'(A,B).
'double[1]'(A,B) :- 'double(1)'(A,B).
'double[1]'(A,B) :- 'double(0)'(A,B).
'double[0]'(A,B) :- 'double(0)'(A,B).
'path[1]'(A,B) :- 'path(1)'(A,B).
'path[1]'(A,B) :- 'path(0)'(A,B).
'path[0]'(A,B) :- 'path(0)'(A,B).