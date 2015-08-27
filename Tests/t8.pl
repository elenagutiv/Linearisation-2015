'x(0)'(A,B):-A>0,'x(0)'(A,B).
'y(0)'(A,B):-B>0,'y(0)'(A,B).
'y(1)'(A,B):-'x(0)'(A,C),'x(0)'(C,B).

'x(1)'(A,B):-'y(0)'(A,B),'x(0)'(A,B). %U
'x(2)'(A,B):-'x(1)'(A,B),'x(1)'(A,B). %U
'x(2)'(A,B):-'y(1)'(A,B),'y(1)'(A,B). %U

%% PROGRAM AFTER REMOVING USELESS CLAUSES (U)
%% 'x(0)'(A,B):-A>0,'x(0)'(A,B).
%% 'y(0)'(A,B):-B>0,'y(0)'(A,B).
%% 'y(1)'(A,B):-'x(0)'(A,C),'x(0)'(C,B).

%% SOLUTION [Removing useless clauses (U) from the previous program does not affect the ouput of the procedure]

%% Eureka Definitions:
%% 'new1(1)'(A,B) :- 'x(0)'(A,C), 'x(0)'(C,B).
%% Linearised Program:
%% 'x(0)'(A,B) :- A>0, 'x(0)'(A,B).
%% 'y(0)'(A,B) :- B>0, 'y(0)'(A,B).
%% 'y(1)'(A,B) :- A>0, 'new1(1)'(A,B).
%% 'new1(1)'(A,B) :- A>0, 'new1(1)'(A,B).