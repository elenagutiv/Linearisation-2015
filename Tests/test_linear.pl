%% 'a(0)'(A,B) :- A>0, B>0.
%% %'p(1)'(A,B) :- 'a(1)'(A,C), 'p[0]'(C,B).
%% 'p(1)'(A,B) :- 'p(1)'(C,B), 'a[0]'(A,C).
%% %'p(1)'(A,B) :- 'a(0)'(A,C), 'p(0)'(C,B).
%% %'d(1)'(A,B) :- 'p(1)'(A,B), 'p[0]'(B,A).
%% %'d(1)'(A,B) :- 'p(1)'(B,A), 'p[0]'(A,B).
%% %'d(1)'(A,B) :- 'p(0)'(A,B), 'p(0)'(B,A).
%% %'a[1]'(A,B) :- 'a(1)'(A,B).
%% %'a[1]'(A,B) :- 'a(0)'(A,B).
%% 'a[0]'(A,B) :- 'a(0)'(A,B).
%% %'d[1]'(A,B) :- 'd(1)'(A,B).
%% %'d[1]'(A,B) :- 'd(0)'(A,B).
%% %'d[0]'(A,B) :- 'd(0)'(A,B).
%% %'p[1]'(A,B) :- 'p(1)'(A,B).
%% %'p[1]'(A,B) :- 'p(0)'(A,B).
%% %'p[0]'(A,B) :- 'p(0)'(A,B).

%% 'p(0)'(A,B) :- A>0, B>0.
%% 'p(1)'(A,B) :- 'p(1)'(A,C), 'p[0]'(C,B).
%% 'p(1)'(A,B) :- 'p(1)'(C,B), 'p[0]'(A,C).
%% 'p(1)'(A,B) :- 'p(0)'(A,C), 'p(0)'(C,B).
%% 'd(1)'(A,B) :- 'p(1)'(A,B), 'p[0]'(B,A).
%% 'd(1)'(A,B) :- 'p(1)'(B,A), 'p[0]'(A,B).
%% 'd(1)'(A,B) :- 'p(0)'(A,B), 'p(0)'(B,A).
%% 'd[1]'(A,B) :- 'd(1)'(A,B).
%% %'d[1]'(A,B) :- 'd(0)'(A,B).
%% %'d[0]'(A,B) :- 'd(0)'(A,B).
%% %'p[1]'(A,B) :- 'p(1)'(A,B).
%% %'p[1]'(A,B) :- 'p(0)'(A,B).
%% 'p[0]'(A,B) :- 'p(0)'(A,B).

'p(0)'(A,B) :- A>0, B>0.
'p(0)'(A,B) :- 'p(0)'(A,B).
't(2)'(A,B) :- C>0,'d(1)'(A,C),'d(1)'(C,B).
'd(1)'(A,B) :- 'p(0)'(A,B), 'p(0)'(B,A).
%'d[2]'(A,B) :- 'd(2)'(A,B).
%'d[2]'(A,B) :- 'd(1)'(A,B).
%'d[2]'(A,B) :- 'd(0)'(A,B).
%'d[1]'(A,B) :- 'd(1)'(A,B).
%'d[1]'(A,B) :- 'd(0)'(A,B).
%'d[0]'(A,B) :- 'd(0)'(A,B).
%'p[2]'(A,B) :- 'p(2)'(A,B).
%'p[2]'(A,B) :- 'p(1)'(A,B).
%'p[2]'(A,B) :- 'p(0)'(A,B).

















