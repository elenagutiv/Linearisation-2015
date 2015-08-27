%% If the set of variables (Vs) appearing in the predicates of the eurekable body do not
%% occur in the rest of the clause, the eureka predicate is defined on Vs.
%% Otherwise, the ones we do not include would depend on those we include.

'p(1)'(A):-A>0,'d(0)'(B,C),'d(0)'(C,B).
'd(0)'(A,B):-'d(0)'(A,B).