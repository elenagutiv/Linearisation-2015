:- module(minimally,[main/1,go/1]).
:- use_module(clauses).

go(F):-
	main(['-prg',F]).

main(ArgV) :-
	cleanup,
	setOptions(ArgV,File,OutS),
	load_file(File),
	clauseIds(Ids),

	all_minimally_non_linear(Ids,MNLCls),

	writeClauses(MNLCls,OutS),
	close(OutS).

%Selects minimally non linear clauses and stores them in the list of
%the 2nd argument.

all_minimally_non_linear([Id|Ids],[(H:-B)|MNLCls]):-
	my_clause(H,B,Id),
	length(B,L),
	L>1,
	is_minimally_non_linear(H,B),
	!,
	all_minimally_non_linear(Ids,MNLCls),
	numbervars((H:-B)).
all_minimally_non_linear([Id|Ids],MNLCls):-
	my_clause(_,_,Id),
	!,
	all_minimally_non_linear(Ids,MNLCls).
all_minimally_non_linear([],[]):-
	!.

is_minimally_non_linear(H,[B|Bs]):-
	functor(H,P,_),
	functor(B,Q,_),
	P=Q,
	!,
	is_minimally_non_linear(H,Bs).
is_minimally_non_linear(H,[B|Bs]):-
	functor(H,P,_),
	functor(B,Q,_),
	P\=Q,
	!,
	indexOfAtom(Q,K),
	K=48,	%%48 is ascii code for '0'.
	is_minimally_non_linear(H,Bs).
is_minimally_non_linear(_,[]):-
	!.