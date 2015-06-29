:- module(minimally,[all_minimally_non_linear/2]).
:- use_module(clauses).

go(F):-
	main(['-prg',F]).

main(ArgV) :-
	cleanup,
	setOptions(ArgV,File,OutS),
	load_file(File),
	clauseIds(Ids),
	all_intensional(Ids),

	all_minimally_non_linear(Ids,MNLIds),

	writeClausesIds(MNLIds,OutS),
	close(OutS).

%Selects minimally non linear clauses and store their ids in a list in
%the 2nd argument.

all_minimally_non_linear([Id|Ids],[Id|MNLIds]):-
	my_clause(H,B,Id),
	findall(C,(member(C,B),functor(C,P,_),intensional(P)),Is),
	length(Is,L),
	L>1,
	is_minimally_non_linear(H,Is),
	!,
	all_minimally_non_linear(Ids,MNLIds).
all_minimally_non_linear([_|Ids],MNLIds):-
	all_minimally_non_linear(Ids,MNLIds).
all_minimally_non_linear([],[]).

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