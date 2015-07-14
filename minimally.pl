:- module(minimally,[all_minimally_non_linear/4]).
:- use_module(clauses).

go(F):-
	main(['-prg',F]).

main(ArgV) :-
	cleanup,
	setOptions(ArgV,File,OutS),
	load_file(File),
	clauseIds(Ids),
	all_intensional(Ids),
	create_dependence_graph(Ids,DG),

	assert(mindex(49)),
	mindex(M),
	all_minimally_non_linear(DG,M,Ids,MNLIds),

	writeClausesIds(MNLIds,OutS),
	close(OutS).

%Selects minimally non linear clauses and store their ids in a list in
%the 2nd argument.

all_minimally_non_linear(DG,M,[Id|Ids],[Id|MNLIds]):-
	my_clause(He,B,Id),
	functor(He,H,_),
	indexOfAtom(H,K),

	findall(P,(member(C,B),functor(C,P,_),intensional(P)),Is),
	
	length(Is,L),
	L>1,
	M=K,
	is_minimally_non_linear(DG,M,H,Is),
	!,
	all_minimally_non_linear(DG,M,Ids,MNLIds).
all_minimally_non_linear(DG,M,[_|Ids],MNLIds):-
	all_minimally_non_linear(DG,M,Ids,MNLIds).
all_minimally_non_linear(_,_,[],[]).

is_minimally_non_linear(_,M,_,Bs):-
	findall(B,(member(B,Bs),indexOfAtom(B,M)),Rs),
	Rs=[],
	!.
is_minimally_non_linear(DG,M,H,Bs):-
	findnsols(1,B,(member(B,Bs),indexOfAtom(B,M)),Rs),
	Rs=[R],
	depends(DG,R,H),
	!.
is_minimally_non_linear(_,_,_,[]):-
	!.