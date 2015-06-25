:- module(linear,[main/1,go/1]).
:- use_module(clauses).

go(F):-
	main(['-prg',F]).

main(ArgV) :-
	cleanup,
	setOptions(ArgV,File),
	load_file(File),
	clauseIds(Ids),

	%% ELP
	all_minimally_non_linear(Ids,MNLIds),
	clp(MNLIds,LCls,EDCls),

	writeClauses(Cls,OutS),
	close(OutS).

%% Minimally non linear clauses Manipulation
all_minimally_non_linear([Id|Ids],[MNLId|MNLIds]):-
	my_clause(H,B,Id),
	length(B,L),
	L>1,
	is_minimally_non_linear(H,B),
	!,
	my_clause(H,B,MNLId),
	all_minimally_non_linear(Ids,MNLIds).
all_minimally_non_linear([Id|Ids],MNLIds):-
	my_clause(_,_,Id),
	!,
	all_minimally_non_linear(Ids,MNLIds).
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


%% CLP (Clause Linearisation Procedure)

clp([Id|Ids],LCls,EDefs):-
	%etree_construction(Id,LCls1,EurCls)
	%ed_introduction(EurCls,EDefs1),
	%etree_folding(EurCls,EDefs1,LCls2),
	%ftree_construction(EDefs1,LCls3),

	%clp(Ids,)
.


clp([],[],[]).
