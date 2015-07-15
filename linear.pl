:- module(linear,[main/1,go/1]).
:- use_module(clauses).

go(F):-
	main(['-prg',F]).

main(ArgV) :-
	cleanup,
	setOptions(ArgV,File),
	load_file(File),
	clauseIds(Ids),
	all_intensional(Ids),
	create_dependence_graph(Ids,DG),
	assert(mindex(49)),
	assert(edsId(1)),

	all_non_linear(Ids,NLIds),
	%% ELP
	elp(NLIds),
	
	writeClauses(Cls,OutS),
	close(OutS).

%% ELP
elp(NLIds):-
	mindex(M),
	all_minimally_non_linear(DG,M,NLIds,MNLIds),
	MNLIds=[SId|RIds],

	clp(SId,LCls,EDCls),

	remember_all_linear(LCls),
	remember_all_EDs(EDCls),

	update_mindex(MNLIds),
	elp(RIds).

%% Minimally non linear clauses Manipulation
all_minimally_non_linear(DG,M,[Id|Ids],[Id|MNLIds]):-
	my_clause(He,B,Id),
	functor(He,H,_),
	indexOfAtom(H,K),

	findall(P,(member(C,B),functor(C,P,_),intensional(P)),Is),
	
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

%% Non-linear clause Manipulation
all_non_linear([Id|Ids],[Id|NLIds]):-
	my_clause(_,Bds,Id),
	findall(B,(member(B,Bds),functor(B,P,_),intensional(P)),Bs),
	length(Bs,L),
	L>1,
	!,
	all_non_linear(Ids,NLIds).
all_non_linear([Id|Ids],NLIds):-
	all_non_linear(Ids,NLIds).
all_non_linear([],[]).

%% CLP (Clause Linearisation Procedure)
%% TODO

update_mindex(MNLIds):-
	length(MNLIds,L),
	L-1=0,
	!,
	mindex(M),
	N is M+1,
	assert(mindex(N)).
update_mindex(_).

%% Remember linear clauses and eureka definition methods
remember_all_linear([LCl|LCls]):-
	clsId(Id),
	remember_clause(LCl,Id),
	NId is Id+1,
	assert(clsId(NId)),
	remember_all_linear(LCls).

remember_all_EDs([EDCl|EDCls]):-
	edsId(Id),
	remember_ED(EDCl,Id),
	NId is Id+1,
	assert(edsId(NId)),
	remember_all_EDs(EDCLs).
