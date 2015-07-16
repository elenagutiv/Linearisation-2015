:- module(main,[main/1,go/1]).
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
	assert(mindex(49)), %mindex is index of clauses which may be minimally non-linear. It changes after a number of iterations of ELP.
	assert(edsId(1)),

	all_non_linear(Ids,NLIds),
	%% ELP
	elp(NLIds,DG).
	
	%writeClauses(Cls,OutS),
	%close(OutS).

%% ELP
elp(NLIds,DG):-
	mindex(M),
	all_minimally_non_linear(DG,M,NLIds,MNLIds),
	MNLIds=[SId|RIds],

	clp(SId,LCls),

	remember_all_linear(LCls),
	
	update_mindex(MNLIds),
	elp(RIds).

%% Folding and Unfolding operations

fold_clause((H1:-Body1),(H2:-Body2),(H1:-Body3)) :-
		append([Pre,Body2,Post],Body1),
        append([Pre,[H2],Post],Body3),
        numbervars((H1:-Body3)).

unfold((H:-B),A,Clauses) :-
        findall((H:-B1), unfold_clause((H:-B),A,(H:-B1)),Clauses).

unfold_clause((H:-Body),A,(H:-Body1)) :-
		split(Pre,A,Post,Body),
        my_clause(A,Body2,_),
        append(Body2,Post,Body3),
        append(Pre,Body3,Body1),
        numbervars((H:-Body1)).

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
all_non_linear([_|Ids],NLIds):-
	all_non_linear(Ids,NLIds).
all_non_linear([],[]).

%% CLP (Clause Linearisation Procedure)
clp(SId,LCls):-
	e_tree_cons(SId,LCls1,ECls),
	e_tree_del,

	intro_eureka_defs(ECls,EDIds),
	f_tree_cons(ECls,FCls),

	append([LCls1,FCls],LCls2),

	linearise_EDs(EDIds,LCls3),

	append([LCls2,LCls3],LCls).

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
	remember_all_EDs(EDCls).

%% e-tree construction methods
e_tree_cons(RId,LCls,ECls):-
	my_clause(H,B,RId),
	!,
	recorda(0,my_node(H,B,1)),
	assert(next_node_id(2)),
	construct_subtree(1,LCls,ECls).

e_tree_cons(RId,LCls,ECls):-
	my_ed(H,B,RId),
	recorda(0,my_node(H,B,1)),
	assert(next_node_id(2)),
	construct_subtree(1,LCls,ECls).

construct_subtree(RId,LCls,ECls):-
	recorded(_,my_node(H,B,RId)),

	selection_rule(B,A),
	unfold((H:-B),A,Cls),
	all_linear(Cls,LCls1),
	select_list(LCls1,Cls,RCls),
	all_eurekable(RId,RCls,ECls1),
	select_list(ECls1,RCls,FCls),
	construct_all_subtrees(FCls,LCls2,ECls2),

	append([LCls1,LCls2],LCls),
	append([ECls1,ECls2],ECls).

construct_all_subtrees([(H:-B)|FCls],LCls,ECls):-
	recorded(_,my_node(H,B,Id)),

	construct_subtree(Id,LCls1,ECls1),
	construct_all_subtrees(FCls,LCls2,ECls2),

	append([LCls1,LCls2],LCls),
	append([ECls1,ECls2],ECls).
construct_all_subtrees([],[],[]).

all_eurekable(FId,[(H:-B)|Cls],[(H:-B)|ECls1]):-
	remember_node(FId,H,B,Id),
	is_eurekable(Id,Id),
	!,
	all_eurekable(FId,Cls,ECls1).
all_eurekable(FId,[(H:-B)|Cls],ECls):-
	remember_node(FId,H,B,_),
	all_eurekable(FId,Cls,ECls).
all_eurekable(_,[],[]).

is_eurekable(_,1):-
	!,
	fail.
%% is_eurekable(Id0,Id1) is true iff clause of node Id0 is eurekable wrt its father node Id1.
is_eurekable(Id0,Id1):-
	recorded(K1,my_node(_,_,Id1)),
	recorded(_,my_node(_,B1,K1)),

	recorded(_,my_node(_,B0,Id0)),
	is_instance_of(B1,B0),
	!.
is_eurekable(Id0,Id1):-
	recorded(K1,my_node(_,_,Id1)),
	is_eurekable(Id0,K1),
	!.

%% is_instance_of(B1,B2) is true iff the conjunction of atoms in the body B1 are an instance 
%% of the conjunction of atoms in the body B2.
is_instance_of(B1,B2):-
	unifiable(B1,B2,_).

remember_node(FId,H,B,X):-
	next_node_id(X),
	recorda(FId,my_node(H,B,X)),
	X1 is X+1,
	assert(next_node_id(X1)).

e_tree_del:-
	retract_all(my_node(_,_,_)),
	retract_all(next_node_id(_)).

%% selection_rule(B,A) defines a linear lowest-index-first selection rule.
%% It selects from body B an intensional atom whose predicate has the lowest index of those appearing in B and whose transitive closure is linear.
%% By definition of linear transitive closure these are 0-index predicates.
%% In case of more than one 0-index predicate in B, it selects the first from the left.
selection_rule(B,A):-
	findall(C,(member(C,B),functor(C,P,_),intensional(P),indexOfAtom(P,48)),As),
	As=[A|_].

all_linear([(H1:-B1)|Cls],[(H1:-B1)|LCls]):-
	findall(C,(member(C,B1),functor(C,P,_),intensional(P)),Cs),
	length(Cs,L),
	L=<1,
	!,
	all_linear(Cls,LCls).
all_linear([_|Cls],LCls):-
	all_linear(Cls,LCls).
all_linear([],[]).

%% f-tree construction methods
f_tree_cons([(H1:-B1)|ECls],[(H3:-B3)|FCls]):-
	findnsols(1,(H2:-B2),(my_ed(EH,EB,_),fold_clause((H1:-B1),(EH:-EB),(H2:-B2))),FCls),
	FCls=[(H3:-B3)],
	!,
	f_tree_cons(ECls,FCls).
f_tree_cons([],[]).

linearise_EDs([Id|EDIds],LCls):-
	linearise_ED(Id,LCls1),
	linearise_EDs(EDIds,LCls2),

	append([LCls1,LCls2],LCls).
linearise_EDs([],[]).

linearise_ED(Id,LCls):-
	e_tree_cons(Id,LCls1,ECls),
	e_tree_del,

	f_tree_cons(ECls,LCls2),

	append([LCls1,LCls2],LCls).

%% ED Introduction methods
intro_eureka_defs([ECl|ECls],[I|EDIds]):-
	intro_eureka_def(ECl,I),
	intro_eureka_defs(ECls,EDIds).
intro_eureka_defs([],[]).

intro_eureka_def((H:-Bd),I):-
	findall(B,(member(B,Bd),functor(B,Q,_),intensional(Q)),Bs),
	findall(EDId,(my_ed(_,Bs,EDId)),EDIds),
	EDIds=[],
	!,

	functor(H,P,M),
	indexOfAtom(P,K),
	edsId(I),
	atom_concat('new',I,EN),
	dim_ed(EN,K,EP),
	functor(ED,EP,M),

	numbervars((ED:-Bs)),
	assert(my_ed(ED,Bs,I)),

	I1 is I+1,
	assert(edsId(I1)).
intro_eureka_def(_).