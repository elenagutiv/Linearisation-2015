:- module(e_tree_cons,[main/1,go/1]).
:- use_module(clauses).
:- use_module(unfolding).
:- use_module(ls).
:- use_module(minimally).

:-dynamic my_node/3.
:-dynamic next_node_id/1.

%% In order to prove this program, module clauses has to contain
%% a compatible version of setOptions definition.

go(F):-
	main(['-prg',F]).

main(ArgV) :-
	cleanup,
	setOptions(ArgV,File,OutS),
	load_file(File),
	clauseIds(Ids),
	all_intensional(Ids),
	all_minimally_non_linear(Ids,MNLIds),

	[_,_,RId|_]=MNLIds,

	write(OutS,'E-tree Root:'),
	nl(OutS),
	my_clause(RH,RB,RId),
	numbervars((RH:-RB)),
 	writeClauses([(RH:-RB)],OutS),

	test_e_tree_cons(RId,LCls,EurCls),

	nl(OutS),
	write(OutS,'Linear Clauses:'),
	nl(OutS),
 	writeClauses(LCls,OutS),

 	nl(OutS),
 	write(OutS,'Eurekable Clauses:'),
	nl(OutS),
	writeClauses(EurCls,OutS),

	close(OutS).

test_e_tree_cons(RId,LCls,ECls):-
	e_tree_cons(RId,LCls,ECls).

e_tree_cons(RId,LCls,ECls):-
	my_clause(H,B,RId),
	recorda(0,my_node(H,B,1)),
	assert(next_node_id(2)),
	construct_subtree(1,LCls,ECls).

construct_subtree(RId,LCls,ECls):-
	recorded(_,my_node(H,B,RId)),

	selection_rule(B,A),
	unfold((H:-B),A,Cls),
	all_linear(Cls,LCls1),
	select_list(LCls1,Cls,RCls),
	all_folded(RCls,LCls2),

	append([LCls1,LCls2],LCls3),

	select_list(RCls,LCls2,ZCls),
	all_eurekable(RId,ZCls,ECls1),
	select_list(ECls1,RCls,FCls),
	construct_all_subtrees(FCls,LCls4,ECls2),

	append([LCls3,LCls4],LCls),
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

%% all_folded(RCls,LCls) searches among the set of EDs, those that can fold a node of E-tree and
%% give as a result a linear clause.
all_folded([(H1:-B1)|RCls],[(H3:-B3)|LCls]):-
	tuple2list(B1,LB),
	findnsols(1,(H2:-B2),(my_ed(EH:-EB),fold_clause((H1:-B1),(EH:-EB),(H2:-B2))),FCls),
	FCls=[(H3:-B3)],
	!,
	all_folded(RCls,LCls).
all_folded([_|RCls],LCls):-
	all_folded(RCls,LCls).
all_folded([],[]).

%% is_instance_of(B1,B2) is true iff the conjunction of atoms in the body B1 are an instance 
%% of the conjunction of atoms in the body B2.
is_instance_of(B1,B2):-
	unifiable(B1,B2,_).

remember_node(FId,H,B,X):-
	next_node_id(X),
	recorda(FId,my_node(H,B,X)),
	X1 is X+1,
	assert(next_node_id(X1)).

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






