:- module(e_tree_cons,[main/1,go/1]).
:- use_module(clauses).
:- use_module(unfolding).
:- use_module(ls).

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
	clauseIds(Ids).

	test_e_tree_cons(RId,LCls,EurCls),

 	writeClauses(LCls,OutS),
	writeClauses(EurCls,Outs),
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

all_eurekable(FId,[(H:-B)|Cls],ECls):-
	remember_node(FId,H,B,Id),
	is_eurekable(Id),
	!,
	all_eurekable(FId,Cls,ECls1),
	append([(H:-B)],ECls1,ECls2).
all_eurekable(FId,[(H:-B)|Cls],ECls):-
	remember_node(FId,H,B,Id),
	all_eurekable(FId,Cls,ECls).
all_eurekable(_,[],[]).

remember_node(FId,H,B,X):-
	next_node_id(X),
	recorda(FId,my_node(H,B,X)),
	X1 is X+1,
	assert(next_node_id(X1)).



