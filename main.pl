:- module(main,[main/1,go/1,go/2]).
:- use_module(clauses).

% Gutierrez Viedma, Elena
% Adapted from pocedure described in:
% Gutierrez Viedma, Elena.
% Complexity of linearisation procedure ELP
% applied to a k-index bounded set of Horn clauses.

% Input: (for details see below)
% A file containing a set of Horn clauses generating the at-most-k-dimensional
% derivations of a given  program (default ouput of kdim procedure written by J.P. Gallagher)

% Output:
% A linear set of Horn clauses which is the result of applying ELP to the input program.

% Program files:
% main.pl	Contains procedures ELP and CLP and other methods directly related with the procedure.
% clauses.pl 	Contains clause manipulation and other auxiliar methods.

% Usage:
% go('Tests/test_linear.pl').	(write to standard output)
% go('Tests/test_linear.pl','Tests/test_output.txt')	(write to named output file)

% tested in SWI Prolog

%command line usage (if compiled with SWI Prolog command):
% swipl main.pl

go(F):-
	main(['-prg',F]).
go(FI,FO):-
	main(['-prg',FI,'-o',FO]).

main(ArgV) :-
	cleanup,
	set_indexes,
	set_options(ArgV,File,OutS),
	load_file(File),
	clause_ids(Ids),
	create_dependence_graph(DG),
	all_non_linear(Ids,NLIds),
	elp(NLIds,DG),
	show_output(OutS),
	close(OutS).

% Program linearisation procedure
% Tranforms the set of non-linear clauses into a set of linear clauses.
%	NLIds is the set of non-linear clauses in the input program.
%	DG is the dependence graph associated to the program at each iteration.
elp([],_):-
	!.
elp(NLIds,DG):-
	mindex(M),
	all_minimally_non_linear(DG,M,NLIds,MNLIds),
	MNLIds=[SId|_],
	clp(SId,LCls),
	remember_all_linear(LCls),
	retractall(my_clause(_,_,SId)),
	select_list([SId],NLIds,RNLIds),
	update_dependence_graph(NDG),
	set_mindex(M,RNLIds),
	elp(RNLIds,NDG).

% Folding and unfolding operations. 

fold_clause((H1:-Body1),(H2:-Body2),(H1:-Body3)) :-
	select_list(Body2,Body1,Cs),
    append([Cs,[H2]],Body3),
    is_linear((_:-Body3)). % the result of folding operation is a linear clause.

% Provided by J.P. Gallagher.
unfold((H:-B),A,Clauses) :-
	findall((H:-B1), unfold_clause((H:-B),A,(H:-B1)),Clauses).

% Provided by J.P. Gallagher.
unfold_clause((H:-Body),A,(H:-Body5)) :-
	split(Pre,A,Post,Body),
    my_clause(A,Body2,_),
    append(Body2,Post,Body3),
    append(Pre,Body3,Body1),
    separate_constraints(Body1,Cs,Body4),
    list_to_set(Cs,Cs1),
    append(Cs1,Body4,Body5).

split(Pre,A,Post,Body):-
	append(Pre,[A|Post],Body), % unfolding operation works on the first occurence of atom A (if there is more than one) in the given clause.
	!.

% Minimally non linear clause manipulation.

all_minimally_non_linear(DG,M,[Id|Ids],[Id|MNLIds]):-
	my_clause(He,B,Id),
	functor(He,H,_),
	index_of_atom(H,K),
	M=K,
	separate_constraints(B,_,Bs),
	findall((P,N),(member(C,Bs),functor(C,P,N)),Ps),
	is_minimally_non_linear(DG,M,H,Ps),
	!,
	all_minimally_non_linear(DG,M,Ids,MNLIds).
all_minimally_non_linear(DG,M,[_|Ids],MNLIds):-
	all_minimally_non_linear(DG,M,Ids,MNLIds).
all_minimally_non_linear(_,_,[],[]).

is_minimally_non_linear(_,M,_,Bs):-
	findall((B,N),(member((B,N),Bs),index_of_atom(B,M)),Rs),
	Rs=[],
	!.
is_minimally_non_linear(DG,M,H,Bs):-
	findall((B,N),(member((B,N),Bs),index_of_atom(B,M)),Rs),
	Rs=[R],
	depends(DG,R,H),
	!.
is_minimally_non_linear(DG,M,_,Bs):-
	findall((B,N),(member((B,N),Bs),index_of_atom(B,M)),Rs),
	Rs=[R],
	tc_is_linear(DG,R),
	!.
is_minimally_non_linear(_,_,_,[]):-
	!.

% Non-linear clause manipulation.

all_non_linear([Id|Ids],[Id|NLIds]):-
	my_clause(_,B,Id),
	separate_constraints(B,_,Bs),
	length(Bs,L),
	L>1,
	!,
	all_non_linear(Ids,NLIds).
all_non_linear([_|Ids],NLIds):-
	all_non_linear(Ids,NLIds).
all_non_linear([],[]).

% Clause linearisation Procedure.
% Transforms a non-linear clause into a set of linear clauses.
%	SId is the id of the non-linear clause.
%	LCls is the resultant set of linear clauses.
clp(SId,LCls):-
	e_tree_cons(SId,LCls1,ECls,EDIds),
	e_tree_del,
	f_tree_cons(ECls,FCls),
	append([LCls1,FCls],LCls2),
	linearise_eds(EDIds,LCls3),
	append([LCls2,LCls3],LCls).

% Storing linear clauses methods.

remember_all_linear([LCl|LCls]):-
	is_duplicated(LCl),
	!,
	remember_all_linear(LCls).
remember_all_linear([LCl|LCls]):-
	remember_clause(LCl),
	set_cls_id,
	remember_all_linear(LCls).
remember_all_linear([]).

is_duplicated((H1:-B1)):-
	my_clause(H2,B2,_),
	(H1:-B1)=@=(H2:-B2).

% E-tree construction methods.

e_tree_cons(RId,LCls,ECls,EDIds):-
	my_clause(H,B,RId),
	!,
	recorda(0,my_node(H,B,1)),
	set_next_node_id,
	construct_subtree(1,LCls,ECls,EDIds).
e_tree_cons(Id,LCls,ECls):-
	my_ed(H,B,Id),
	recorda(0,my_node(H,B,1)),
	set_next_node_id,
	construct_subtree(1,LCls,ECls,_).

construct_subtree(RId,LCls,ECls,EDIds):-
	recorded(_,my_node(H,B,RId)),
	selection_rule(B,A),
	unfold((H:-B),A,Cls),
	all_linear(Cls,LCls1),
	select_list(LCls1,Cls,RCls),
	all_eurekable(RId,RCls,ECls1,EDIds1),
	select_list(ECls1,RCls,FCls),
	construct_all_subtrees(RId,FCls,LCls2,ECls2,EDIds2),
	append([LCls1,LCls2],LCls),
	append([ECls1,ECls2],ECls),
	append([EDIds1,EDIds2],EDIds).

construct_all_subtrees(RId,[(H:-B)|FCls],LCls,ECls,EDIds):-
	recorded(RId,my_node(H,B,Id)),
	construct_subtree(Id,LCls1,ECls1,EDIds1),
	construct_all_subtrees(RId,FCls,LCls2,ECls2,EDIds2),
	append([LCls1,LCls2],LCls),
	append([ECls1,ECls2],ECls),
	append([EDIds1,EDIds2],EDIds).
construct_all_subtrees(_,[],[],[],[]).

all_eurekable(FId,[(H:-B)|Cls],[(H:-B)|ECls1],[EDId|EDIds]):-
	remember_node(FId,H,B,Id),
	is_eurekable(Id,Id,T),
	intro_eureka_def(H,T,EDId),
	!,
	all_eurekable(FId,Cls,ECls1,EDIds).
all_eurekable(FId,[(H:-B)|Cls],[(H:-B)|ECls1],EDIds):- % This rule only succeeds while e-tree construction wrt to each ED.
	recorded(FId,my_node(H,B,Id)),
	is_eurekable(Id,Id,_),
	!,
	all_eurekable(FId,Cls,ECls1,EDIds).
all_eurekable(FId,[(H:-B)|Cls],[(H:-B)|ECls],EDIds):- % Memoization.
	separate_constraints(B,_,Bs),
	findall(Id,(my_ed(_,EB,Id),subsumes_term(EB,Bs)),Ids),
	Ids=[_],
	!,
	all_eurekable(FId,Cls,ECls,EDIds).
all_eurekable(FId,[_|Cls],ECls,EDIds):-
	all_eurekable(FId,Cls,ECls,EDIds).
all_eurekable(_,[],[],[]).

% Succeeds if first argument references to an eurekable node in e-tree with respect to
% an ancestor node, computed form the second argument.

% Succeeds if Id0 references to an eurekable node wrt the Id1 node's father.
is_eurekable(Id0,Id1,T):-
	recorded(K1,my_node(_,_,Id1)),
	recorded(_,my_node(_,B1,K1)),
	recorded(_,my_node(_,B0,Id0)),
	separate_constraints(B1,_,B1s),
	separate_constraints(B0,_,B0s),
	msg(B0s,B1s,T),
	!.
% Succeeds if Id0 references to an eurekable node wrt an ancestor of Id1 node's father.	
is_eurekable(Id0,Id1,T):-
	recorded(K1,my_node(_,_,Id1)),
	is_eurekable(Id0,K1,T),
	!.
% If Id1 node is the root (1) then no ancestor has been found and Id0 is not eurekable.
is_eurekable(_,1,_):-
	fail.

% E-tree node labeling methods.

set_next_node_id:-
	retract(next_node_id(Id)),
	!,
	NId is Id+1,
	assert(next_node_id(NId)).
set_next_node_id:-
	assert(next_node_id(2)).

remember_node(FId,H,B,Id):-
	next_node_id(Id),
	recorda(FId,my_node(H,B,Id)),
	set_next_node_id,
	!.

% E-tree deletion methods.

e_tree_del:-
	findall(Ref,recorded(_,my_node(_,_,_),Ref),Refs),
	erase_all(Refs),
	retractall(next_node_id(_)).

erase_all([Ref|Refs]):-
	erase(Ref),
	erase_all(Refs).
erase_all([]).

%

selection_rule(B,A):-
	mindex(M),
	K is M-1,
	separate_constraints(B,_,Bs),
	findall(C,(member(C,Bs),functor(C,P,_),index_of_atom(P,J),J=<K),[R|Rs]),
	foldl(lowest_index, Rs, R, A). % A is the predicate with the lowest index in [R|Rs].

lowest_index(A,B,A):-
	functor(A,P,_),
	functor(B,Q,_),
	index_of_atom(P,K1),
	index_of_atom(Q,K2),
	K1<K2,
	!.
lowest_index(_,B,B).

all_linear([(H:-B)|Cls],[(H:-B)|LCls]):-
	separate_constraints(B,_,Bs),
	length(Bs,L),
	L=<1,
	!,
	all_linear(Cls,LCls).
all_linear([_|Cls],LCls):-
	all_linear(Cls,LCls).
all_linear([],[]).

% F-tree construction methods.

f_tree_cons([(H1:-B1)|ECls],[(H2:-B2)|RCls]):-
	separate_constraints(B1,_,B1s),
	findall(EB,(my_ed(_,EB,_),subsumes_term(EB,B1s)),EBs),
	ms_ed(EBs,(H:-B)),
	fold_clause((H1:-B1),(H:-B),(H2:-B2)),
	!,
	f_tree_cons(ECls,RCls).
f_tree_cons([],[]).

ms_ed(EBs,(H:-B)):-
	ms(EBs,B1),
	my_ed(H,B,_),
	B=@=B1.

ms([L|Ls],M):-
	foldl(most_specific,Ls,L,M).

most_specific(B1,B2,B2):-
	subsumes_term(B1,B2),
	\+ subsumes_term(B2,B1).
most_specific(B1,_,B1).

% Eureka Definition methods.

linearise_eds([EDId|EDIds],LCls):-
	linearise_ed(EDId,LCls1),
	linearise_eds(EDIds,LCls2),
	append([LCls1,LCls2],LCls).
linearise_eds([],[]).

linearise_ed(EDId,LCls):-
	e_tree_cons(EDId,LCls1,ECls),
	e_tree_del,
	f_tree_cons(ECls,LCls2),
	append([LCls1,LCls2],LCls).

intro_eureka_def(H,T,I):-
	findall(EDId,(my_ed(_,EB,EDId),EB=@=T),EDIds),
    EDIds=[], % EDIds is empty if an ED with Bs as body has not been introduced before.     !,
	functor(H,P,_),
	index_of_atom(P,K),
	eds_id(I),
	atom_concat('new',I,EN),
	dim_ed(EN,K,EP),
	set_of_vars(T,VT),
	append([EP],VT,NHs),
	ED=..NHs,
	assert(my_ed(ED,T,I)),
	set_eds_id.
intro_eureka_def(_,_,_):-
    fail.

msg(B0s,B1s,T):-
	term_subsumer(B0s,B1s,T),
	non_trivial_msg(T).

non_trivial_msg([T|Ts]):-
	nonvar(T),
	non_trivial_msg(Ts),
	!.
non_trivial_msg([]).

% Output methods.

show_output(OutS):-
	findall((EH:-EB),my_ed(EH,EB,_),EDs),
	clauseVars(EDs),
	write(OutS,'Eureka Definitions:'),nl(OutS),
	write_clauses(EDs,OutS),
	findall((H:-B),my_clause(H,B,_),LCls),
	clauseVars(LCls),
	write(OutS,'Linearised Program:'),nl(OutS),
	write_clauses(LCls,OutS).

clauseVars([(H:-B)|LCls]):-
	numbervars((H:-B),0,_),
	clauseVars(LCls).
clauseVars([]).