%:- module(lin_elp,[script/0,lineariseHornELP/4]).
:- module(lin_elp,[lineariseHornELP/4, go/0]).

:- use_module(clauses).
:- use_module(setops).
:- use_module(kdim).
:- use_module(common).

:- use_module(library(terms_check)).
:- use_module(library(pathnames), [path_basename/2, path_concat/3, path_split/3]).
:- use_module(library(terms_vars)).
:- use_module(library(terms)).
:- use_module(library(strings)).
:- use_module(library(lists)).
:- use_module(library(hiordlib)). % foldl is define there



:- dynamic script/1.
:- dynamic db/2.


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
% go('<program_to_be_linearised.horn>').	(write to standard output)
% go('<program_to_be_linearised.horn>','<output_file.txt>').	(write to named output file)

% tested in SWI Prolog

%command line usage (if compiled with SWI Prolog command):
% swipl main.pl


/*add by Bishoksan*/

go:-
    lineariseHornELP('/tmp','fib2.horn','output.txt',2).



lineariseHornELP(ResultDir, File, OutFile, K):-
    retractall(db(_,_)),
	cleanup,
    assert(script(false)),
	set_indexes,
    kdim_out_file(ResultDir, K, File, F_KDIM),
    kdim:main(['-prg', File, '-k', K, '-o', F_KDIM]),
	load_file(F_KDIM),
	clause_ids(Ids),
	create_dependence_graph(DG),
	all_non_linear(Ids,NLIds),
	elp(NLIds,DG),
    open(OutFile, write, OutS),
	show_output(OutS),
	close(OutS).


kdim_out_file(ResultDir, K, File, F_KDIM) :-
    path_basename(File, Orig_F),
    number_atom(K, A),
    atom_concat(Orig_F, A, F_KDIM_K),
    atom_concat(F_KDIM_K, '.pl', F_KDIM_K_PL),
    path_concat(ResultDir, F_KDIM_K_PL, F_KDIM).

/*add by Bishoksan*/



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
    lin_elp:split(Pre,A,Post,Body),
    my_clause(A,Body2,_),
    append(Body2,Post,Body3),
    append(Pre,Body3,Body1),
    separate_constraints(Body1,Cs,Body4),
    makeset(Cs,Cs1),
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
	e_tree_cons4(SId,LCls1,ECls,EDIds),
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
	variant((H1:-B1),(H2:-B2)).

% E-tree construction methods.

e_tree_cons4(RId,LCls,ECls,EDIds):-
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
	recorded(RId,my_node(Hs,Bs,Id)),
    variant((H:-B),(Hs:-Bs)),
	construct_subtree(Id,LCls1,ECls1,EDIds1),
	construct_all_subtrees(RId,FCls,LCls2,ECls2,EDIds2),
	append([LCls1,LCls2],LCls),
	append([ECls1,ECls2],ECls),
	append([EDIds1,EDIds2],EDIds).
construct_all_subtrees(_,[],[],[],[]).

all_eurekable(FId,[(H:-B)|Cls],[(H:-B)|ECls],EDIds):- % Memoization.
	separate_constraints(B,_,Bs),
	remember_node(FId,H,B,_),
	findall(Id,(my_ed(_,EB,Id),subsumes_term(EB,Bs)),Ids),
	Ids=[_|_],
	!,
	all_eurekable(FId,Cls,ECls,EDIds).
all_eurekable(FId,[(H:-B)|Cls],[(H:-B)|ECls1],[EDId|EDIds]):-
	recorded(FId,my_node(Hs,Bs,Id)),
    variant((H:-B),(Hs:-Bs)),
	is_eurekable(Id,Id,T),
	intro_eureka_def(H,T,EDId),
	!,
	all_eurekable(FId,Cls,ECls1,EDIds).
all_eurekable(FId,[(H:-B)|Cls],[(H:-B)|ECls1],EDIds):-
	recorded(FId,my_node(Hs,Bs,Id)),
    variant((H:-B),(Hs:-Bs)),
	is_eurekable(Id,Id,_),
	!,
	all_eurekable(FId,Cls,ECls1,EDIds).
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

selection_rule(B,A) :-
	separate_constraints(B,_,Bs),
	Bs=[A|_]. % predicates in the body are ordered from lower to higher index.

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

% If there is more than one ED whose body generalise the body of the clause to be folded,
% ms_ed selects the most specific conjunction of atoms in the list of bodies EBs and, therefore
% the best ED to fold the given clause.
% 	EBs is a list that contains bodies of EDs (conjunctions of atoms) that generalise
%	the body of the clause to be folded.
%	(H:-B) is the selected ED to fold the given clause. 
ms_ed(EBs,(H:-B)):-
	ms(EBs,B1),
	my_ed(H,B,_),
	variant(B,B1).

% Succeeds if M is the most specific conjunction of atoms in [L|Ls].
ms([L|Ls],M):-
	foldl(most_specific_generalization,Ls,L,M).

% Eureka Definition methods.

linearise_eds([EDId|EDIds],LCls):-
	linearise_ed(EDId,LCls1),
	linearise_eds(EDIds,LCls2),
	append([LCls1,LCls2],LCls).
linearise_eds([],[]).

linearise_ed(EDId,LCls):-
	e_tree_cons(EDId,LCls1,ECls),
	f_tree_cons(ECls,LCls2),
	append([LCls1,LCls2],LCls).

intro_eureka_def(H,T,I):-
	findall(EDId,(my_ed(_,EB,EDId),variant(EB,T)),EDIds),
    EDIds=[], % EDIds is empty if an ED with Bs as body has not been introduced before.
    !,
	functor(H,P,_),
	index_of_atom(P,K),
	eds_id(I),
    atom_number(I1,I),
	atom_concat('new',I1,EN),
	dim_ed(EN,K,EP),
	varset(T,VT),
	append([EP],VT,NHs),
	ED=..NHs,
	assert(my_ed(ED,T,I)),
	set_eds_id.
intro_eureka_def(_,_,_):-
    fail.

msg(B0s,B1s,T):-
	most_specific_generalization(B0s,B1s,T),
	non_trivial_msg(T). % msg has to be a conjunction of atoms.

non_trivial_msg([T|Ts]):-
	nonvar(T),
	non_trivial_msg(Ts),
	!.
non_trivial_msg([]).

% Output methods.

show_output(OutS):-
	findall((H:-B),my_clause(H,B,_),LCls),
	clauseVars(LCls),
	write_clauses(LCls,OutS).

clauseVars([(H:-B)|LCls]):-
	numbervars((H:-B),0,_),
	clauseVars(LCls).
clauseVars([]).

% extra predicates
%motivated from SWI Prolog



/*

recorda(+Key, +Term, -Reference)
Assert Term in the recorded database under key Key. Key is a small integer (range min_tagged_integer ...max_tagged_integer, atom or compound term. If the key is a compound term, only the name and arity define the key. Reference is unified with an opaque handle to the record (see erase/1).

recorda(+Key, +Term)
Equivalent to recorda(Key, Term, _).

*/

recorda(Key, Value):-
    asserta(db(Key, Value)).

recorded(Key, Value):-
    db(Key, Value).

