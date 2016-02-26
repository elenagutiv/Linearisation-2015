:- module(clauses,[cleanup/0,set_options/3,load_file/1,clause_ids/1,write_clauses/2,write_clauses_ids/2,my_clause/3,my_ed/3,index_of_atom/2,cls_id/1,eds_id/1,mindex/1,next_node_id/1, update_dependence_graph/1,create_dependence_graph/1,depends/3,remember_clause/1,set_indexes/0,set_cls_id/0,set_eds_id/0,set_mindex/0,set_mindex/2,select_list/3,dim_ed/3,tc_is_linear/2,is_linear/1,all_clauses_of_index_k/3,intersect_lists/3, append/2]).

:- use_module(library(graphs/ugraphs)).

:- use_module(library(strings)).
:- use_module(library(lists)).


:- use_module(setops).
:- use_module(common).


:- dynamic my_clause/3.
:- dynamic my_ed/3.
:- dynamic cls_id/1.
:- dynamic eds_id/1.
:- dynamic mindex/1.
:- dynamic next_node_id/1.

% Gutierrez Viedma, Elena
% This file contains clause manipulation and other auxiliar methods of ELP procedure referenced in main.pl file.
% For further details see main.pl.


set_indexes:-
	set_cls_id,
	set_eds_id,
	set_mindex. %mindex is the index of clauses which may be minimally non-linear at a certain iteration of ELP.

% Database removal method.

cleanup :-
	retractall(my_clause(_,_,_)),
	retractall(my_ed(_,_,_)),
	retractall(cls_id(_)),
	retractall(eds_id(_)),
	retractall(mindex(_)).

% Setting options from input.

% Provided by J.P. Gallagher.
set_options(ArgV,File,OutS) :-
	get_options(ArgV,Options,_),
	(member(programO(File),Options) -> true; 
			write(user_output,'No input file given.'),nl(user_output),fail),
	(member(outputFile(OutFile),Options) -> open(OutFile,write,OutS); 
				OutS=user_output).

% Provided by Michael Leuschel.
get_options([],[],[]).
get_options([X|T],Options,Args) :-
   (recognised_option(X,Opt,Values) ->
	  ( append(Values, Rest, T),
	    RT = Rest,
	    Options = [Opt|OT], Args = AT
	  )
   ;
	  (
	    Options = OT,	Args = [X|AT],
	    RT = T
	  )
   ),
   get_options(RT,OT,AT).

% Provided by J.P. Gallagher.
recognised_option('-prg',  programO(R),[R]).
recognised_option('-o',    outputFile(R),[R]).

% Methods to store clauses in database. Provided by J.P. Gallagher.

load_file(F) :-
	open(F,read,S),
	remember_all(S),
	close(S).

remember_all(S) :-
	read(S,C),
	(
	    C == end_of_file ->
	    true
	;
	    remember_clause(C),
	    set_cls_id,
	    remember_all(S)
	).

remember_clause((H :- B)) :-
	!,
	cls_id(N),
	tuple_to_list(B,LB),
	make_clause_id(N,CN),
	assert(my_clause(H,LB,CN)).
remember_clause(H) :-
	cls_id(N),
	make_clause_id(N,CN),
	assert(my_clause(H,[],CN)),
	!.
remember_clause((:- _)).

% Computing and storing counters in database.

set_cls_id:-
	retract(cls_id(Id)),
	!,
	NId is Id+1,
	assert(cls_id(NId)).
set_cls_id:-
	assert(cls_id(1)).

set_eds_id:-
	retract(eds_id(Id)),
	!,
	NId is Id+1,
	assert(eds_id(NId)).
set_eds_id:-
	assert(eds_id(1)).

set_mindex(K,NLIds):-
	all_clauses_of_index_k(NLIds,K,Ids),
	Ids=[],
	!,
	N is K+1,
	retract(mindex(K)),
	assert(mindex(N)).
set_mindex(_,_).
set_mindex:-
	assert(mindex(1)).

% Provided by J.P. Gallagher.
make_clause_id(N,CN) :-
	name(N,NN),
	append([99],NN,CNN),
	name(CN,CNN).

clause_ids(Ids) :-
	findall(Id,my_clause(_,_,Id),Ids).

make_ed_id(N,EN) :-
	name(N,NN),
	append([101],NN,ENN),
	name(EN,ENN).

% Ouput methods.

write_clauses([(H:-B)|Rs],S) :-
	writeq(S,H),
	write(S,' :-'),
	write_body_atoms(S,B),
	write(S,'.'),nl(S),
	write_clauses(Rs,S).
write_clauses([],_).

write_clauses_ids([Id|Ids],S):-
	writeq(S,Id),
	write(S,'.'),nl(S),
	write_clauses_ids(Ids,S).
write_clauses_ids([],_).
	
write_body_atoms(S,[]) :-
	!,
	write(S,' '),
	write(S,true).
write_body_atoms(S,[B]) :-
	!,
	write(S,' '),
	writeq(S,B).
write_body_atoms(S,[B1,B2|Bs]) :-
	write(S,' '),
	writeq(S,B1),
	write(S,','),
	write_body_atoms(S,[B2|Bs]).

% Dependence graph manipulation methods.


update_dependence_graph(NDG):-
	create_dependence_graph(NDG).

create_dependence_graph(DG2):-
	clause_ids([Id|Ids]),
	create_nodes_clause([],Id,DG1),
	create_nodes_rest_clauses(DG1,Ids,DG2).

create_nodes_clause(GD1,Id,GD2):-
	my_clause(H,B,Id),
	functor(H,P,N),
	separate_constraints(B,_,Bs),
	findall((Q,M),(member(C,Bs),functor(C,Q,M)),Qs),
	create_nodes(GD1,(P,N),Qs,GD2).

create_nodes(GD1,T,Qs,GD2):-
	create_node_list(T,Qs,Res),
	add_edges(GD1,Res,GD2).

create_nodes_rest_clauses(DG1,[Id|Ids],DG3):-
	create_nodes_clause(DG1,Id,DG2),
	create_nodes_rest_clauses(DG2,Ids,DG3).
create_nodes_rest_clauses(DG,[],DG).

depends(DG,S,H):-
	reachable(S,DG,Vrs),
	findnsols(1,Vr,(member(Vr,Vrs),Vr=(H,_)),Vs),
	Vs=[_].

create_node_list(E,L1s,Res):-
	findall(E-L,(member(L,L1s)),Res).


% Lists manipulation methods.

% Succeeds if list Ls3 is the result of selecting list [L|Ls] from Ls2.
select_list([L|Ls],Ls2,Ls3):-
	select(L,Ls2,Rs),
	select_list(Ls,Rs,Ls3),
	!.
select_list([],Ls,Ls).

% Succeeds if list Res is the result of selecting elements between E1 and E2 (not including those)
% from list Ls.
select_list(E1,E2,Ls,Res):-
	append([_,[E1],Ls1],Ls),
	append([Ls2,[E2],_],Ls),
	intersect_lists(Ls1,Ls2,Res),
	!.

% Succeeds if [L|Is] is the intersection of lists [L|Ls1] and Ls2. It avoids unifications.
intersect_lists([L|Ls1],Ls2,[L|Is]):-
	nu_member(L,Ls2),
	!,
	intersect_lists(Ls1,Ls2,Is).
intersect_lists([_|Ls1],Ls2,Is):-
	intersect_lists(Ls1,Ls2,Is).
intersect_lists([],_,[]).

% Succeeds if El is an element of list [H|T]. It avoids unifications.
nu_member(El,[H|T]):-
	nu_member_(T,El,H).

nu_member_(_,El,H):-
	El==H,
	!.
nu_member_([H|T],El,_):-
	nu_member_(T,El,H).

tuple_to_list((A,As),[A|LAs]) :-
	!,
	tuple_to_list(As,LAs).
tuple_to_list(A,A):-	
	is_list(A),
	!.
tuple_to_list(A,[A]).

%

index_of_atom(A,I) :-
	atom_to_chars(A,Ls),
	select_list(40,41,Ls,Is),
	number_to_chars(I,Is),
	!.
index_of_atom(A,I) :-
	atom_to_chars(A,Ls),
	select_list(91,93,Ls,Is),
	number_to_chars(I,Is),
	!.

tc_is_linear(DG,G):-
	reachable(G,DG,Vrs),
	findall((He:-B),(member((H,N),Vrs),functor(He,H,N),my_clause(He,B,_)),Cls),
	all_are_linear(Cls).

all_are_linear([Cl|Cls]):-
	is_linear(Cl),
	!,
	all_are_linear(Cls).
all_are_linear([]).

is_linear((_:-B)):-
	separate_constraints(B,_,Bs),
	length(Bs,L),
	L=<1.

all_clauses_of_index_k([Id1|Ids1],K,[Id1|Ids2]):-
	index_of_clause(Id1,K),
	!,
	all_clauses_of_index_k(Ids1,K,Ids2).
all_clauses_of_index_k([_|Ids1],K,Ids2):-
	all_clauses_of_index_k(Ids1,K,Ids2).
all_clauses_of_index_k([],_,[]).

index_of_clause(Id,K):-
	my_clause(He,_,Id),
	functor(He,H,_),
	index_of_atom(H,K).

% Succeeds if predicate name ED1 is the result of adding the dimension number (CK)
% to predicate name ED.
dim_ed(ED,CK,ED1) :-
	ED =.. [P|Xs],
    atom_number(CK1,CK),
	atom_concat(CK1,')',EDK1),
	atom_concat('(',EDK1,Suff),
	atom_concat(P,Suff,P1),
	ED1 =.. [P1|Xs].

constraint(_=_).
constraint(_ = _).
constraint(_>_).
constraint(_>=_).
constraint(_=<_).
constraint(_<_).
constraint(0=0).
constraint(1=0).
constraint(true).
constraint(fail).




%predicates defined to cope with the difference with SWI prolog

% extra predicates
%motivated from SWI Prolog

%append(+ListOfLists, ?List)
%Concatenate a list of lists.  List is the concatenation of these lists.
append([], []).
 append([L|Ls], As) :-
 	append(L, Ws, As),
 	append(Ls, Ws).

is_list([]).
is_list([_|R]):-
    is_list(R).

atom_to_chars(A, S):-
    atom_codes(A,S).
number_to_chars(N, S):-
    number_codes(N,S).

/*

reachable(+Vertex, +Graph, -Vertices)
Unify Vertices with the set of all vertices in Graph that are reachable from Vertex. Example:
?- reachable(1,[1-[3,5],2-[4],3-[],4-[5],5-[]],V).
V = [1, 3, 5]

*/

reachable(V, Graph, Vertices):-
    reachable3([V], Graph, [], Vertices1),
    (member(V, Vertices1) -> Vertices=Vertices1; Vertices=[V|Vertices1]).

reachable3([], _, _, []).
reachable3([V|R], Graph, Considered, Vertices):-
    neighbors(V, Graph, Neighbors),
    append(R, Neighbors, ReachableInOneStep),
    difference(ReachableInOneStep, [V|Considered], NotConsideredSofar),
    reachable3(NotConsideredSofar, Graph, [V|Considered], Vertices1),
    append(Neighbors, Vertices1, Vertices).

/*
go :-
    reachable(1,[1-[3,5],2-[4],3-[],4-[5],5-[]],V),
    write(V).
*/



