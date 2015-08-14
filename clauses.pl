:- module(clauses,[cleanup/0,set_options/3,load_file/1,clause_ids/1,write_clauses/2,write_clauses_ids/2,my_clause/3,my_ed/3,index_of_atom/2,cls_id/1,eds_id/1,create_dependence_graph/2,depends/3,remember_clause/2,remember_ed/2,select_list/3,dim_ed/3,tc_is_linear/2,separate_constraints/3,set_of_vars/2,intersect_lists/3]).

:- use_module(library(ugraphs)).

:- dynamic my_clause/3.
:- dynamic my_ed/3.
:- dynamic cls_id/1.
:- dynamic eds_id/1.

%% Clause manipulation functions provided by John Gallagher.

%% Removing data from database
cleanup :-
	retractall(my_clause(_,_,_)),
	retractall(my_ed(_,_,_)).

%% Setting options from input 
set_options(ArgV,File,OutS) :-
	get_options(ArgV,Options,_),
	(member(programO(File),Options) -> true; 
			write(user_output,'No input file given.'),nl(user_output),fail),
	(member(outputFile(OutFile),Options) -> open(OutFile,write,OutS); 
				OutS=user_output).

% get_options/3 provided by Michael Leuschel
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

recognised_option('-prg',  programO(R),[R]).

%% Storing clauses in database
load_file(F) :-
    %retractall(my_clause(_,_,_)),
	open(F,read,S),
	remember_all(S,1),
	close(S).

remember_all(S,N) :-
	read(S,C),
	(
	    C == end_of_file ->
	    asserta(cls_id(N)),
	    true
	;
	    remember_clause(C,N),
	    N1 is N+1,
	    remember_all(S,N1)
	).

remember_clause((H :- B),N) :-
	!,
	tuple_to_list(B,LB),
	make_clause_id(N,CN),
	assertz(my_clause(H,LB,CN)).
remember_clause(H,N) :-
	make_clause_id(N,CN),
	assertz(my_clause(H,[],CN)),
	!.
remember_clause((:- _),_).

%% Stores Eureka Definitions in database
remember_ed((H:-B),N):-
	tuple_to_list(B,LB),
	make_ed_id(N,EN),
	asserta(my_ed(H,LB,EN)).

make_clause_id(N,CN) :-
	name(N,NN),
	append([99],NN,CNN),
	name(CN,CNN).

make_ed_id(N,EN) :-
	name(N,NN),
	append([101],NN,ENN),
	name(EN,ENN).

tuple_to_list((A,As),[A|LAs]) :-
	!,
	tuple_to_list(As,LAs).
tuple_to_list(A,A):-	
	is_list(A),
	!.
tuple_to_list(A,[A]).

%% Showing output
write_clauses([(H:-B)|Rs],S) :-
	writeq(S,H),
	write(S,' :-'),
	%nl(S),
	write_body_atoms(S,B),
	write(S,'.'),
	nl(S),
	write_clauses(Rs,S).
write_clauses([],_).

%% Write clauses Ids

write_clauses_ids([Id|Ids],S):-
	writeq(S,Id),
	write(S,'.'),
	nl(S),
	write_clauses_ids(Ids,S).
write_clauses_ids([],_).
	
write_body_atoms(S,[]) :-
	!,
	%write(S,'   '),
	write(S,' '),
	write(S,true).
write_body_atoms(S,[B]) :-
	!,
	%write(S,'   '),
	write(S,' '),

	writeq(S,B).
write_body_atoms(S,[B1,B2|Bs]) :-
	%write(S,'   '),
	write(S,' '),
	writeq(S,B1),
	write(S,','),
	%nl(S),
	write_body_atoms(S,[B2|Bs]).

clause_ids(Ids) :-
	findall(C,my_clause(_,_,C),Ids).

%% Computes dimension of a given atom.
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

write_pred(S,[I|Is]):-
	writeq(S,I),
	write_pred(S,Is).
write_pred(_,[]).

%% Dependence graph manipulation methods

create_dependence_graph([Id|Ids],DG2):-
	create_nodes_clause([],Id,DG1),
	create_nodes_rest_clauses(DG1,Ids,DG2).

create_nodes_clause(GD1,Id,GD2):-
	my_clause(H,B,Id),
	functor(H,P,_),
	separate_constraints(B,_,Bs),
	findall(Q,(member(C,Bs),functor(C,Q,_)),Qs),
	create_nodes(GD1,P,Qs,GD2).

create_nodes(GD1,P,Qs,GD2):-
	create_node_list(P,Qs,Res),
	add_edges(GD1,Res,GD2).

create_nodes_rest_clauses(DG1,[Id|Ids],DG3):-
	create_nodes_clause(DG1,Id,DG2),
	create_nodes_rest_clauses(DG2,Ids,DG3).
create_nodes_rest_clauses(DG,[],DG).

depends(DG,S,H):-
	reachable(S,DG,Vrs),
	findnsols(1,Vr,(member(Vr,Vrs),Vr=H),Vs),
	Vs=[_].

tc_is_linear(DG,G):-
	reachable(G,DG,Vrs),
	findall((H:-B),(member(H,Vrs),my_clause(H,B,_)),Cls),
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

%% List methods
select_list([L|Ls],Ls2,Ls3):-
	select(L,Ls2,Rs),
	select_list(Ls,Rs,Ls3),
	!.
select_list([],Ls,Ls).

select_list(E1,E2,Ls,Res):-
	append([_,[E1],Ls1],Ls),
	append([Ls2,[E2],_],Ls),
	intersect_lists(Ls1,Ls2,Res),
	!.

%% 3rd argument contains the intersection of 1st and 2nd argument lists.
%% Lists can contain either variables or ground terms. In case of variable lists, it avoids unifications.
intersect_lists([L|Ls1],Ls2,[L|Is]):-
	nu_member(L,Ls2),
	!,
	intersect_lists(Ls1,Ls2,Is).
intersect_lists([_|Ls1],Ls2,Is):-
	intersect_lists(Ls1,Ls2,Is).
intersect_lists([],_,[]).

nu_member(El,[H|T]):-
	nu_member_(T,El,H).

nu_member_(_,El,H):-
	El==H,
	!.
nu_member_([H|T],El,_):-
	nu_member_(T,El,H).

create_node_list(E,L1s,Res):-
	findall(E-L,(member(L,L1s)),Res).

dim_ed(ED,CK,ED1) :-
	ED =.. [P|Xs],
	atom_concat(CK,')',EDK1),
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

separate_constraints([],[],[]).
separate_constraints([B|Bs],[B|Cs],Ds) :-
	constraint(B),
	!,
	separate_constraints(Bs,Cs,Ds).
separate_constraints([B|Bs],Cs,[B|Ds]) :-
	separate_constraints(Bs,Cs,Ds).

set_of_vars(Ls,S):-
	extract_vars(Ls,VLs),
	list_to_set(VLs,S).

extract_vars([L|Ls],Res):-
	L=..FTs,
	FTs=[_|Ts],
	all_vars(Ts,Vs),

	extract_vars(Ls,RVs),
	append(Vs,RVs,Res).
extract_vars([],[]).

all_vars([T|Ts],[T|Vs]):-
	var(T),
	!,
	all_vars(Ts,Vs).
all_vars([_|Ts],Vs):-
	all_vars(Ts,Vs).
all_vars([],[]).