:- module(clauses,[cleanup/0,setOptions/3,load_file/1,clauseIds/1,writeClauses/2,writeClausesIds/2,my_clause/3,my_ed/3,indexOfAtom/2,intensional/1,clsId/1,edsId/1,all_intensional/1,create_dependence_graph/2,depends/3,remember_clause/2,remember_ED/2,select_list/3,dim_ed/3,tc_is_linear/2]).

:- use_module(library(ugraphs)).

:- dynamic my_clause/3.
:- dynamic my_ed/3.
:- dynamic intensional/1.
:- dynamic clsId/1.
:- dynamic edsId/1.

%% Clause manipulation functions provided by John Gallagher.

%% Removing data from database

cleanup :-
	retractall(my_clause(_,_,_)).

%% Setting options from input 

setOptions(ArgV,File,OutS) :-
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
	    asserta(clsId(N)),
	    true
	;
	    remember_clause(C,N),
	    N1 is N+1,
	    remember_all(S,N1)
	).
%% remember_clause((H :- [B]),N) :-
%% 	!,
%% 	makeClauseId(N,CN),
%% 	asserta(my_clause(H,B,CN)).
remember_clause((H :- B),N) :-
	!,
	tuple2list(B,LB),
	makeClauseId(N,CN),
	asserta(my_clause(H,LB,CN)).
remember_clause(H,N) :-
	makeClauseId(N,CN),
	asserta(my_clause(H,[],CN)),
	!.
remember_clause((:- _),_).

%% Stores Eureka Definitions in database
remember_ED((H:-B),N):-
	tuple2list(B,LB),
	makeEDId(N,EN),
	asserta(my_ed(H,LB,EN)).

makeClauseId(N,CN) :-
	name(N,NN),
	append([99],NN,CNN),
	name(CN,CNN).

makeEDId(N,EN) :-
	name(N,NN),
	append([101],NN,ENN),
	name(EN,ENN).

tuple2list((A,As),[A|LAs]) :-
	!,
	tuple2list(As,LAs).
tuple2list(A,A):-	
	is_list(A),
	!.
tuple2list(A,[A]).


%% Showing output

writeClauses([(H:-B)|Rs],S) :-
	writeq(S,H),
	write(S,' :-'),
	%nl(S),
	writeBodyAtoms(S,B),
	write(S,'.'),
	nl(S),
	writeClauses(Rs,S).
writeClauses([],_).

%% Write clauses Ids

writeClausesIds([Id|Ids],S):-
	writeq(S,Id),
	write(S,'.'),
	nl(S),
	writeClausesIds(Ids,S).
writeClausesIds([],_).
	
writeBodyAtoms(S,[]) :-
	!,
	%write(S,'   '),
	write(S,' '),
	write(S,true).
writeBodyAtoms(S,[B]) :-
	!,
	%write(S,'   '),
	write(S,' '),

	writeq(S,B).
writeBodyAtoms(S,[B1,B2|Bs]) :-
	%write(S,'   '),
	write(S,' '),
	writeq(S,B1),
	write(S,','),
	%nl(S),
	writeBodyAtoms(S,[B2|Bs]).

clauseIds(Ids) :-
	findall(C,my_clause(_,_,C),Ids).

%% Computes dimension of a given atom.
indexOfAtom(A,I) :-
	atom_to_chars(A,Ls),
	append(_,[40,I,41|_],Ls),
	!.
indexOfAtom(A,I) :-
	atom_to_chars(A,Ls),
	append(_,[91,I,93|_],Ls),
	!.

%% Collects intensional atoms from a given set of clauses
all_intensional([Id|Ids]):-
	my_clause(_,B,Id),
	member(true,B),
	!,
	all_intensional(Ids).
all_intensional([Id|Ids]):-
	my_clause(H,_,Id),
	functor(H,P,_),
	intensional(P),
	!,
	all_intensional(Ids).
all_intensional([Id|Ids]):-
	my_clause(H,_,Id),
	functor(H,P,_),
	asserta(intensional(P)),
	all_intensional(Ids).
all_intensional([]).

%% Writes set of intensional atoms from a given program
writeIntensional(S):-
	findall(X,intensional(X),Is),
	writePred(S,Is).

writePred(S,[I|Is]):-
	writeq(S,I),
	writePred(S,Is).
writePred(_,[]).

%% Dependence graph manipulation methods

create_dependence_graph([Id|Ids],DG2):-
	create_nodes_clause([],Id,DG1),
	create_nodes_rest_clauses(DG1,Ids,DG2).

create_nodes_clause(GD1,Id,GD2):-
	my_clause(H,Bs,Id),
	functor(H,P,_),
	findall(Q,(member(B,Bs),functor(B,Q,_)),Qs),
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

is_linear((_:-Bd)):-
	findall(B,(member(B,Bd),functor(B,P,_),intensional(P)),Bs),
	length(Bs,L),
	L=<1.

%% List methods
select_list([L|Ls],Ls2,Ls3):-
	select(L,Ls2,Rs),
	select_list(Ls,Rs,Ls3).
select_list([],Ls,Ls).

create_node_list(E,L1s,Res):-
	findall(E-L,(member(L,L1s)),Res).

dim_ed(ED,K,ED1) :-
	ED =.. [P|Xs],
	char_code(CK,K),
	atom_concat(CK,')',EDK1),
	atom_concat('(',EDK1,Suff),
	atom_concat(P,Suff,P1),
	ED1 =.. [P1|Xs].