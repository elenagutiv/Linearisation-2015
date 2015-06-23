:- module(clauses,[cleanup/0,setOptions/3,load_file/1,clauseIds/1,writeClauses/2,my_clause/3]).

:- dynamic my_clause/3.

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
	    C == end_of_file -> true
	;
	    remember_clause(C,N),
	    N1 is N+1,
	    remember_all(S,N1)
	).

remember_clause((H :- B),N) :-
	!,
	tuple2list(B,LB),
	makeClauseId(N,CN),
	assert(my_clause(H,LB,CN)).

remember_clause(H,N) :-
	makeClauseId(N,CN),
	assert(my_clause(H,[],CN)),
	!.
remember_clause((:- _),_).

makeClauseId(N,CN) :-
	name(N,NN),
	append([99],NN,CNN),
	name(CN,CNN).

tuple2list((A,As),[A|LAs]) :-
	!,
	tuple2list(As,LAs).
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

%---

clauseIds(Ids) :-
	findall(C,my_clause(_,_,C),Ids).