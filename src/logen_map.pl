:- module(logen_map, _).
/*
based on linearSolveProg.pl interpreter
*/

:- dynamic pred_map/2.

:- use_module(library(lists)).
:- use_module(library(dynamic)).
:- use_module(library(read)).
:- use_module(load_simple).

:- use_module(library(process), [process_call/3]).

:- use_module(common).

go:-
    recoverOriginalPred('../mc91.pl.lin',  '../mc91.pl.lin.rec.pl').

/*
Given a linearised file produced using logen, it recovers a program wrt the original program.

*/

recoverOriginalPred(F_Logen,  F_Orig):-
    get_predicateMap(F_Logen),
    load_file(F_Logen),
    open(F_Orig, write, S),
    replacePredicates,
    writeClauses(S),
    close(S).

replacePredicates:-
    my_clause(H, B,CId),
    separate_constraints(B, Cs, Bs), % since all clauses are linear, Bs =[] | [B1]
    functor(H, P, N),
    H=..[_|Args],
    (pred_map(P/N, P1/N)-> H1=..[P1|Args]; H1=H),
    (Bs=[]-> B2=[]
    ;
    Bs=[B1],
    functor(B1, BP1, BN1),
    B1=..[_|Args1],
    (pred_map(BP1/BN1, BP2/BN1)-> B3=..[BP2|Args1], B2=[B3]; B2=[B1])),
    append(Cs, B2, Body),
    retract(my_clause(_,_,CId)),
    assert(my_clause(H1,Body,CId)),
    fail.
replacePredicates.

writeClauses(S) :-
	my_clause(A,Body, _),
    %do not write clauses whose head is go__0, since it si logen entry predicate
    (A=go__0 -> true
    ;
        numbervars((A,Body),0,_),
        writeq(S,A),
        write(S,' :- '),
        nl(S),
        writeBodyClauses(Body,S),
        write(S,'.'),
        nl(S)
    ),
	fail.
writeClauses(_).
	
writeBodyClauses([],S) :-
	write(S,'      '),
	write(S,true).
writeBodyClauses([A],S) :-
	!,
	write(S,'      '),
	writeq(S,A).
writeBodyClauses([A|As],S) :-
	write(S,'      '),
	writeq(S,A),
	write(S,','),
	nl(S),
	writeBodyClauses(As,S).



get_predicateMap(F):-
    cleanup,
    load_file(F),
    F1='comments.pl',
    parse_comments(F, F1),
    load_file(F1),
    process_call(path('rm'), [F1],[]),
    predicateMap.
    %printMap.

 /*
sed -n 's/^\/\*  \(.*\)\. \*\//\1./p' $1 > logen_renames.pl
in prolog '\' has to be replaced by '\\'

*/


parse_comments(F, F1):-
    process_call(path('sed'), ['-n', 's/^\\/\\*  \\(.*\\)\\. \\*\\//\\1./p', F],
	             [stdout(file(F1))]).


/*
map pred_map(PE_predicates, Original_predicates)

search for clauses whose:
     -body constains at most one non-constraint atom
    - the head takes 3 arguments
    - the first argument is a single element list


*/
predicateMap:-
    %look for only one atom in the body
    my_clause(H, [B], _),
    H=..[_|[[A1], _, _]],
    functor(A1, P1,N1),
    functor(B, P,N),
    assert(pred_map(P/N, P1/N1)),
    fail.
predicateMap.

printMap:-
    pred_map(O,PE),
    write(O), write(' -- '), write(PE), nl,
    fail.
printMap.

cleanup:-
    retractall(pred_map(_,_)).




