:- module(unfolding,[unfold/3]).
:- use_module(clauses).

%% In order to prove this program, module clauses has to contain
%% a compatible version of setOptions definition.

go(F):-
	main(['-prg',F]).

main(ArgV) :-
	cleanup,
	setOptions(ArgV,File,OutS),
	load_file(File),
	clauseIds(Ids),

	%% test_unfold takes first clause in file and unfolds it
	%% at the only atom in its body wrt to the set of clauses in file.
	test_unfold(Ids,Clauses),

	writeClauses(Clauses,OutS),
	close(OutS).

% unfold a clause (H:-Body) on a given body atom A, returning Clauses

unfold((H:-B),A,Clauses) :-
        findall((H:-B1), unfold_clause((H:-B),A,(H:-B1)),Clauses).

unfold_clause((H:-Body),A,(H:-Body1)) :-
        append(Pre,[A|Post],Body),
        my_clause(A,Body2,_),
        append(Body2,Post,Body3),
        append(Pre,Body3,Body1),
        numbervars((H:-Body1)).

test_unfold([Id1|_],Clauses) :-
		my_clause(H1,B1,Id1),
		member(A,B1),
		unfold((H1:-B1),A,Clauses).

