:- module(folding,[main/1,go/1]).
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

	%% test_fold takes first clause in the file and folds it wrt to
	%% second clause in the file.
	test_fold(Ids,Clause),

	writeClauses([Clause],OutS),
	close(OutS).

fold_clause((H1:-Body1),(H2:-Body2),(H1:-Body3)) :-
		append([Pre,Body2,Post],Body1),
        append([Pre,[H2],Post],Body3),
        numbervars((H1:-Body3)).

test_fold([Id1|Ids],Clause):-
		my_clause(H1,B1,Id1),
		member(Id2,Ids),
		my_clause(H2,B2,Id2),
		fold_clause((H1:-B1),(H2:-B2),Clause).

