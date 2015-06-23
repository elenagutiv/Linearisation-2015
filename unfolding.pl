:- module(unfolding,[main/1,go/1]).
:- use_module(clauses).


go(F):-
	main(['-prg',F]).

main(ArgV) :-
	cleanup,
	setOptions(ArgV,File,OutS),
	load_file(File),
	clauseIds(Ids),

	%% Folding
	unfold_frst_wrt_scnd(Ids,Clauses),

	writeClauses(Clauses,OutS),
	close(OutS).

% unfold a clause (H:-Body) on a given body atom A, returning Clauses

unfold((H:-B),A,Clauses) :-
        findall((H:-B1), unfoldClause((H:-B),A,(H:-B1)),Clauses).

unfoldClause((H:-Body),A,(H:-Body1)) :-
        append(Pre,[A|Post],Body),
        my_clause(A,Body2,_),
        append(Body2,Post,Body3),
        append(Pre,Body3,Body1),
        numbervars((H:-Body1),0,_).

unfold_frst_wrt_scnd([Id1|_],Clauses) :-
		my_clause(H1,B1,Id1),
		member(A,B1),
		unfold((H1:-B1),A,Clauses).

