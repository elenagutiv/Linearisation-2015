:- module(linear,[main/1,go/1]).
:- use_module(clauses).

go(F):-
	main(['-prg',F]).

main(ArgV) :-
	cleanup,
	setOptions(ArgV,File),
	load_file(File),
	clauseIds(Ids),
	%% ELP
	writeClauses(Cls,OutS),
	close(OutS).

%% ELP Procedure