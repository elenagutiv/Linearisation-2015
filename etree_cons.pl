:- module(etree_cons,[main/1,go/1]).
:- use_module(clauses).

%% In order to prove this program, module clauses has to contain
%% a compatible version of setOptions definition.

go(F):-
	main(['-prg',F]).

main(ArgV) :-
	cleanup,
	setOptions(ArgV,File,OutS),
	load_file(File),
	clauseIds(Ids).

%% 	%% test_fold takes first clause in the file and folds it wrt to
%% 	%% second clause in the file.
%% 	test_etree_cons(Id,LCls,EurCls),

%% 	writeClauses(LCls,OutS),
%% 	writeClauses(EurCls,Outs),
%% 	close(OutS).


%% etree_cons(Id,[(H1:-B1)|LCls],[(H2:-B2)|EurCls]).
%% etree_cons(_,[],[]).


%% test_etree_cons(Id,LCls,EurCls):-
%% 	etree_cons(Id,LCls,EurCls).

