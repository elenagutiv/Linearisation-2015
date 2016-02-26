
/*
the input is a set of Horn clauses and a stack bound and output is a set of linear Horn clauses

*/

:- module(lin_pe, _).


:- use_module(library(lists)).
:- use_module(library(system_extra), [mkpath/1]).
:- use_module(library(system), [copy_file/3]).
:- use_module(library(process), [process_call/3]).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(pathnames), [path_basename/2, path_concat/3, path_split/3]).
:- use_module(library(bundle/paths_extra), [fsR/2]).
:- use_module(library(system_extra), [mktempdir_in_tmp/2, rmtempdir/1,mkpath/1]).

:- use_module(load_simple).
:- use_module(logen_map).
:- use_module(common).
:- use_module(chc2logen).




go:-
    linearise('/Users/kafle/Linearisation-2015/case-studies/P0/mccarthy91.horn', 'linearSolveProg_k_perm.pl', 'linearSolve_k_perm.pl.ann', 2, 'mc91.lin.pl').
/*
test:-
    lin_pe:main(['-prg', '/Users/kafle/Linearisation-2015/case-studies/P0/mccarthy91.horn','-k', 2, '-o', 'mc91.lin.pl']).
*/


logen_executable(Logen) :-
	find_bundle_cmd(logen, Logen0),
	!,
	Logen = Logen0.
logen_executable(_Logen) :-
	format(user_error, "ERROR: logen does not seem to be installed. Please follow usage instructions to install it.~n~n", []),
	throw(error_logen_not_found). % TODO: throw exception?

% ---------------------------------------------------------------------------


:- use_module(engine(internals), [top_ciao_path/1]).
:- use_module(library(system), [file_exists/1]).

% Lookup a bundle command (either in the build/bin directory of top
% CIAOPATH, or the current executable directory)
find_bundle_cmd(Cmd, Path) :-
	% Try find Cmd in the same directory as the executable
	current_executable(ExecPath),
	path_split(ExecPath, ExecDir, _),
	path_concat(ExecDir, Cmd, Path0),
	file_exists(Path0),
	!,
	Path = Path0.
find_bundle_cmd(Cmd, Path) :-
	% Otherwise look in <CIAOPATH>/build/bin 
	top_ciao_path(Top),
	path_concat(Top, 'build/bin', ExecDir),
	path_concat(ExecDir, Cmd, Path0),
	file_exists(Path0),
	!,
	Path = Path0.

find_asset(Name, Path) :-
	( current_executable(ExecPath),
	  path_split(ExecPath, Dir, _)
	; fsR(bundle_src('Linearisation-2015')/src, Dir)
	),
	path_concat(Dir, Name, Path0),
    %display(looking_for_asset(Path0)), nl,
	file_exists(Path0),
	!,
	Path = Path0.

% ---------------------------------------------------------------------------

lineariseHornPE(ResultDir, File,OutFile,K) :-
    path_concat(ResultDir, 'linearSolveProg_k_perm.pl', Interpreter),
    find_asset('linearSolve_k_perm.pl.ann', Annotation),
    linearise(File, Interpreter, Annotation, K,  OutFile).


linearise(P, Interpreter, Annotation, Dim,  PLin):-
    stackSize(P, Dim, Size),
    F_Logen='lin.logen',
    linearisePE(P,  Interpreter, Annotation, Size, F_Logen),
    recoverOriginalPred(F_Logen,  PLin),
    process_call(path('rm'), [F_Logen],[]).


linearisePE(In, Interpreter, Annotation, StackSize, PLin):-
	atom_concat(In, '.logen', InLogen),
	chc2logen:main([In, InLogen]),
	atom_concat(Interpreter, '.ann', OutAnn),
	copy_file(InLogen, OutAnn, [overwrite]),
	copy_file(Annotation, OutAnn, [append]),
	% logen goal
	number_atom(StackSize, Goal),
	atom_concat(['go(', Goal, ')'], LogenGoal),
	% logen
	logen_executable(Logen),
	process_call(Logen, ['-np', Interpreter, LogenGoal], [stdout(file(PLin))]),
	process_call(path('rm'), [InLogen],[]),
	process_call(path('rm'), [OutAnn],[]).

% formula: Size=(max. nr of body atoms in the program -1)* program_dimension + 1

stackSize(F, Dimension, Size):-
    load_file(F),
    max_nr_of_body_atoms(Index),
    Size is (Index-1)*Dimension + 1.
    %print(Size), nl.

max_nr_of_body_atoms(Index):-
    findall(Nr, (my_clause(_,B,_), separate_constraints(B, _, Bs), length(Bs, Nr)), SizeList),
    max_member(SizeList, Index).