
/*
the input is a set of Horn clauses P and a value of dimension k  and output is a set of linear Horn clauses obtained by linearising k-dim(P)

*/

:- module(linearise, _).


:- use_module(library(lists)).
:- use_module(library(system_extra), [mktempdir_in_tmp/2, rmtempdir/1,mkpath/1]).


:- use_module(lin_pe).
:- use_module(lin_elp).
:- use_module(common).

:- include(get_options).

:- data(pe/0).


main(ArgV) :-
    retractall(pe),
	setOptions(ArgV,File,OutFile,K),
    mktempdir_in_tmp('linearisepe-XXXXXXXX', ResultDir),
    (pe ->
        %for pe
        lineariseHornPE(ResultDir, File,OutFile,K)
    ;
        lineariseHornELP(ResultDir, File, OutFile, K)
    ),
    rmtempdir(ResultDir).

recognised_option('-prg',  programO(R),[R]).
recognised_option('-k',    dimension(K),[K]).
recognised_option('-o',    outputFile(R),[R]).
recognised_option('-pe',  linproc,[]).


setOptions(ArgV,File,OutFile,K1) :-
	get_options(ArgV,Options,_),
	(member(programO(File),Options) -> true;
			write(user_output,'No input file given.'),nl(user_output),fail),
	(member(dimension(K),Options) -> convert2num(K,K1);
			write(user_output,'No dimension given.'),nl(user_output),fail),
    (member(linproc,Options) -> assert(pe); true),
	(member(outputFile(OutFile),Options) -> true;
				write(user_output,'No output file is given.'),nl(user_output),fail).

