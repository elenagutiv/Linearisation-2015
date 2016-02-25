
/*
the input is a set of Horn clauses P and a value of dimension k  and output is a set of linear Horn clauses obtained by linearising k-dim(P)

*/

:- module(linearise, _).


:- use_module(library(lists)).


:- use_module(lin_pe).
:- use_module(common).

:- include(get_options).

:- data(elp/0).



main(ArgV) :-
	setOptions(ArgV,File,OutFile,K),
    %mktempdir_in_tmp('linearisepe-XXXXXXXX', ResultDir),
    (elp ->
        %for elp
        true
    ;
        lineariseHornPE(File,OutFile,K)
    ).
    %rmtempdir(ResultDir).

recognised_option('-prg',  programO(R),[R]).
recognised_option('-k',    dimension(K),[K]).
recognised_option('-o',    outputFile(R),[R]).
recognised_option('-elp',  linproc,[]).


setOptions(ArgV,File,OutFile,K1) :-
	get_options(ArgV,Options,_),
	(member(programO(File),Options) -> true;
			write(user_output,'No input file given.'),nl(user_output),fail),
	(member(dimension(K),Options) -> convert2num(K,K1);
			write(user_output,'No dimension given.'),nl(user_output),fail),
    (member(linproc,Options) -> assert(elp); true),
	(member(outputFile(OutFile),Options) -> true;
				write(user_output,'No output file is given.'),nl(user_output),fail).

