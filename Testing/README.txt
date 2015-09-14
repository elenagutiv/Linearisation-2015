This folder contains:

-P0: Collection of constraint logic programs (set of Horn clauses with constrains).
-P1: Collection of at-most-k dimension programs (for a given k). It results from applying 'kdim' code to set P0.
-P2: Collection of linear programs. It results from applying 'elp' procedure to set P1.


-run-tests.sh: Script to automatizes the generation of P1 and P2 from P0 and the execution of qarmc.

-qarmc.osx: QARMC Tool.
-kdim.pl: Procedure that generates at-most-k dimension program from a given set of constrained horn clauses.

How to run run-tests.sh:

Usage: ./run-tests.sh -n 4 -k 3

It takes CLP program named 4.horn in P0 folder and generates 4.horn in P1 (at-most-3 dimension program) and 4.horn (linear program after applying elp to the previous file) in P2. Once these 3 files are built, it runs qarmc with each of the 3 files as argument and shows the result of the tests by console. 
