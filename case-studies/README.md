This folder contains:

Directory/file | Contents															|
---------------|--------------------------------------------------------------------|
P0			   | It contains a set of non-linear CLPs.	 															|
scripts			   | It contains `run-tests.py`-a python script that (given a dimension value *k*) builds for each program in `P0/`, the at-most-k-dimension program (and locates it in `P1/`) and for each program in `P1/`, its linear version (and locates it in `P2/`). Finally, it runs Q'ARMC on each program in `P1/` and `P2/` and collects runtimes and outputs in JSON format. It also contains `remove.sh`- a script that cleans the directory after each test execution and `kdim.pl`- Prolog code that transforms (given k) a program from `P0/` into its at-most-k-dimension version (provided by J.P. Gallagher). 	 															|
results			   | It contains `run-tests.py` output in JSON format.		 															|


## How to run tests:

It requires Q'ARMC (Abstraction Refinement Model Checker for Horn clauses, revision 123 or later) located in the same path as `run-tests.py`.
