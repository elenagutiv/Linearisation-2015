
## **case-studies** ##

Given a set of *non-linear* Constrained Logic Programs namely P0,  and a positive integer *k*, we will build sets  P1 and P2. 

P1 is the set of CLPs generating the *at-most-k-dimensional* derivations w.r.t. set P0. P2 is the set of *linear* programs w.r.t P1. We will rely on transformation procedure implemented in `kdim.pl` to build P1 and on *ELP* to build P2.

Once P1 and P2 are built, we will run QARM'C (Model Checker for HC) on each program of both sets. For each program, we will annotate runtime and QARM'C output in JSON format.

#Contents#

Directory | Contents															|
---------------|--------------------------------------------------------------------|
P0			   | A set of non-linear CLPs.	 															|
scripts			   | <ul><li>`run-tests.py`- It builds sets P1 and P2, runs QARM'C and writes the results in a JSON file.</li> <li>`remove.sh`- It cleans the directory after each `run-tests.py`execution</li><li>`kdim.pl` Given *k*, it transforms a program from P0 into a program in P1 ( code provided by J.P. Gallagher)</li></ul> 	 															|
results			   | `run-tests.py` output in JSON format.

#How to run tests:#


> - It requires to include QARM'C (Abstraction Refinement Model Checker for Horn clauses, revision 123 or later)  in the same path as `run-tests.py`

Type in `case-studies/scripts/`:
`$ python run-tests.py`

JSON file will be generated in `results/`

To customize execution options, open and edit `run-tests.py`:


`# USER OPTIONS #

tests = glob(join('../P0', '<myfile.horn> ...')) # Specify set of programs in P0 to be tested
k="<d>" # Specify dimension value
extraoptions = "<qarmc extraoptions>" # Specify QARMC extra options
qarmc_timelimit = "<s>" # Specify QARMC time limit in seconds
elp_timelimit = "<s>" # Specify ELP time limit in seconds
JSONfile = '../results/<myJSONfile.json>' # Specify name of file`


To clean, type in `case-studies/scripts/`:

`$ ./remove.sh`

This will remove all log files in `PO/` and directories `P1/` and `P2/`