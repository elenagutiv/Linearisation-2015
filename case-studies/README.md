
## **case-studies** ##

Given a set of *non-linear* Constrained Logic Programs namely P0,  and a positive integer *k*, we will build sets  P1 and P2. 

P1 is the set of CLPs generating the *at-most-k-dimensional* derivations w.r.t. set P0. P2 is the set of *linear* programs w.r.t P1. We will rely on transformation procedure implemented in `kdim.pl` to build P1 and on *ELP* to build P2.

Once P1 and P2 are built, we will run QARMC (Model Checker for HC) on each program of both sets. For each program, we will annotate runtime and QARMC output in JSON format.

#Contents#

Directory | Contents															|
---------------|--------------------------------------------------------------------|
P0			   | A set of non-linear CLPs.	 															|
scripts			   | <ul><li>`run-tests-to-YAML.py`- It builds sets P1 and P2, runs QARMC for P0, P1 and P2 and writes the results ( including QARMC answer and runnning times ) in 2 different formats: JSON format in  `results/running-times.json` ( directory `results/` is created during the execution ) and YAML format in `plot-scripts` folder.</li><li>`remove.sh`- It cleans the directory after each `run-tests-to-YAML.py` execution</li><li>`kdim.pl` Given *k*, it transforms a program from P0 into a program in P1 ( code provided by J.P. Gallagher)</li></ul>


----------


> - During `run-tests-to-JSON.py` directories `P1/`, `P2/` and `results/` are created containing sets of programs P1 and P2 and `run-tests-to-YAML.py` output file: `running-times.json`.

----------


> #Software Requirements:#

> - QARMC (Abstraction Refinement Model Checker for Horn clauses, revision 123 or later)  in the same path as `run-tests-to-JSON.py` and `run-tests-to-plot.py` files. The executable file name is assumed to be `qarmc-latest.osx` but it can be changed by editing the script (see second section below).
> - Pyhton 3.5.0 or greater to execute both python scripts.
> - SWI-Prolog Version 7.2.3 or greater.
>- To generate plots, Mustache will be needed (for further information see [README.md](https://github.com/elenagutiv/Linearisation-2015/blob/master/plot-scripts/README.md))


----------

To **run** tests, type in `case-studies/scripts/`:

`$ python3 run-tests-to-JSON.py`

JSON file will be generated in `results/`.

Alternatively, to get JSON data in a suitable format to build the **scatter plot** (NVD3):

`$ python3 run-tests-to-plot.py`

----------

To **customize** execution options, open and edit `run-tests-to-JSON.py` or `run-tests-to-plot.py`:


`# USER OPTIONS #`

`tests = glob(join('../P0', '<myfile.horn> ...')) # Specify set of programs in P0 to be tested`

`k="[<d1,..>]" # Specify dimension values for which running-times will be measured.`

`extraoptions = "<qarmc extraoptions>" # Specify QARMC extra options`

`qarmc_timelimit = "<s>" # Specify QARMC time limit in seconds`

`elp_timelimit = "<s>" # Specify ELP time limit in seconds`

`YAMLformatfile = '../../plot-scripts/<myYAMLfile>.yml' # Specify name of YAML file`
`JSONformatfile = '../results/<myJSONfile.json>'  # Specify name of JSON file`

----------


To **clean**, type in `case-studies/scripts/`:

`$ ./remove.sh`

This will remove all log files in `PO/` and directories `P1/` and `P2/`