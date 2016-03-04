
## **benchmarks** ##

Given a *non-linear* Constrained Logic Program (CLP) and a positive integer *k*, we build a *linear* k-index bounded CLP. To build the linear version of each CLP, we have implemented 2 linearisers. Firstly, ELP is a linearisation procedure that relies on the syntactic structure of index-bounded CLPs to perform a set of transformations such as folding/unfolding, introduction of new definition clauses and removal of useless clauses (for further information, read: [ELP]()). The second one is a procedure based on Partial Evaluation.

----------


> - *A k-index bounded* CLP is a program containing Horn clauses that generate derivation trees of bounded dimension. Thus, given a non-linear program and a k index value, we give as a result a linear program which is an under-approximation of the original, in the sense that, some sets of program traces have been posibly eliminated and the result is a linear program which generates derivation trees of dimension not greater than k.

----------


#Contents#

Directory | Contents															|
---------------|--------------------------------------------------------------------|
programs			   | A set of non-linear CLPs.	 															|
scripts			   | <ul><li>`run-tests-ELP-PE.py`- For each program in ´programs/´ and for each value of k specified in the script, it builds the corresponding linear index-bounded CLP using both linearisation procedures: ELP and PE-based procedure (each linear program is located in a new directory ´linear-programs/´). Then, it runs QARMC for each linear program and measures the runtime needed to solve it. All the results are saved in 2 different formats: JSON format in  `results/running-times.json` ( directory `results/` is created during the execution ) and YAML format in `plot-scripts` folder. These results can be visualized in a plot (see [README.md](https://github.com/elenagutiv/Linearisation-2015/blob/master/plot-scripts/README.md))</li><li>`remove.sh`- It cleans the directory after each `run-tests-to-ELP-PE.py` execution</li></ul>

----------


> #Software Requirements:#

> - **QARMC** revision 123 or later. It needs to be located in the same path as `run-tests-ELP-PE.py`. The executable file name is assumed to be `qarmc-latest.osx` but it can be changed by editing the script (see second section below).
> - **Pyhton 3.5.0** or greater to execute both python scripts.
> - **Ciao (see [README.md](https://github.com/elenagutiv/Linearisation-2015/blob/master/README.md)).
> - **Mustache** (for further information see [README.md](https://github.com/elenagutiv/Linearisation-2015/blob/master/plot-scripts/README.md))
> - **GNU coreutils package**. This provides `gtimeout` command, called in `run-tests-to-YAML.py`.
> - This project was tested under **OS X 10.9.X** and **10.10.X** ( other platforms were untested ).


----------

To **run** tests, type in `case-studies/scripts/`:

`$ python3 run-tests-ELP-PE.py`

Two files will be generated with esentially the same information but different formats. A JSON file in `/case-studies/results/` and a YAML file in '/plot-scripts'.

----------

To **customize** execution options, open and edit `run-tests-to-YAML.py`:


`# USER OPTIONS #`

`tests = glob(join('../programs', '<myfile.horn> ...')) # Specify set of programs in P0 to be tested`

`k="[<k1,..>]" # Specify index values for which running-times will be measured`

`qarmc_filename = "./qarmc-latest.osx" # Specify QARCM executable name`

`extraoptions = "<qarmc extraoptions>" # Specify QARMC extra options`

`qarmc_timelimit = "<s>" # Specify QARMC time limit in seconds`

`elp_timelimit = "<s>" # Specify ELP time limit in seconds`

`YAMLformatfile = '../../plot-scripts/<myYAMLfile>.yml' # Specify name of YAML file`

`JSONformatfile = '../results/<myJSONfile.json>'  # Specify name of JSON file`

----------


To **clean**, type in `case-studies/scripts/`:

`$ ./remove.sh`

This will remove all log files in `programs/` and the directory `linear-programs/`