## **scripts** ##

##Contents

Directory | Contents															|
---------------|--------------------------------------------------------------------|
`remove.sh`			   | Cleans `benchmarks/` directory after the execution of `run-tests.py`.	 															|
`run-tests.py`			   | For each program in `benchmarks/programs/` and for each value of k specified in the script, it builds the corresponding linear index-bounded CLP using both linearisation procedures: ELP and PE-based procedure (each linear program is located in a new directory `benchmarks/linear-programs/`). Then, it runs QARMC for each linear program and measures the runtime needed to solve it. All the results are saved in 2 different formats: JSON format in  `results/running-times.json` and YAML format in `plot-scripts` folder. These results can be visualized in a plot (see [README.md](https://github.com/elenagutiv/Linearisation-2015/blob/master/plot-scripts/README.md)).

##Requirements:

1. **QARMC** revision 123 or later. It needs to be located in the same path as `run-tests.py`. The executable file name is assumed to be `qarmc.osx` but it can be changed by editing the script (see second section below).
2. **Pyhton 3.5.0** or greater to execute both python scripts.
3. **Ciao** (see [README.md](https://github.com/elenagutiv/Linearisation-2015/blob/master/README.md)).
4. **Mustache** (for further information see [README.md](https://github.com/elenagutiv/Linearisation-2015/blob/master/plot-scripts/README.md))
5.  **GNU coreutils package**. This provides `gtimeout` command, called in `run-tests-to-YAML.py`.
> - This project was tested under **OS X 10.9.X** and **10.10.X** ( other platforms were untested ).


----------

To **run** tests, type in `scripts/`:

`$ python3 run-tests.py`

Two files will be generated with esentially the same information but different formats. A JSON file in `results/` and a YAML file in 'plot-scripts/'.

----------

To **customize** execution options, open and edit `run-tests.py`:


`# USER OPTIONS #`

`tests = glob(join('../benchmarks/programs', '<myfile.horn> ...')) # Specify set of programs in programs/ to be tested`

`k="[<k1,..>]" # Specify index values for which running-times will be measured`

`qarmc_filename = "./qarmc.osx" # Specify QARCM executable name`

`extraoptions = "<qarmc extraoptions>" # Specify QARMC extra options`

`qarmc_timelimit = "<s>" # Specify QARMC time limit in seconds`

`elp_timelimit = "<s>" # Specify ELP time limit in seconds`

`YAMLformatfile = '../plot-scripts/<myYAMLfile>.yml' # Specify name of YAML file`

`JSONformatfile = '../results/<myJSONfile.json>'  # Specify name of JSON file`

----------


To **clean**, type in `case-studies/scripts/`:

`$ ./remove.sh`

This will remove all log files in `benchmarks/programs/` and the directory `benchmarks/linear-programs/`