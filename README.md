## Linearisation-2015##

Linearisation of an index bounded set of Horn clauses.





#Contents#

Directory | Contents															|
---------------|--------------------------------------------------------------------|
case-studies			   | Set of tests, scripts and results. 	 															|
plot-scripts			   | It contains scatter-plot generation HTML files.	 															|
src		   | `source files`






## Programming 

The lineariser  is written in Ciao and  uses
Logen as a PE.

## Requirements
1. [Ciao](http://github.com/ciao-lang/ciao) 
(installed with `./ciao-boot.sh local-install`)

2. Partial evaluator [Logen](https://github.com/leuschel/logen)
(install the Ciao port with `ciao get github.com/jfmc/logen`).

## Build and installation

You can automatically fetch, build, and install `linearise` using:

```
ciao get https://github.com/elenagutiv/Linearisation-2015
```

This command stores the source and generates the binaries in the Ciao
_workspace directory_. This directory is given by the value of the
`CIAOPATH` environment variable (or `~/.ciao` if unspecified).

Binaries are placed in the `$CIAOPATH/build/bin` directory (or
`~/.ciao/build/bin`). To call `linearise` without specifying its
full path it is recommended to include this directory in your `PATH`:

```
export PATH=$CIAOPATH/build/bin:$PATH
or export PATH=~/.ciao/build/bin:$PATH
```

**For developing** `Linearisation-2015` it is recommended to define `CIAOPATH`
(E.g., `~/ciao`) and clone this repository in your workspace.

## Usage

**Usage**: `linearise` -prg <*input file containing a set of Horn clauses*\> -k <*dimension_for_linearisation*\> [-elp] -o <*output file containing a set of linear Horn clauses*\>

**Input**: `a set of (non)-linear Horn clauses` written using Prolog
notation: e.g. `h(X):- C, b1(X1),...,bn(Xn).`

**Output**: ` set of linear Horn clauses`.

## Generate a standalone binary distribution



1. `mkdir dist; cd dist`
2. `ciaoc_sdyn ../src/linearise`

This creates a platform specific binary `linearise` at `dist/`
directory, together with the collection of shared libraries for the
dependencies. **Note**: you may need to include a standalone copy of
`logen` binary and related files.





