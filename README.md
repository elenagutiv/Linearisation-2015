## Linearisation Procedure for index-bounded sets of Horn clauses##

Given a *non-linear* Constrained Logic Program (CLP) and a positive integer *k*, we build a *linear* k-index bounded CLP. To build the linear version of each CLP, we have implemented 2 linearisers. <ul><li>**ELP** is a linearisation procedure that relies on the syntactic structure of index-bounded CLPs to perform a set of transformations such as folding/unfolding, introduction of new definition clauses and removal of useless clauses (for further information, read: [ELP](https://drive.google.com/open?id=0B9cK-R3AAviCN0VoVldyR2VmWWc)).</li> <li>**Linearisation Procedure based on Partial Evaluation** (PE-based procedure)</li></ul>

----------


> - *A k-index bounded* CLP is a program containing Horn clauses that generate derivation trees of bounded dimension. Thus, given a non-linear program and a k index value, we return a linear program which is an under-approximation of the original, in the sense that, some sets of program traces have been possibly eliminated (specifically, those of dimension greater than k). The result is a linear program that generates derivation trees of dimension not greater or equal to k.

----------

#Contents#

Directory | Contents															|
---------------|--------------------------------------------------------------------|
benchmarks			   | Set of tests, scripts and results. 	 															|
plot-scripts			   | It contains scatter-plot generation HTML files.	 															|
src		   | `source files`



## Programming 

The linearisers are written in Ciao. PE-based procedure uses Logen as Partial Evaluator.

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

**Usage**: `linearise [key value*]+` 

**key list**:

`-prg` : value that follows it denotes an input program  

`-k`   : value that follows it denotes a dimension 

`-o`   : value that follows it denotes  an output file

`-pe` : uses a linearisation procedure based on Partial Evaluation (is not followed by any value and is optional, the default procedure is elp)



**Example**:
 
`linearise` -prg  `example.pl` -k 2  -o `example2lin.pl`

For a program `example.pl`, `example2lin.pl` represents a linearised version of 2-dim `example.pl`.

**Input**: `a set of (non)-linear Horn clauses` written using Prolog
notation: e.g. `h(X):- C, b1(X1),...,bn(Xn)` and `a value of dimension` 

**Output**: ` set of linear Horn clauses`.

## Generate a standalone binary distribution



1. `mkdir dist; cd dist`
2. `ciaoc_sdyn ../src/linearise`

This creates a platform specific binary `linearise` at `dist/`
directory, together with the collection of shared libraries for the
dependencies. **Note**: you may need to include a standalone copy of
`logen` binary and related files.





