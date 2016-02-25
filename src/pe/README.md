# linearise\_HC\_PE
Linearisation using Partial Evaluation (PE)

## Programming 

The lineariser  is written in Ciao and  uses
Logen as a PE.

## Requirements
1. [Ciao](http://github.com/ciao-lang/ciao) with
   [Parma Polyhedra Library](http://bugseng.com/products/ppl/) support
   (installed with `./ciao-boot.sh local-install
   --contrib:with_ppl=yes --contrib:auto_install_ppl=yes`)

2. Partial evaluator [Logen](https://github.com/leuschel/logen)
   (install the Ciao port with `ciao get github.com/jfmc/logen`).

## Build and installation

You can automatically fetch, build, and install linearise\_HC\_PE using:

```
ciao get https://github.com/elenagutiv/Linearisation-2015
```

This command stores the source and generates the binaries in the Ciao
_workspace directory_. This directory is given by the value of the
`CIAOPATH` environment variable (or `~/.ciao` if unspecified).

Binaries are placed in the `$CIAOPATH/build/bin` directory (or
`~/.ciao/build/bin`). To call `lhornsolver` without specifying its
full path it is recommended to include this directory in your `PATH`:

```
export PATH=$CIAOPATH/build/bin:$PATH
 or export PATH=~/.ciao/build/bin:$PATH
```

**For developing** LHornSolver it is recommended to define `CIAOPATH`
(E.g., `~/ciao`) and clone this repository in your workspace.

## Usage

**Usage**: `linearise_HC_PE` -prg <*input file containing a set of Horn clauses*\> -k <*dimension_for_linearisation*\> -o <*output file containing a set of linear Horn clauses*\>

**Input**: `a set of (non)-linear Horn clauses` written using Prolog
notation: e.g. `h(X):- C, b1(X1),...,bn(Xn).`

**Output**: ` set of linear Horn clauses`.

## Generate a standalone binary distribution

```sh
mkdir dist; cd dist
ciaoc_sdyn ../src/linearise_HC_PE
```

1. `mkdir dist; cd dist`
2. `ciaoc_sdyn ../src/linearise_HC_PE`

This creates a platform specific binary `linearise_HC_PE` at `dist/`
directory, together with the collection of shared libraries for the
dependencies. **Note**: you may need to include a standalone copy of
`logen` binary and related files.

