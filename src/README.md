## src ##

ELP Prolog implementation.

> - It requires to install SWI-Prolog (7.2.3 or later)

#Contents#

-**`main.pl`** Contains ELP implementation, including CLP (*Clause Linearisation Procedure*) and other methods directly related with ELP.

-**`clause.pl`** Contains clause manipulation methods.

# How to run ELP: #

Type:

`$ swipl main.pl`

In SWI-Prolog prompt, type:

`?- go('<program_to_be_linearised.horn>').`

To redirect output to an external file, type:

`?- go('<program_to_be_linearised.horn>','<output_file.txt>').`

