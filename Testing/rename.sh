#!/bin/sh

# Change the extension of a set of files.

OLD_EXTENSION=qarmc
NEW_EXTENSION=horn
for file in *.$OLD_EXTENSION
do
 	
	filename="${file%.*}"
	new="$filename".$NEW_EXTENSION
 	echo $new
 	mv "$file" $new
done