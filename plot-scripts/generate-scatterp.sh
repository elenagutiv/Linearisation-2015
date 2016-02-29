#!/bin/sh

if [ $# != 2 ] || [ $1 != "-p" ] || ([ $2 != 1 ] && [ $2 != 2 ] && [ $2 != 3 ]); then
	echo "Usage: ./generate-scatterplot.sh -p <plot_id> where"
	echo "<plot-id> = 1 to generate elp-scatterplot-P1-P2"
	echo "<plot-id> = 2 to generate elp-scatterplot-P0-PK"
	echo "<plot-id> = 3 to generate elp-scatterplot-ELP-PE"
	echo ""
	echo "This script requires mustache to be installed"
	exit
fi

if [ $2 -eq 1 ]; then 
	TEMPLATE="scatterplot-template-1.html"
	PLOT="elp-scatterplot-P1-P2.html"
elif [ $2 -eq 2 ]; then 
	TEMPLATE="scatterplot-template-2.html"
	PLOT="elp-scatterplot-P0-PK.html"
else
	TEMPLATE="scatterplot-template-3.html"
	PLOT="elp-scatterplot-ELP-PE.html"
fi

VIEW="running-times.yml"

# Run mustache:

echo "Running mustache..."
mustache $VIEW $TEMPLATE > $PLOT

echo "HTML file: $PLOT"
