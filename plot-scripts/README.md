
## **plot-scripts** ##


#Contents#

Directory | Contents															|
---------------|--------------------------------------------------------------------|
`elp-scatterplot-P0-Pk.html`			   | A scatterplot comparing the time for P0 vs the best solving time for P1, P2 using QARMC. This tells the gain/cost of underapproximating. 															|
`elp-scatterplot-P1-P2.html`			   | A scatterplot comparing the time for P1 vs P2 using QARMC.	 															|
`scatterplot-template-1`		   | Mustache template to inline JSON data in `elp-scatterplot-P1-P2.html`
`scatterplot-template-2`		   | Mustache template to inline JSON data in `elp-scatterplot-P0-Pk.html`
`generate-scatterp.sh`		   | Script that runs Mustache using one of the templates above and produces an HTML file containing JSON data hard-coded. 
`running-times.yml`		   | Data used to plot both graphs in a format specified by Mustache. 

----------


> #Software Requirements:#
> - Mustache tool needs to be installed to run `generate-scatterp.sh`. For this end, visit: https://github.com/janl/mustache.js/

----------

To **generate** `elp-scatterplot-P1-P2.html`, run `generate-scatterp.sh` using the following parameter:

`$ ./generate-scatterp.sh -p 1`

This will run Mustache, taking template `scatter-template-1` and data file `running-times.yml` as input and will give an html file according to the template given and including the data inlined under the name of `elp-scatterplot-P1-P2.html`.

To **generate** `elp-scatterplot-P0-Pk.html`, run `generate-scatterp.sh` using the following parameter:

`$ ./generate-scatterp.sh -p 2`

The commands above will generate `elp-scatterplot-P1-P2.html` and `elp-scatterplot-P0-Pk.html` respectively, to be opened with your favourite browser.

> - Both `elp-scatterplot-P0-Pk.html` and `elp-scatterplot-P1-P2.html` can be opened in all major browsers (Firefox, Chrome, Safari,..)
