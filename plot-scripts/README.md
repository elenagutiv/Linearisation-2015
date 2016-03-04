
## **plot-scripts** ##


#Contents#

Directory | Contents															|
---------------|--------------------------------------------------------------------|
`scatterplot-ELP-PE.html`			   | A scatterplot comparing the time for ELP-linear program vs PE/linear program using QARMC.	 															|
`scatterplot-template-1`		   | Mustache template to inline JSON data in `scatterplot-ELP-PE.html`
`generate-scatterp.sh`		   | Script that runs Mustache using one of the templates above and produces an HTML file containing JSON data hard-coded. 
`running-times.yml`		   | Data used to plot both graphs in a format specified by Mustache. 

----------


> #Software Requirements:#
> - Mustache tool needs to be installed to run `generate-scatterp.sh`. For this end, visit: https://github.com/janl/mustache.js/

----------

To **generate** `scatterplot-PE-ELP.html`, run `generate-scatterp.sh` using the following parameter:

`$ ./generate-scatterp.sh -p 1`

This will run Mustache, taking template `scatterplot-template-1` and data file `running-times.yml` as input and will give an html file according to the template given and including the data inlined under the name of `scatterplot-ELP-PE.html`.

This command above will generate `scatterplot-ELP-PE.html`, to be opened with your favourite browser.

> - `scatterplot-ELP-PE.html` can be opened in all major browsers (Firefox, Chrome, Safari,..)
