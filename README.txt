RefSeeker


DESCRIPTION

	RefSeeker is a package developed for the R statistical programming language. It provides a convenient
	way of analyzing candidate target expression stabilities from raw Cp/Ct/Cq values obtained normally
	from RT-qPCR experiments.

	It uses the RefFinder method(Xie et. al. 2012), which includes stability values calculated by four
	different algorithms: Normfinder(Andersen et. al. 2004), geNorm(Vandesompele et. al. 2002),
	BestKeeper(Pfaffl et. al. 2004) and the comparative delta-Ct method(Silver et. al. 2006). The target
	stabilities are then ranked and a geometric mean of the ranking from each algorithm is then calculated
	as the comprehensive ranking.

	The package include functions for easy import of raw data, calculatiion of stability values for each
	algorithm and an easy way of exporting these data to graphs and tabular file formats.


INSTALLATION

The package can be installed in two ways:

	1. Download Package Archive:

		a) Download the RefSeeker_latests.tar.gz file to your computer:
		https://github.com/Hannibal83dk/RefSeeker/releases/latest/download/RefSeeker_latest.tar.gz

		b) Open R

		c) In the R Console type:
		{ install.packages("<PATH/TO/RefSeeker_1.0.0.tar.gz>", repos = NULL, type = "source") }

		Note: Please note that the entire part of <PATH/TO/RefSeeker_latest.tar.gz> needs to be changed to the specific location of the downloaded
		file on your computer

		Alternatively, if RStudio is being used:
		open Tools -> Install Packages ->
		Select Package from Archive File in the Install from drop down	menu.
		Browse for the downloaded Package archive -> press Install


	2. Use devtools to download from github:

		a) Make sure the devtools package is installed in your R environment.
			In the R Console, devtools can be installed from CRAN using:
			{ install.packages("devtools") }

		b) In the R Console type:
		{ devtools::install_github("Hannibal83dk/RefSeeker", build_vignettes = TRUE) }


		After installation the library can be loaded by typing
		{ library(RefSeeker) }


USAGE

To use the package data needs to be prepared either in a supported file type: xlsx, ods, csv, tsv or txt, or as a data frame directly in R.
Multiple files of different file type can be loaded simultaneously. However, if the filebrowser is used the files need to be in the same folder.

	No matter the input source, the following requirements are the same:

	- Each column must be representing a gene/target and each row representing an individual sample.

	- Each column must be named.

	- Row names must be excluded

	- No missing data is allowed


For a quick analysis use { rs_wizard() }
	A dialog window will appear to let you select input file(s) and select parameters for graphical and tabular outputs.

	Note: Nothing will be returned to R at this point. (Future revisions will include saving the state of the selections and return of the data and results to the console.


Load data from file:

	1. Data can be loaded using the rs_loaddata function, a file selection dialog will appear if a file path is not provided.
	2. Browse to and select the files.

	{ data <- rs_loaddata() }

	Note: if data has been loaded or are available as a variable in R, this step is unnecessary.


Calculate stabilities:

	To calculate stabilities use the rs_reffinder function. This will calculate stabilities using all the available algorithms.

	{ results <- rs_reffinder(data) }

	Create a graph:

	To create a simple graph of the results use rs_graph. Several option exists for tweaking the graph including adding color changing size and orientation, see the package vignettes for more.

	{ rs_graph(results, filename = "DemoStabilities") }



Export tables

	Several types of tables can be exported here is a quick example, please see the Exporting Stability Tables vignette in the package.

	To save the the data in a cross compatible format:
	{ rs_exporttable(results, filename = "DemoStabilities", tabletype = "csv") }

	To create a docx formatted table for publication
	{ rs_exporttable(results, filename = "DemoStabilities", tabletype = "docx-combi") }


SUPPORT

	For bug reports or please use the github repository bug tracker
	For support regarding installation and use, or to report issues or bugs please open an issue through github: https://github.com/Hannibal83dk/RefSeeker/issues


LICENSE

	RefSeeker is made available under the GPL ≥ 2






