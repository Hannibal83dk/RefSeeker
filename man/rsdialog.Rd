% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rsdialogs.R
\name{rsdialog}
\alias{rsdialog}
\title{Dialog window to select a folder for output files}
\usage{
rsdialog(
  outdir = "",
  inputfile = c("No selection"),
  filename = "",
  graphtype = "multi",
  ordering = "Comprehensive Rank",
  imagetype = "png",
  orientation = "horizontal",
  tabletype = "xlsx"
)
}
\arguments{
\item{outdir}{A path to an folder to output graphs and tables}

\item{inputfile}{One or more paths to files containing expression data}

\item{filename}{A Prefix to use for file names.}

\item{graphtype}{A Type of graph, may be "individual" for one graph per data set, or "multi" for a combined graph of all data sets.}

\item{ordering}{Used to control sorting of the x axis, use "Target","delta-Ct","BestKeeper","Normfinder","geNorm" or "Comprehensive Rank".}

\item{imagetype}{A string determining which image file type the graph should be saved to. can be one of; "png", "tiff", "jpeg" or "svg".}

\item{orientation}{Selection of the orientation of the bars in the graph. May be "horizontal" or "vertical". Actually anything other than "horizontal" will be interpreted as vertical.}

\item{tabletype}{Select type of output may be one of ; "xlsx", "ods, csv", "tsv", "txt",  "docx-stability",  "docx-rank" or  "docx-combi"}
}
\value{
A list of string representing answer given in the dialog
1) Was the ok button pressed? - TRUE/FALSE
2) Output directory selected
3) Graph type selected - individual/multi
4) Ordering of targets on graph x-axis - reffinder/genorm/normfinder/bestkeeper/deltact/targets
4) File type selected for graph output - png/tiff/jpeg/none
5) File type for data output - xlsx/ods/csv/tsv/txt
6) One or more input data file paths
}
\description{
Dialog window to select a folder for output files
}
\examples{

\dontrun{
dialog.answers <- rsdialog()
}


}
