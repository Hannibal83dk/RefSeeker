% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rs_graph.R
\name{rs_graph}
\alias{rs_graph}
\title{Wrapper for selecting single or multifaceted graph output.}
\usage{
rs_graph(
  refseekerlist,
  filename = "",
  filetype = "png",
  forceSingle = FALSE,
  width,
  height,
  units = "px",
  res = 250,
  ordering = "Comprehensive Rank",
  colors = "",
  orientation = "horizontal"
)
}
\arguments{
\item{refseekerlist}{A refseekerlist list or a list of refseekerlist lists obtained frpm rs_reffinder() function}

\item{filename}{String. A name of the output file. Can contain an optional relative path.}

\item{filetype}{Selection of image file type may be "png", "tiff", "jpeg" or "svg".}

\item{forceSingle}{If TRUE will make a single graph for each data set provided.}

\item{width}{Integer value parameter passed to the PNG graphics device. Width of the output png. True width is dependent on the selected units-type.}

\item{height}{Integer value parameter passed to the PNG graphics device. Height of the output png. True Height is dependent on the selected units-type}

\item{units}{Integer value parameter passed to the PNG graphics device. The units in which height and width are given. Can be "px" (pixels, the default), "in" (inches), "cm" or "mm".}

\item{res}{Integer value parameter passed to the PNG graphics device. The nominal resolution in ppi which will be recorded in the bitmap file, if a positive integer. Also used for units other than the default, and to convert points to pixels.}

\item{ordering}{Used to control sorting of the x axis, use "Target","delta-Ct","BestKeeper","Normfinder","geNorm" or "Comprehensive Rank".}

\item{colors}{A data frame mapping target names to a hex color with targets in column 1 and colors in columns 2
target1       "#2271b2"
target2       "#d55e00"}

\item{orientation}{Selection of the orientation of the bars in the graph. May be "horizontal" or "vertical". Actually anything other than "horizontal" will be interpreted as vertical.}
}
\value{
Creates a plot shown in the active device, usually the plots pane. Optionally outputs the plot to a file if filename has been given.
}
\description{
Wrapper for selecting single or multifaceted graph output.
}
\examples{
set.seed(100)
ct_vals <- matrix(rnorm(5*20, mean = 25), ncol = 5, nrow = 20)
dimnames(ct_vals)[[2]] <-  c("gene1", "gene2", "gene3", "gene4", "gene5")
\dontrun{
rffndr <- rs_reffinder(ct_vals)

rs_graph(rffndr)
}


###
data(tble2)
\dontrun{
rs_graph(rs_reffinder(tble2))
}

}
