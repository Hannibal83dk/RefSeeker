% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rs_alorithms.R
\name{rs_reffinder}
\alias{rs_reffinder}
\title{Calculate reffinder stability tables from data frame, tibble or matrix or a list of these}
\usage{
rs_reffinder(expression)
}
\arguments{
\item{expression}{A data frame, tibble or matrix or a list of these}
}
\value{
A list of two data frame tables containing ranked stabilities and stability values from Normfinder, geNorm, BestKeeper and the reffinders comprehensive ranking.
if a list is passed a list of lists containing the above data for each data set
}
\description{
Calculate reffinder stability tables from data frame, tibble or matrix or a list of these
}
\examples{


set.seed(100)
ct_vals <- matrix(rnorm(5*20, mean = 25), ncol = 5, nrow = 20)
dimnames(ct_vals)[[2]] <-  c("gene1", "gene2", "gene3", "gene4", "gene5")

\dontrun{

rffndr <- rs_reffinder(ct_vals)
rffndr

}
}
