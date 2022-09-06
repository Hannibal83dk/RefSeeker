#' Calculates stabilites based on the bestKeeper method
#'
#' @param expression A matrix, data frame or tibble with columns for each genes and rows for each samples.
#'
#' @return A data frame with stability values - std dev +/- CP
#' @export
# #' @import ctrlGene
#'
#' @examples
#' set.seed(100)
#' ct_vals <- data.frame(matrix(rnorm(5*20, mean = 25), ncol = 5, nrow = 20))
#'
#' names(ct_vals) <- c("gene1", "gene2", "gene3", "gene4", "gene5")
#' bks <- rs_bestKeeper(ct_vals)
#' bks
#'
#' @references Pfaffl MW, Tichopad A, Prgomet C, Neuvians TP. 2004. Determination of stable housekeeping genes, differentially regulated target genes and sample integrity: BestKeeper--Excel-based tool using pair-wise correlations. Biotechnology letters 26:509-515.
#'
rs_bestKeeper <- function(expression){

  # Make sure the input is in the form of a data frame
  input <- as.data.frame(expression)

  # Get the gene names from the column names
  genes <- names(input)
  # Create a new data frame for the stability values
  bk <- data.frame(matrix(nrow = length(genes), ncol = 3))

  # Set names of the three columns
  names(bk) <- c("Target", "MAD", "Stability Rank")

  # Write targets to bestkeeper table
  bk$Target <- genes

  # Calculate the mean absolute deviation
  for (i in 1:ncol(input)) {
    # Go through each input data column and save results in bestkeeper table
    bk[i,2] <- mean(abs(input[,i]-mean(input[,i])))
  }

  bk <- rsorderbystability(bk)


  # Write the ranking column
  bk <- rsaddstabilityrank(bk, 2)

  return(bk)

}

