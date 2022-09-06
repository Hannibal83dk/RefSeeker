


#' Calculates expression stabilities by the Delta-Ct method
#'
#' @param expression A matrix, data frame or tibble with columns for each genes and rows for each samples.
#'
#' @return Data frame with mean standard deviations for pairwise delta Ct values
#' @export
#'
#' @examples
#' set.seed(100)
#' ct_vals <- matrix(rnorm(5*20, mean = 25), ncol = 5, nrow = 20)
#' dimnames(ct_vals)[[2]] <-  c("gene1", "gene2", "gene3", "gene4", "gene5")
#'
#' dct <- rs_deltaCt(ct_vals)
#' dct
#'
#'########################
#'
#' set.seed(100)
#' ct_vals <- data.frame(matrix(rnorm(5*20, mean = 25), ncol = 5, nrow = 20))
#'
#' names(ct_vals) <- c("gene1", "gene2", "gene3", "gene4", "gene5")
#' dct <- rs_deltaCt(ct_vals)
#' dct
#'
#' @references Silver N, Best S, Jiang J, Thein SL. 2006. Selection of housekeeping genes for gene expression studies in human reticulocytes using real-time PCR. BMC molecular biology 7:33.
#'
#'
rs_deltaCt <- function(expression){

  # Convert to matrix
  expression <- as.matrix(expression)

  # Get the gene names for later use
  genes <- dimnames(expression)[[2]]

  # Create a square data frame to hold all combinations of standard deviations.
  sddf <- data.frame(matrix(nrow = ncol(expression), ncol = ncol(expression)))
  row.names(sddf) <- genes
  names(sddf) <- genes

  # Calculate standard deviations of all combinations of deltaCt and save them in the data frame
  for (i in 1:(ncol(expression)-1)){
    for (j in (i+1):ncol(expression)){
      sddf[j,i] = stats::sd( expression[,i] - expression[,j] )
    }
  }

  # Create a new data frame for the mean standard deviations for each gene
  DC_tbl <- data.frame(matrix(ncol = 3, nrow = ncol(expression)))
  names(DC_tbl) <- c("Target", "Average of STDEV", "Stability Rank")
  DC_tbl$Target <- genes

  # Calculate the mean standard deviations for each gene, save it in the data frame
  for (i in 1:nrow(sddf)){
    # mean is calculated by combining row and column for each gene in a vector.
    DC_tbl[i,2] <- round(mean( unlist(  c(sddf[i, ] , sddf[, i] )   ), na.rm = TRUE), 3)

  }

  DC_tbl <- rsaddstabilityrank(DC_tbl, 2)
  # DC_tbl <- rsorderbystability(DC_tbl)
  #
  # DC_tbl$`Stability Rank` <- order(DC_tbl$`Average of STDEV`)


  return(DC_tbl)

}

