

#' Gene expression stability by geNorm
#'
#'
#'
#' @param expression A matrix, data frame or tibble with columns for each genes and rows for each samples.
#' @param decimals An integer value indicating result precision. Used for the rounding of results.
#' @return Data frame with average pairwise variation for each gene
#' @export
#'
#' @examples
#' set.seed(100)
#' ct_vals <- matrix(rnorm(5*20), ncol = 5, nrow = 20)
#' dimnames(ct_vals)[[2]] <-  c("gene1", "gene2", "gene3", "gene4", "gene5")
#'
#' gnm <- rs_genorm(ct_vals)
#' gnm
#'
#'########################
#'
#' set.seed(100)
#' ct_vals <- data.frame(matrix(rnorm(5*20, mean = 25), ncol = 5, nrow = 20))
#'
#' names(ct_vals) <- c("gene1", "gene2", "gene3", "gene4", "gene5")
#' gnm <- rs_genorm(ct_vals)
#' gnm
#'
#'
#' @references Vandesompele J, De Preter K, Pattyn F, Poppe B, Van Roy N, De Paepe A, Speleman F. 2002. Accurate normalization of real-time quantitative RT-PCR data by geometric averaging of multiple internal control genes. Genome biology 3:RESEARCH0034.
#'
rs_genorm <- function(expression, decimals = 3){


  #gnex <- as.data.frame(expression)
  gnrm <- ctrlGene::geNorm2(expression)
  # gnrm$Avg.M <- round(gnrm$Avg.M, 3)


  gnrm$Avg.M[nrow(gnrm)] <- gnrm$Avg.M[nrow(gnrm)-1]

  names(gnrm)[1] <- "Target"

  #gnrm <- rsorderbystability(gnrm)

  gnrm$Avg.M <- round(gnrm$Avg.M, digits = decimals)

  gnrm <- rsaddstabilityrank(gnrm, 2)



  return(gnrm)

}







