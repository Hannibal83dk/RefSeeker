


#' Gene expresssion stabilities by NormFinder
#'
#' @param expression A matrix, data frame or tibble with columns for each genes and rows for each samples.
#'
#' @return Data frame with stability values based on the Normfinder method
#' @export
#'
# #' @source "https://moma.dk/files/r.NormOldStab5.txt"
#' @examples
#' set.seed(100)
#' ct_vals <- matrix(rnorm(5*20, mean = 25), ncol = 5, nrow = 20)
#' dimnames(ct_vals)[[2]] <-  c("gene1", "gene2", "gene3", "gene4", "gene5")
#' \dontrun{
#'
#' nrmfndr <- rs_normfinder(ct_vals)
#' nrmfndr
#'
#' }

#'
#'########################
#'
#' set.seed(100)
#' ct_vals <- data.frame(matrix(rnorm(5*20, mean = 25), ncol = 5, nrow = 20))
#'
#' names(ct_vals) <- c("gene1", "gene2", "gene3", "gene4", "gene5")
#'
#' \dontrun{
#' nrmfndr <- rs_normfinder(ct_vals)
#' nrmfndr
#' }
#'
#'
#'
#' @references Andersen C.L., Ledet-Jensen J., Ørntoft T.: Normalization of real-time quantitative RT-PCR data: a model based variance estimation approach to identify genes suited for normalization - applied to bladder- and colon-cancer data-sets.
#' Cancer Research. 2004 (64): 5245-5250
#'
#'
#'
#'


rs_normfinder <- function(expression){

  # Look for the exdata folder check if r.NormOldStab5.txt exists in this folder, if not download it to that location

  if(!file.exists(
    paste0(dir(find.package("refSeeker", lib.loc=NULL, quiet = TRUE), pattern = "exdata", recursive = TRUE, full.names = TRUE, include.dirs = TRUE), "/r.NormOldStab5.txt")

    )){

    utils::download.file("https://moma.dk/files/r.NormOldStab5.txt",
                        paste0(dir(find.package("refSeeker", lib.loc=NULL, quiet = TRUE), pattern = "exdata", recursive = TRUE, full.names = TRUE, include.dirs = TRUE), "/r.NormOldStab5.txt"),
                        quiet = FALSE)

    rsadjustnmfRounding(decimals = 3)
  }
  # source the r.NormOldStab5.txt
  source(paste0(dir(find.package("refSeeker", lib.loc=NULL, quiet = TRUE), pattern = "exdata", recursive = TRUE, full.names = TRUE, include.dirs = TRUE), "/r.NormOldStab5.txt"))


  ###################################################################################################################

  # write the temporary txt file table for the normfinderfuncton
  utils::write.table(data.frame(t(expression)), "expression_temp.txt")

  # Calculate the stabilities and save in Results
  Results = Normfinder("expression_temp.txt", Groups = FALSE)

  # Remove the temporary txt file
  unlink("expression_temp.txt")


  # show the results of the analysis based on individual targets
  nrmfndr <- data.frame(Target = row.names(Results$Ordered), Stability = Results$Ordered$GroupSD)

  nrmfndr <- rsorderbystability(nrmfndr)

  nrmfndr <- rsaddstabilityrank(nrmfndr, 2)

  return(nrmfndr)

}




#' Adjust rounding in the normfinder algorithm to 3 decimals
#'
#' @param decimals An integer value indicating desired precision rounding of the Normfinder results
#' @return Void
#' @export
#'
#'
rsadjustnmfRounding <- function(decimals = 3) {

  decimals <- as.character(decimals)
  tx <- readLines(paste0(find.package("refSeeker", lib.loc = NULL, quiet = TRUE), "/exdata/r.NormOldStab5.txt"))

  substring(tx[193], 41, 41) <- decimals

  substring(tx[193], 71, 71) <- decimals

  substring(tx[194], 32, 32) <- decimals

  substring(tx[196], 38, 38) <- decimals

  substring(tx[196], 65, 65) <- decimals

  substring(tx[197], 28, 28) <- decimals

  substring(tx[198], 45, 45) <- decimals

  substring(tx[199], 48, 48) <- decimals

  substring(tx[203], 28, 28) <- decimals

  substring(tx[229], 40, 40) <- decimals

  substring(tx[232], 26, 26) <- decimals

  substring(tx[236], 40, 40) <- decimals

  substring(tx[196], 38, 38) <- decimals

  writeLines(tx, con = paste0(find.package("refSeeker", lib.loc = NULL, quiet = TRUE), "/exdata/r.NormOldStab5.txt"))

}



