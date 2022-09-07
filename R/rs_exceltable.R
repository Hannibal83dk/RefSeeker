#'
#'
#' Output excel workbook containing stability and ranking as separate sheets
#'
#' @param reffinderlist A reffinderlist created by the reffinder function
#'
#' @param filename A file name to identify the data
#' @param addDate Possibility to automatically add date to output file name
#'
#' @return Nothing, outpust an excel file in the working directory or an output folder selected by the file name
#' @export
#'
#' @examples
#' set.seed(100)
#' ct_vals <- matrix(rnorm(5*20, mean = 25), ncol = 5, nrow = 20)
#' dimnames(ct_vals)[[2]] <-  c("gene1", "gene2", "gene3", "gene4", "gene5")
#'
#' \dontrun{
#' rffndr <- rs_reffinder(ct_vals)
#'
#' rs_exceltable(rffndr)
#' }
#'
#'
rs_exceltable <- function(reffinderlist, filename = "Stability-table", addDate = TRUE){

  rfStability <- reffinderlist$stabilityTable
  rfRank <- reffinderlist$rankTable

  if(addDate == TRUE){
    filename <- paste(filename, Sys.Date(), sep = "")
  }

  rsexcelfile(filename, rfStability, rfRank, overwrite = TRUE)

}

