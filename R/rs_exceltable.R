#'
#'
#' Output excel workbook containing stability and ranking as separate sheets
#'
#' @param refseekerlist A RefSeekerlist created by the rs_reffinder function
#'
#' @param filename A file name to identify the data
#' @param addDate Possibility to automatically add date to output file name
#'
#' @return Nothing, output an excel file in the working directory or an output folder selected by the file name
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
rs_exceltable <- function(refseekerlist, filename = "Stability-table", addDate = TRUE){


  if(!is.null(refseekerlist$stabilityTable)){ # Simple one dimensional list, just one data set (refseekerlist$stabilityTable exists)

    rsStability <- refseekerlist$stabilityTable
    rsRank <- refseekerlist$rankTable

    if(addDate == TRUE){
      filename <- paste0(filename, "_", Sys.Date())
    }

    rsexcelfile(filename, rsStability, rsRank, overwrite = TRUE)
  }


  if(!is.null(refseekerlist[[1]]$stabilityTable)){ # A list of lists of results tables (refseekerlist[[1]]$stabilityTable exists)

    for (i in 1:length(refseekerlist)) {

      rsStability <- refseekerlist[[i]]$stabilityTable
      rsRank <- refseekerlist[[i]]$rankTable

      newfilename <- paste0(filename, "_", names(refseekerlist[i]))

      if(addDate == TRUE){
        newfilename <- paste0(newfilename, "_", Sys.Date())
      }

      rsexcelfile(newfilename, rsStability, rsRank, overwrite = TRUE)
    }

  }

}
