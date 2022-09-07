


#' Load one or more datasets from an excel file
#'
#' @param filepath a path to output the data files
#' @return A list containing expression data for each sheet in the excel file
#'
#' @export
#'
#' @import readxl
#'
#'
#'
#' @examples
#'
#' \dontrun{
#' rs_loadexceldata()
#' }
#'
#'
rs_loadexceldata <- function(filepath = ""){

  datalist <- list()

  if(filepath == ""){
    filepath <- file.choose()
  }
  sheets <- readxl::excel_sheets(filepath)

  for (i in 1:length(sheets)){

    datalist[[i]] <- readxl::read_excel(filepath, i)
  }

  names(datalist) <- sub(" ", "_", sheets)
  return(datalist)
}
