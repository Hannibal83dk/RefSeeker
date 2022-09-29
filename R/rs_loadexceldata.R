


#' Load one or more datasets from an excel file
#'
#' @param filepath a path to an excel or ods file. If left blank, a dialog will appear to select this interactivly.
#' @return A list containing expression data for each sheet in the excel file
#'
#' @import readxl
#' @import readODS
#'
#'
#' @export
#'
#' @aliases rs_loadodsdata()
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

  ext <- tolower(tools::file_ext(filepath))

# For excel files
  if(substring(ext, 1,2) == "xl"){

      sheets <- readxl::excel_sheets(filepath)

    for (i in 1:length(sheets)){

      datalist[[i]] <- readxl::read_excel(filepath, i)

    }

    names(datalist) <- sub(" ", "_", sheets)
  }

# For ods
  if(ext == "ods"){

    sheets <- readODS::list_ods_sheets(filepath)

    for (i in 1:length(sheets)){

      datalist[[i]] <- readODS::read_ods(filepath, i)

    }

    names(datalist) <- sub(" ", "_", sheets)
  }


  rsdatatest(datalist)


  #cat("\nPlease note: These tests are not a guarantee for (im)prober structure of dataset(s)")

  return(datalist)
}


#' @rdname rs_loadexceldata
#' @examples
#' \dontrun{
#' rs_loadodsdata()
#' }
#' @export
rs_loadodsdata <- rs_loadexceldata




#' Tests imported data for correct format
#'
#' @param expression Imported list of expression data sets
#'
#' @return void outputs warnings if discrepancies are found
#'
#' @note This is a simple test for missing data and character columns, correctness of data format is not guarantied if no warnings are given
#'
# #' @examples
rsdatatest <- function(expression){

  # Testing for missing values
  for (i in 1:length(expression)) {
    if(FALSE %in% stats::complete.cases(expression[[i]])){
      warning(paste("Caution: Missing data located in:", names(expression[i]), "\nMissing data is discauraged and may lead to errors"))
    }
  }

  # testing for non numeric columns. Making sure data is not transposed
  for (i in 1:length(expression)) {
    for (j in 1:ncol(expression[[i]])) {
      if(!is.numeric(expression[[i]][[j]])){

        warning(paste("Non numeric columns found in:", names(expression[i]),"\nPlease make sure columns represent target RNA species and rows represent samples"))
        break()
      }
    }
  }
}

