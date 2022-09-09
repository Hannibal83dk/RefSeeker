


#' Load one or more datasets from an excel file
#'
#' @param filepath a path to an excel or ods file. If left blank, a dialog will appear to select this interactivly.
#' @return A list containing expression data for each sheet in the excel file
#'
#' @import readxl
#' @import readODS
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

  return(datalist)
}


#' @rdname rs_loadexceldata
#' @examples
#' \dontrun{
#' rs_loadodsdata()
#' }
#' @export
rs_loadodsdata <- rs_loadexceldata







