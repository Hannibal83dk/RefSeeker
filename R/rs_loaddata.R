

#' Load data from a supported source
#'
#' @param files A character value or vector containing path(s) for files to be loaded into R. If left empty a dialog will allow for selection of files
#'
#' @return a list of data frames contain the selected data sets
#' @export
#'
#' @import tcltk
#'
#' @examples
#' \dontrun{
#' rs_loaddata()
#' }
rs_loaddata <- function(files = ""){

  filter <- c("{{Supported} {.txt}}",
              "{{Supported} {.csv}}",
              "{{Supported} {.tsv}}",
              "{{Supported} {.xlsx}}",
              "{{Supported} {.xls}}",
              "{{Supported} {.ods}}",
              "{{All files} {*}}")

  if(files[1] == ""){
    files <- rsselectfiles()
  }

  if(files[1] != ""){

  datalist <- list()

  for (i in 1:length(files)) {

    filext <- sub('.*\\.', '', files[i])

    if(substring( filext, 1,3) ==  "xls" | filext == "ods"){
      datalist <- c(datalist, rsloadspreadsheet(files[i]))
    } else {
      datalist <- c(datalist, rsloadtable(files[i]))
    }

  }


  # Renaming duplicated names in the data list.
  if(any(duplicated(names(datalist)))){

    names(datalist) <- make.unique(names(datalist), ".")
    warning("Some datasets have douplicated names. Suffix has been added to douplicates")

  }

  return(datalist)

  }


}


#######################################################



#' Load one or more datasets from an excel file
#'
#' @param filepath a path to an excel or ods file. If left blank, a dialog will appear to select this interactively.
#' @return A list containing expression data for each sheet in the excel file
#'
#' @import readxl
#' @import readODS
#'
#'
#'
#' @aliases rsloadodsdata()
#'
#' @examples
#'
#' \dontrun{
#' rsloadexceldata()
#' }
#'
#'
rsloadspreadsheet <- function(filepath = ""){

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


#######################################################
# #' @rdname rsloadexceldata
# #' @examples
# #' \dontrun{
# #' rsloadodsdata()
# #' }
# rsloadodsdata <- rsloadexceldata


#######################################################

#' Load data from text based data files.
#'
#' Read in expression data from text based files (csv, tsv, txt, etc.) with auto delimiter detection.
#' Please note that though the most common uses of the file types has been tested, some delimiter and file type combination may be incompatible.
#'
#' @param files An optional file path or a vector of file paths one or more data files.
#' If no files are given a dialog will allow selection of files
# #' @param batch The function will search the folder of the provided file path to find all files with same extension to load.
#'
#' @return A list or list of lists of input data tables
#'
#' @examples
#'
#' \dontrun{
#'
#' rsloadtable()
#'
#' rsloadtable("filepath")
#'
#' rsloadtable(c("fileone", "filetwo"))
#'
#' }
#'
#'
rsloadtable <- function(files = ""){


  # filepath = "./inst/exdata/csvtest/FFPE.csv"

  if(any(files == "")){
    files <- tcltk::tk_choose.files()
  }


  inputdatalist <- list()

  ## File path is a file
  ### get dir and extension
  #### load these




  for (i in 1:length(files)) {
    inputdatalist[[i]] <- data.table::fread(files[i], check.names = F)

  }

  names(inputdatalist) <- tools::file_path_sans_ext(basename(files))




  # test files for correct format
  ## can they be evluated?

  rsdatatest(inputdatalist)

  return(inputdatalist)

}



#######################################################




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


#######################################################
