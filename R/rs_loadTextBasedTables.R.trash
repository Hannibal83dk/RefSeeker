



#' Read in expression data from text based files (csv, tsv, txt, etc.) with auto delimiter detection.
#' Please note that though the most common uses of the file types has been tested, some delimiter and file type combination may be incompatible.
#'
#' @param files An optional file path or a vector of file paths one or more data files.
#' If no files are given a dialog will allow selection of files
# #' @param batch The function will search the folder of the provided file path to find all files with same extension to load.
#'
#' @return A list or list of lists of input data tables
#'
#' @export
#'
#'
#'
#'
#' @examples
#'
#' \dontrun{
#'
#' rs_load.table()
#'
#' rs_load.table("filepath")
#'
#' rs_load.table(c("fileone", "filetwo"))
#'
#' }
#'
#'
#'
rs_load.table <- function(files = ""){

  warning("Use of this function is discouraged, please consider using rs_loaddata() instead")
  # filepath = "./inst/exdata/csvtest/FFPE.csv"

  if(any(files == "")){
    files <- tcltk::tk_choose.files()
  }


  inputdatalist <- list()
  #filetype <- tools::file_ext(filepath)

  ## File path is a file
  ### get dir and extension
  #### load these

  # if(filetype != ""){
  #   dir <- dirname(filepath)
  # }

  ## filepath is a folder
  ### filepath is the dir
  #### find files and load them


  # if(filetype == ""){ # filepath is dir
  #   dir <- filepath
  #   filetype <- tools::file_ext(list.files(dir)[1])
  # }


    #files <- list.files(dir, pattern = paste0("*.", filetype))


    for (i in 1:length(files)) {
      inputdatalist[[i]] <- data.table::fread(files[i], check.names = F)

    }

    names(inputdatalist) <- tools::file_path_sans_ext(basename(files))




  # test files for correct format
  ## can they be evluated?

  rsdatatest(inputdatalist)

  return(inputdatalist)

}



#######################################################3

#' Read in reffinder data files with auto delimiter detection
#'
#' @param filepath An optional file path to a data file. The function will try to find all files with same extension to load.
#'
#' @return A list or list of lists of input data tables
#' @export
#'
#' @note This functions has been depricated and will be removed in the near future. Please use rs_load.table() for importing data sets from text based formats"

rs_loadtxtdata <- function(filepath = ""){

  warning("Please note this functions has been depricated and will be removed in the near future. Please use rs_load.table() for importing data sets from text based formats")

  return(rs_load.table(filepath))

}

#######################################################3

#' Read in csv files for processing
#'
#' @param filepath An optional file path to a data file. The function will try to find all files with same extension to load.
#'
#' @return A list or list of lists of input data tables
#' @export
#'
#' @note This functions has been depricated and will be removed in the near future. Please use rs_load.table() for importing data sets from text based formats"

rs_loadcsvdata <- function(filepath = ""){

  warning("Please note this functions has been depricated and will be removed in the near future. Please use rs_load.table() for importing data sets from text based formats")

  return(rs_load.table(filepath))

}

#######################################################3


# #' Read in reffinder data files with auto delimiter detection
# #'
# #' @param filepath An optional file path to a data file. The function will try to find all files with same extension to load.
# #'
# #' @return A list or list of lists of input data tables
# #' @export
# #'
# #' @importFrom data.table fread
# #'
# #'
# #'
# #' @examples
# #'
# #' \dontrun{
# #' rs_loadtxtdata()
# #' }
# #'
# #'

# rs_loadtxtdata <- function(filepath = ""){
#
#   if(filepath == ""){
#     filepath <- file.choose()
#   }
#
#
#   inputdatalist <- list()
#   filetype <- tools::file_ext(filepath)
#
#   ## filepath is a file
#   ### get dir and extension
#   #### load these
#
#   if(filetype != ""){
#     dir <- dirname(filepath)
#   }
#
#   ## filepath is a folder
#   ### filepath is the dir
#   #### find files and load them
#
#
#   if(filetype == ""){ # filepath is dir
#     dir <- filepath
#     filetype <- tools::file_ext(list.files(dir)[1])
#   }
#
#
#   files <- list.files(dir, pattern = paste0("*.", filetype))
#
#
#   for (i in 1:length(files)) {
#     inputdatalist[[i]] <- data.table::fread(paste0(dir, "/", files[i]))
#   }
#
#   names(inputdatalist) <- gsub(paste0(".", filetype), "", files)
#
#   # test files for correct format
#   ## can they be evluated?
#
#
#   rsdatatest(inputdatalist)
#   return(inputdatalist)
#
# }
