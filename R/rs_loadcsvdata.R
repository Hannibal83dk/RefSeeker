



#' Read in csv files for processing
#'
#' @param filepath An optional file path to a data file. The function try to find all files with same extension to load.
#'
#' @return A list or list of lists of input data tables
#' @export
#'
#'
#'
#'
#' @examples
#'
#' \dontrun{
#' rs_loadcsvdata()
#' }
#'
#'
#'
rs_loadcsvdata <- function(filepath = ""){

  # filepath = "./inst/exdata/csvtest/FFPE.csv"

  if(filepath == ""){
     filepath <- file.choose()
  }

  inputdatalist <- list()
  filetype <- tools::file_ext(filepath)

  ## Filepath is a file
  ### get dir and extension
  #### load these

  if(filetype != ""){
    dir <- dirname(filepath)
  }

  ## filepath is a folder
  ### filepath is the dir
  #### find files and load them


  if(filetype == ""){ # filepath is dir
    dir <- filepath
    filetype <- tools::file_ext(list.files(dir)[1])
  }


    files <- list.files(dir, pattern = paste0("*.", filetype))


    for (i in 1:length(files)) {
      inputdatalist[[i]] <- utils::read.csv(paste0(dir, "/", files[i]), check.names = F)

    }

    names(inputdatalist) <- gsub(paste0(".", filetype), "", files)

  # test files for correct format
  ## can they be evluated?

  return(inputdatalist)

}




#' Read in reffinder data files with auto delimiter detection
#'
#' @param filepath An optional file path to a data file. The function will try to find all files with same extension to load.
#'
#' @return A list or list of lists of input data tables
#' @export
#'
#' @importFrom data.table fread
#'
#'
#'
#' @examples
#'
#' \dontrun{
#' rs_loadtxtdata()
#' }
#'
#'
#'
rs_loadtxtdata <- function(filepath = ""){

  if(filepath == ""){
    filepath <- file.choose()
  }


  inputdatalist <- list()
  filetype <- tools::file_ext(filepath)

  ## filepath is a file
  ### get dir and extension
  #### load these

  if(filetype != ""){
    dir <- dirname(filepath)
  }

  ## filepath is a folder
  ### filepath is the dir
  #### find files and load them


  if(filetype == ""){ # filepath is dir
    dir <- filepath
    filetype <- tools::file_ext(list.files(dir)[1])
  }


  files <- list.files(dir, pattern = paste0("*.", filetype))


  for (i in 1:length(files)) {
    inputdatalist[[i]] <- data.table::fread(paste0(dir, "/", files[i]))
  }

  names(inputdatalist) <- gsub(paste0(".", filetype), "", files)

  # test files for correct format
  ## can they be evluated?

  return(inputdatalist)

}
