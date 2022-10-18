

#' find the path to the testref folder containing reffences for checking results during testing
#'
#' @return A path to testref folder
#'
#'
#'
#'
testrefpath <- function(){

  if(grepl("testthat", getwd(), fixed = TRUE)){
    searchdir = "../.."

  } else { searchdir = "."}

  path <- dir(searchdir, pattern = "exdata", recursive = TRUE, include.dirs = TRUE, full.names = TRUE)

  #path <- paste0(path,"/testref")

  #write(path[1], "~/R/testlog.txt", append = TRUE)

  return(path[1])
}


