

#' Creates batch output from selected comma separated txt files
#'
#' @param input A path to a file txt-like files (txt, csv, tsv etc.) containing a properly formatted reffinder data table. All files with the same file extension in that folder will be loaded as a data set. (See vignette about preparing and loading txt-like files)
#' @param outdir A selected output directory, if not set a dialog will appear to allow selection for output directory
#' @param outtype type of data output can be "csv", "excel" or "tsv" - Have not been implemented yet
#' @param printgraph If TRUE print a graph of the result in the output folder
#'
#' @return Nothing. Creates excel files and graphs with stabilities for each sheet of a selected excel file
#' @export
#'
#'
#' @import readxl
#'
#'
#' @examples
#' \dontrun{
#' rs_csvbatch()
#' }
#'
#'
#'
rs_csvbatch <- function(input = "", outdir = "", outtype = "csv",  printgraph = TRUE){

  # Read in the data from the files.
  ## A file is selected and all files in the folder matching the file extension will be loaded as a batch.
  ## The reffinder data is then calculated on these.
  data <- rs_reffinder(rs_loadtxtdata())

  # Selecting an output folder.

  if(outdir == ""){
    outdir <- rsoutdirselect()
  }


  # If no output folder is detected the working directory is selected.
  if(is.na(outdir)){outdir <- getwd()}

  # Save the names of the datasets
  nms <- names(data)

  # For each data set, print two csv files, stabiltiy value and stability ranking
  for (i in 1:length(data)) {
    utils::write.csv(data[[i]][1], paste0(outdir, "/", nms[i], "_StabilityTable_", Sys.Date(), ".txt"))
    cat(paste("csv file created at: ", paste0(outdir, "/", nms[i], "_StabilityTable_", Sys.Date(), ".txt"), "\n"))

    utils::write.csv(data[[i]][2], paste0(outdir, "/", nms[i] ,"_RankTable_",Sys.Date(), ".txt"))
    cat(paste("csv file created at: ", paste0(outdir, "/", nms[i], "_RankTable_", Sys.Date(), ".txt"), "\n"))

  }

  if(printgraph == TRUE){

    ####
    # Get the desired type of graph from the user
    graphtype <- rsgraphtypeselect()

    if(graphtype == "single"){
      # For single/individual graphs one for each data set must be created
      for (i in 1: length(nms)) {

        #temp <- data[i]
        rs_graph(data[[i]], paste(outdir, "/", nms[i], sep = ""), forceSingle = TRUE)
      }

    } else if(graphtype == "multi"){
      rs_graph(data, paste(outdir, "/", paste(nms,collapse = '-'), sep = ""))
    }
    ########
  }
}

