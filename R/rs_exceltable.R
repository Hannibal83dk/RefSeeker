





#' Export RefSeeker results tables
#'
#' @param refseekerlist A RefSeeker list normally provided by rs_reffinder() function
#' @param filename A filename for the output, may include relative path to desired folder
#' @param tabletype Select type of output may be one of ; "xlsx", "csv", "tsv", "txt" or "png" (comming soon)
#' @param addDate Should the current date be added to the file name
#'
#' @return Nothing but creates files containg table data
#' @export
#'
#' @examples
#' set.seed(100)
#' ct_vals <- matrix(rnorm(5*20, mean = 25), ncol = 5, nrow = 20)
#' dimnames(ct_vals)[[2]] <-  c("gene1", "gene2", "gene3", "gene4", "gene5")
#' res <- rs_reffinder(ct_vals)
#'
#' \dontrun{
#' rs_exporttable(res, filename = "../data/EOCstability", tabletype = "csv", addDate = TRUE )
#'
#' }
#'
#'
#'
#'
rs_exporttable <- function(refseekerlist, filename = "Stability-table", tabletype = "xlsx", addDate = TRUE){


  if(tabletype == "xlsx"){
    rs_exceltable(refseekerlist = refseekerlist, filename = filename, addDate = addDate)

  }

  if(tabletype == "ods"){
    rs_odstable(refseekerlist = refseekerlist, filename = filename, addDate = addDate)

  }

  if(tabletype == "csv"){

    #cat("runningcsvtable\n")
    rscsvtable(refseekerlist, filename, addDate = addDate)

  }

  if(tabletype == "tsv"){
    #cat("running tsvtable\n")
    rstsvtable(refseekerlist, filename, addDate = addDate)

  }


  if(tabletype == "txt"){
    #cat("running txttable\n")
    rstxttable(refseekerlist, filename, addDate = addDate)

  }


}



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



#' Creates an excel workbook in the working directory containing the provided tables as sheets
#'
#' @param filename An excel file name or path
#' @param ... One or more tables to be written as sheets
#' @param addFilter Toggle filter selection. Adds a filter from first row of table.
#' @param overwrite Toggle whether to overwrite a possible existing file
#'
#' @return nothing
#' @export
#'
#' @import openxlsx
#'
#' @examples
#'
#' set.seed(100)
#' table1 <- data.frame(matrix(rnorm(5*20, mean = 25), ncol = 5, nrow = 20))
#'
#' set.seed(200)
#' table2 <- data.frame(matrix(rnorm(5*20, mean = 25), ncol = 5, nrow = 20))
#'
#'
#' rsexcelfile("/Output/fileNameofExcelFile", table1, table2, overwrite = TRUE)
#'
#'
#'
# library(openxlsx)
# addFilter = TRUE
#
# sheets <- list(reffinderlist$stabilityTable, reffinderlist$rankTable)
#
# rsexcelfile("testexcel", reffinderlist$stabilityTable, reffinderlist$rankTable)
rsexcelfile <- function(filename, ..., addFilter = TRUE, overwrite = FALSE){

  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop(
      cat("function stopped due to openxlxsx"),
      "Package \"openxlsx\" must be installed to use this function.",
      call. = FALSE
    )
  }

  wb <- openxlsx::createWorkbook()
  sheets <- list(...)

  sheetnames <- as.list(substitute(list(...)))

  sheetnames <- unlist(sheetnames[2:length(sheetnames)])

  for (i in 1:length(sheets)) {
    openxlsx::addWorksheet(wb, sheetnames[[i]])
    openxlsx::writeData(wb, sheetnames[[i]], sheets[[i]], withFilter = addFilter)
  }

  openxlsx::saveWorkbook(wb, paste(filename, ".xlsx", sep = ""),
                         overwrite = overwrite)

  if(dirname(paste(filename, ".xlsx", sep = "")) == "."){

    cat(paste("An excel file was created in the working directory: ", getwd(), "/", filename, ".xlsx\n", sep = ""))

  } else { cat(paste("An excel file was created at the folowing path: ", filename, ".xlsx\n", sep = "")) }

  #cat(dirname(paste(filename, ".xlsx", sep = "")))
}





#'
#'
#' Output ods spreadsheet containing stability and ranking as separate sheets
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
#' rs_odstable(rffndr)
#' }
#'
#'
rs_odstable <- function(refseekerlist, filename = "Stability-table", addDate = TRUE){


  if(!is.null(refseekerlist$stabilityTable)){ # Simple one dimensional list, just one data set (refseekerlist$stabilityTable exists)

    rsStability <- refseekerlist$stabilityTable
    rsRank <- refseekerlist$rankTable

    if(addDate == TRUE){
      filename <- paste0(filename, "_", Sys.Date(), ".ods")
    }

    readODS::write_ods(rsStability, filename, sheet = "rsStability")
    readODS::write_ods(rsRank, filename, sheet = "rsRank", append = TRUE)


    if(dirname(paste(filename, ".ods", sep = "")) == "."){

      cat(paste("An ods file was created in the working directory: ", getwd(), "/", filename, ".xlsx\n", sep = ""))

    } else { cat(paste("An ods file was created at the folowing path: ", filename, "\n", sep = "")) }



  }


  if(!is.null(refseekerlist[[1]]$stabilityTable)){ # A list of lists of results tables (refseekerlist[[1]]$stabilityTable exists)

    for (i in 1:length(refseekerlist)) {

      rsStability <- refseekerlist[[i]]$stabilityTable
      rsRank <- refseekerlist[[i]]$rankTable

      newfilename <- paste0(filename, "_", names(refseekerlist[i]))

      if(addDate == TRUE){
        newfilename <- paste0(newfilename, "_", Sys.Date(), ".ods")
      }

      readODS::write_ods(rsStability, newfilename, sheet = "rsStability")
      readODS::write_ods(rsRank, newfilename, sheet = "rsRank", append = TRUE)


      if(dirname(newfilename) == "."){

        cat(paste("An ods file was created in the working directory: ", getwd(), "/", newfilename, "\n", sep = ""))

      } else { cat(paste("An ods file was created at the folowing path: ", newfilename, "\n", sep = "")) }


    }

  }

}







#filename <- "~/Desktop/test/tester"






#' Creates two csv files containing stabiltiy and rankings using comma as separator
#'
#' @param refseekerlist A RefSeekerlist created by the rs_reffinder function
#' @param filename A file name to identify the data
#' @param addDate Possibility to automatically add date to output file name
#'
#' @return Nothing, output an excel file in the working directory or an output folder selected by the file name
#' @return Nothing. Creates csv files in selected directory
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  data <- rs_miRNA
#'  res <- rs_reffinder(data)
#'  rscsvtable(res, "path/to/folder", filenameprefix = "EOC")
#' }
#'
#'
#'
#'
#'
rscsvtable <- function(refseekerlist, filename, addDate = TRUE) {
  cat("Starting csvtable\n")
  # Save the names of the datasets
  nms <- names(refseekerlist)

  # For each data set, print two csv files, stabiltiy value and stability ranking
  for (i in 1:length(refseekerlist)) {

    # Create the file name with or without date
    if(addDate){
      newfilename <- paste0(filename, "_", nms[i], "_StabilityTable_", Sys.Date(), ".csv")
    } else {
      newfilename <- paste0(filename, "_", nms[i], "_StabilityTable_", ".csv")
    }

    utils::write.csv(refseekerlist[[i]][1], newfilename, row.names = FALSE)



    # Report absolute path of file.
    cat(paste0("csv file created at: ", absfilepath(newfilename), "\n"))

    # if(dirname(filename) == "."){
    #   cat(paste0("csv file created at: ", getwd(),"/", newfilename, "\n"))
    # } else {
    #   cat(paste("csv file created at: ", newfilename, "\n"))
    # }



    # Create the file name with or without date
    if(addDate){
      newfilename <- paste0(filename, "_", nms[i], "_RankTable_", Sys.Date(), ".csv")
    } else {
      newfilename <- paste0(filename, "_", nms[i], "_RankTable_", ".csv")
    }

    utils::write.csv(refseekerlist[[i]][2], newfilename, row.names = FALSE)

    # Report absolute path of file.
    cat(paste0("csv file created at: ", absfilepath(newfilename), "\n"))

  }

}




#' Creates two tsv files containing stabiltiy and rankings using tab as separator
#'
#' @param refseekerlist A RefSeekerlist created by the rs_reffinder function
#' @param filename A file name to identify the data
#' @param addDate Possibility to automatically add date to output file name
#'
#' @return Nothing, output an excel file in the working directory or an output folder selected by the file name
#' @return Nothing but creates tsv files in selected directory
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  data <- rs_miRNA
#'  res <- rs_reffinder(data)
#'  rstsvtable(res, "path/to/folder", filenameprefix = "EOC")
#' }
#'
#'
rstsvtable <- function(refseekerlist, filename, addDate = TRUE) {
  cat("Starting tsvtable\n")
  # Save the names of the datasets
  nms <- names(refseekerlist)

  # For each data set, print two tsv files, stabiltiy value and stability ranking
  for (i in 1:length(refseekerlist)) {

    # Create the file name with or without date
    if(addDate){
      newfilename <- paste0(filename, "_", nms[i], "_StabilityTable_", Sys.Date(), ".tsv")
    } else {
      newfilename <- paste0(filename, "_", nms[i], "_StabilityTable_", ".tsv")
    }

    utils::write.table(refseekerlist[[i]][1], newfilename, sep = "\t", row.names = FALSE)



    # Report absolute path of file.
    cat(paste0("tsv file created at: ", absfilepath(newfilename), "\n"))

    # if(dirname(filename) == "."){
    #   cat(paste0("csv file created at: ", getwd(),"/", newfilename, "\n"))
    # } else {
    #   cat(paste("csv file created at: ", newfilename, "\n"))
    # }



    # Create the file name with or without date
    if(addDate){
      newfilename <- paste0(filename, "_", nms[i], "_RankTable_", Sys.Date(), ".tsv")
    } else {
      newfilename <- paste0(filename, "_", nms[i], "_RankTable_", ".tsv")
    }

    utils::write.table(refseekerlist[[i]][2], newfilename, sep = "\t", row.names = FALSE)

    # Report absolute path of file.
    cat(paste0("tsv file created at: ", absfilepath(newfilename), "\n"))

  }

}
#
# rstsvtable <- function(refseekerlist, outdir, filenameprefix = "") {
#
#   # Save the names of the datasets
#   nms <- names(data)
#
#   # For each data set, print two csv files, stabiltiy value and stability ranking
#   for (i in 1:length(data)) {
#     utils::write.table(data[[i]][1], paste0(outdir, "/", filenameprefix, nms[i], "_StabilityTable_", Sys.Date(), ".tsv"), sep = "\t", row.names = FALSE)
#     cat(paste("csv file created at: ", paste0(outdir, "/", filenameprefix, nms[i], "_StabilityTable_", Sys.Date(), ".tsv"), "\n"))
#
#     utils::write.table(data[[i]][2], paste0(outdir, "/", filenameprefix, nms[i] ,"_RankTable_",Sys.Date(), ".tsv"), sep = "\t", row.names = FALSE)
#     cat(paste("csv file created at: ", paste0(outdir, "/", filenameprefix, nms[i], "_RankTable_", Sys.Date(), ".tsv"), "\n"))
#
#   }
#
# }



#' Creates two txt files containing stabiltiy and rankings using space as separator
#'
#' @param refseekerlist A RefSeekerlist created by the rs_reffinder function
#' @param filename A file name to identify the data
#' @param addDate Possibility to automatically add date to output file name
#'
#' @return Nothing, output an excel file in the working directory or an output folder selected by the file name
#' @return Nothing but creates csv files in selected directory
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  data <- rs_miRNA
#'  res <- rs_reffinder(data)
#'  rstxttable(res, "path/to/folder", filenameprefix = "EOC")
#' }
#'
#'

rstxttable <- function(refseekerlist, filename, addDate = TRUE) {
  cat("Starting tsvtable\n")
  # Save the names of the datasets
  nms <- names(refseekerlist)

  # For each data set, print two txt files, stabiltiy value and stability ranking
  for (i in 1:length(refseekerlist)) {

    # Create the file name with or without date
    if(addDate){
      newfilename <- paste0(filename, "_", nms[i], "_StabilityTable_", Sys.Date(), ".txt")
    } else {
      newfilename <- paste0(filename, "_", nms[i], "_StabilityTable_", ".txt")
    }

    utils::write.table(refseekerlist[[i]][1], newfilename, row.names = FALSE)



    # Report absolute path of file.
    cat(paste0("txt file created at: ", absfilepath(newfilename), "\n"))

    # if(dirname(filename) == "."){
    #   cat(paste0("csv file created at: ", getwd(),"/", newfilename, "\n"))
    # } else {
    #   cat(paste("csv file created at: ", newfilename, "\n"))
    # }



    # Create the file name with or without date
    if(addDate){
      newfilename <- paste0(filename, "_", nms[i], "_RankTable_", Sys.Date(), ".txt")
    } else {
      newfilename <- paste0(filename, "_", nms[i], "_RankTable_", ".txt")
    }

    utils::write.table(refseekerlist[[i]][2], newfilename, row.names = FALSE)

    # Report absolute path of file.
    cat(paste0("txt file created at: ", absfilepath(newfilename), "\n"))

  }

}



#
# rstxttable <- function(refseekerlist, outdir, filenameprefix = "") {
#
#   # Save the names of the datasets
#   nms <- names(data)
#
#   # For each data set, print two csv files, stabiltiy value and stability ranking
#   for (i in 1:length(data)) {
#     utils::write.table(data[[i]][1], paste0(outdir, "/", filenameprefix, nms[i], "_StabilityTable_", Sys.Date(), ".txt"), row.names = FALSE)
#     cat(paste("csv file created at: ", paste0(outdir, "/", filenameprefix, nms[i], "_StabilityTable_", Sys.Date(), ".txt"), "\n"))
#
#     utils::write.table(data[[i]][2], paste0(outdir, "/", filenameprefix, nms[i] ,"_RankTable_",Sys.Date(), ".txt"), row.names = FALSE)
#     cat(paste("csv file created at: ", paste0(outdir, "/", filenameprefix, nms[i], "_RankTable_", Sys.Date(), ".txt"), "\n"))
#
#   }
#
# }
#





#' Find avbsolute path of file
#'
#' @param relativepath A relative path to a file
#'
#' @return the absolute path to a file
#'
#' @examples
#'
#' \dontrun{
#' absfilepath("../path/to/file")
#' }
#'
#'
absfilepath <- function(relativepath) {

  path <- paste(
    normalizePath(dirname(relativepath)),

    basename(relativepath),
    sep = "/"
  )

  return(path)
}









