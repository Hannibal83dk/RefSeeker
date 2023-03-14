


#' Export RefSeeker results tables
#'
#' @param refseekerlist A RefSeeker list normally provided by rs_reffinder() function
#' @param filename A filename for the output, may include relative path to desired folder
#' @param tabletype Select type of output may be one of ; "xlsx", "ods, csv", "tsv", "txt",  "docx-stability",  "docx-rank" or  "docx-combi"
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
rs_exporttable <- function(refseekerlist, filename = "Stability-table", tabletype = "xlsx", addDate = TRUE){

  # Single datasets are converted to to list of one dataset. Names are extracted from input variable
  if(!is.null(refseekerlist$stabilityTable)){

    # The list is created and the data set is the first and only object in the list
    templist <- list(refseekerlist)

    # If the data set provided was not part of a list of data sets then the name is not a subset and the substitute value can be used as the name directly
    if(typeof(substitute(refseekerlist)) == "symbol"){
      names(templist)  <- as.character(substitute(refseekerlist))
    }



    # If the data set originally came from a list but only the single subset was provided, then the name is derived from a subset
    if(typeof(substitute(refseekerlist)) == "language"){
      names(templist)  <- as.character(substitute(refseekerlist)[[3]])
    }

    refseekerlist <- templist

  }



  if(tabletype == "xlsx"){
    rs_exceltable(refseekerlist = refseekerlist, filename = filename, addDate = addDate)

  }

  if(tabletype == "ods"){
    rsodstable(refseekerlist = refseekerlist, filename = filename, addDate = addDate)

  }

  if(tabletype == "csv"){

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

  if(tabletype == "docx-stability"){
    #cat("running txttable\n")
    rsdoctable(refseekerlist, filename = filename, tabletype = "stability", addDate = addDate)

  }

  if(tabletype == "docx-rank"){
    #cat("running txttable\n")
    rsdoctable(refseekerlist, filename = filename, tabletype = "rank", addDate = addDate)

  }

  if(tabletype == "docx-combi"){
    #cat("running txttable\n")
    rsdoctable(refseekerlist, filename = filename, tabletype = "both", addDate = addDate)

  }

}

#rs_exporttable(results$Fresh_Frozen, "txttest", tabletype = "csv")

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
#' rsodstable(rffndr)
#' }
#'
#'
rsodstable <- function(refseekerlist, filename = "Stability-table", addDate = TRUE){


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

    utils::write.csv(refseekerlist[[i]][[1]], newfilename, row.names = FALSE)



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

    utils::write.csv(refseekerlist[[i]][[2]], newfilename, row.names = FALSE)

    # Report absolute path of file.
    cat(paste0("csv file created at: ", absfilepath(newfilename), "\n"))

  }

}

# refseekerlist <- results$Fresh_Frozen
# filename <- "~/Desktop/test/csvfix"
#
# addDate = TRUE


#' Creates two tsv files containing stabiltiy and rankings using tab as separator
#'
#' @param refseekerlist A RefSeekerlist created by the rs_reffinder function
#' @param filename A file name to identify the data
#' @param addDate Possibility to automatically add date to output file name
#'
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

    utils::write.table(refseekerlist[[i]][[1]], newfilename, sep = "\t", row.names = FALSE)



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

    utils::write.table(refseekerlist[[i]][[2]], newfilename, sep = "\t", row.names = FALSE)

    # Report absolute path of file.
    cat(paste0("tsv file created at: ", absfilepath(newfilename), "\n"))

  }

}



#' Creates two txt files containing stabiltiy and rankings using space as separator
#'
#' @param refseekerlist A RefSeekerlist created by the rs_reffinder function
#' @param filename A file name to identify the data
#' @param addDate Possibility to automatically add date to output file name
#'
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

    utils::write.table(refseekerlist[[i]][[1]], newfilename, row.names = FALSE)



    # Report absolute path of file.
    cat(paste0("txt file created at: ", absfilepath(newfilename), "\n"))


    # Create the file name with or without date
    if(addDate){
      newfilename <- paste0(filename, "_", nms[i], "_RankTable_", Sys.Date(), ".txt")
    } else {
      newfilename <- paste0(filename, "_", nms[i], "_RankTable_", ".txt")
    }

    utils::write.table(refseekerlist[[i]][[2]], newfilename, row.names = FALSE)

    # Report absolute path of file.
    cat(paste0("txt file created at: ", absfilepath(newfilename), "\n"))

  }

}



####################################################################################3


#' Creates flextable tables for RefSeeker results
#'
#' @param refseekerlist A RefSeekerlist created by the rs_reffinder function.
#' @param filename A file name prefix. May contain a path to an output directory.
#' @param tabletype A string representing the table to be printer can be; "stability", "rank" or "both".
#' @param addDate Logical indicating whether or not to add the current date to the output file name.
#'
#' @return A flextable object in the form of html code string.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'   table <- rsdoctable(results, filename = "~/Desktop/test/doc2_", tabletype = "both")
#' }
#'
#'
rsdoctable <- function(refseekerlist, filename = "", tabletype = "stability",  addDate = "TRUE"){

  tbls <- list()

  if(!is.null(refseekerlist$stabilityTable)){ # Simple one dimensional list, just one data set (refseekerlist$stabilityTable exists)

    head <- unlist(base::strsplit(deparse(substitute(refseekerlist)), "[$]"))

    head <- head[length(head)]

    head <- gsub("_", " ", head)

    refseekerlist <- list(refseekerlist)

    names(refseekerlist) <- head

  }

  if(!is.null(refseekerlist[[1]]$stabilityTable)){ # A list of lists of results tables (refseekerlist[[1]]$stabilityTable exists)


    names(refseekerlist) <- gsub("_", " ", names(refseekerlist))

  }


  if(tabletype == "both"){

    for (i in 1:length(refseekerlist)) {

      tbls[[i]] <- rsdoctable2(refseekerlist[[i]],
                               filename = paste0(filename, "_", names(refseekerlist)[i]),
                               caption = names(refseekerlist)[i],
                               addDate = addDate)
    }

  } else{

    for (i in 1:length(refseekerlist)) {

      #cat("test1")
      tbls[[i]] <- rsdoctable1(refseekerlist[[i]],
                               filename = paste0(filename, "_", names(refseekerlist)[i]),
                               caption = names(refseekerlist)[i],
                               addDate = addDate,
                               type = tabletype)

    }

  }

  return(tbls)

}




#' Creates tables based on aither stability or rank from a RefSeeker list.
#'
#' @param refseekerlist A RefSeekerlist created by the rs_reffinder function
#' @param filename A file name prefix. May contain a path to an output directory.
#' @param caption A string representing the header of the table.
#' @param type A string determining which table to format can be; "stability" or "RankW"
#' @param addDate Logical indicating whether or not to add the current date to the output file name.

#' @return A kable object consisting of a html table of choice
#' @export
#'
#' @import flextable
#' @importFrom officer prop_section
#' @importFrom officer page_size
#' @importFrom officer page_mar
#' @importFrom officer fp_par
#' @importFrom officer fp_border
#'
#'
#' @examples
#' \dontrun{
#'   rsdoctable1(results, filename = "Test1", caption = "Group1", type = "rank")
#' }
#'
#'
#'
rsdoctable1 <- function(refseekerlist, filename = "", caption = "", type = "stability", addDate = TRUE){


  if(type == "stability"){

    table <- refseekerlist$stability
    caption <- paste0(caption, " - Stability")


  } else if (type == "rank"){
    table <- refseekerlist$rank
    caption <- paste0(caption, " - Rank")
  }
  # my_header <- data.frame(
  #   col_keys = c("Target", "blank1", "delta-Ct", "blank2", "BestKeeper", "blank3", "Normfinder", "blank4", "geNorm", "blank", "Comprehensive Rank"),
  #   line1 = c("Target", "", "delta-Ct", "", "BestKeeper", "", "Normfinder", "", "geNorm", "", "Comprehensive Rank"),
  #   line2 = c("Target", "",  "Avg. STDEV.", "", "MAD", "", "Stability", "", "Avg.M", "", "Geom. mean value")
  # )

  my_header <- data.frame(
    col_keys = c("Target", "delta-Ct", "BestKeeper", "Normfinder", "geNorm", "Comprehensive Rank"),
    line1 = c("Target", "delta-Ct", "BestKeeper", "Normfinder", "geNorm", "Comprehensive Rank"),
    line2 = c("Target",  "Avg. STDEV.", "MAD", "Stability", "Avg.M", "Geom. mean value")
  )


  ft <- flextable(table, col_keys = my_header$col_keys)

  ft <- set_caption(ft, caption, fp_p = officer::fp_par(), align_with_table = FALSE)
  ft <- set_header_df(ft, mapping = my_header, key = "col_keys")
  ft <- theme_booktabs(ft)
  ft <- merge_v(ft, part = "header")
  ft <- merge_h(ft, part = "header")
  ft <- align(ft, align = "center", part = "all")

  ft <- font(ft, j = NULL, fontname = "Calibri", part = "all")
  ft <- fontsize(ft, i = NULL, j = NULL, size = 9, part = "all")
  ft <- bold(ft, i = c(1), part = "header")
  ft <- padding(ft, j=NULL, padding.top = 3, part = "all")
  ft <- padding(ft, j=NULL, padding.bottom = 1, part = "all")
  ft <- padding(ft, j=NULL, padding.right = 0, part = "all")
  ft <- padding(ft, j=NULL, padding.left = 0, part = "all")
  ft <- padding(ft, j=NULL, padding.left = 3, part = "all")
  ft <- line_spacing(ft, space = 0.6, part = "all")


  ft <- autofit(ft)
  ft <- hline_bottom(ft, border = officer::fp_border(width = 2), part = "header")
  #ft <- empty_blanks(ft, part = "header")

  ft <- hline_top(ft, border = officer::fp_border(width = 2), part = "header")
  ft <- vline(ft, i = c(1,2), border = officer::fp_border(color = "white", width = 3), part = "header")
  ft <- vline(ft, i = c(1), border = officer::fp_border(color = "white", width = 3), part = "body")

  ft <- fix_border_issues(ft)


  ft

  sect_properties <- officer::prop_section(
    page_size = officer::page_size(orient = "portrait"),
    type = "continuous",
    page_margins = officer::page_mar())


  #save_as_docx(ft2, pr_section = sect_properties,  path = paste0("StabilityTableTest1", Sys.Date(), ".docx"))


  # ft <- flextable(table, col_keys = my_header$col_keys)
  #
  # ft <- set_caption(ft, caption, fp_p = officer::fp_par(), align_with_table = FALSE)
  # ft <- set_header_df(ft, mapping = my_header, key = "col_keys")
  # ft <- theme_booktabs(ft)
  # ft <- merge_v(ft, part = "header")
  # ft <- merge_h(ft, part = "header")
  # ft <- align(ft, align = "center", part = "all")
  # ft <- autofit(ft)
  # ft <- hline_bottom(ft, border = officer::fp_border(width = 2), part = "header")
  # ft <- empty_blanks(ft, part = "header")
  # ft <- fix_border_issues(ft)
  # ft <- hline_top(ft, border = officer::fp_border(width = 2), part = "header")
  # ft
  #
  # sect_properties <- officer::prop_section(
  #   page_size = officer::page_size(orient = "landscape"),
  #   type = "continuous",
  #   page_margins = officer::page_mar())






  if(addDate){filename <- paste0(filename, "_", Sys.Date())}

  save_as_docx(ft, pr_section = sect_properties,  path = paste0(filename, ".docx"))

  cat(paste0("A doc-table file was created at: ", paste0(filename, ".docx\n")))

  return(ft)


}



#' Creates tables based on aither stability and rank from a RefSeeker list.
#'
#' @param refseekerlist A RefSeeker list created by the rs_reffinder function
#' @param filename A file name prefix. May contain a path to an output directory.
#' @param caption A string representing the header of the table.
#' @param addDate Logical indicating whether or not to add the current date to the output file name.
#'
#' @return A kable object consisting of a html code for a table showing rank and stabiltiy of each reffinder algorithm
#' @export
#'
#' @import flextable
#' @importFrom officer prop_section
#' @importFrom officer page_size
#' @importFrom officer page_mar
#' @importFrom officer fp_par
#' @importFrom officer fp_border
#'
#'
#'
#'
#' @examples
#' \dontrun{
#'   rsdoctable(results, filename = "Test1", caption = "Group1")
#' }
#'
#'
#'
rsdoctable2 <- function(refseekerlist, filename = "", caption = "", addDate = TRUE){

  table <- data.frame( matrix(nrow = nrow(refseekerlist[[1]]), ncol = 11 ) )
  table[1] <- refseekerlist[[1]][1]

  for(i in 1:5){                    #       (ncol(results$Fresh_Frozen$stabilityTable))){

    table[i*2] <- refseekerlist[[1]][i+1]

    table[i*2+1] <- refseekerlist[[2]][i+1]

  }

  my_header <- data.frame(
    col_keys = c('X1', 'X2', 'X3', 'X4', 'X5', 'X6', 'X7', 'X8', 'X9', 'X10', 'X11'),
    line2 = c(" ", "delta-Ct", "delta-Ct", "BestKeeper","BestKeeper", "Normfinder", "Normfinder", "geNorm", "geNorm", "Comprehensive Rank", "Comprehensive Rank"),
    line3 = c("Target",  "Avg. STDEV.", "Rank", "MAD", "Rank", "Stability", "Rank", "Avg.M", "Rank", "Geom. mean value", "Rank" )
  )

  ft <- flextable(table, col_keys = my_header$col_keys)
  ft <- set_caption(ft, caption = caption, fp_p = officer::fp_par(), align_with_table = FALSE)
  ft <- set_header_df(ft, mapping = my_header, key = "col_keys")
  ft <- theme_booktabs(ft)
  ft <- merge_v(ft, part = "header")
  ft <- merge_h(ft, part = "header")
  ft <- align(ft, align = "center", part = "all")




  ft <- font(ft, j = NULL, fontname = "Calibri", part = "all")
  ft <- fontsize(ft, i = NULL, j = NULL, size = 9, part = "all")
  ft <- bold(ft, i = c(1), part = "header")
  ft <- padding(ft, j=NULL, padding.top = 3, part = "all")
  ft <- padding(ft, j=NULL, padding.bottom = 1, part = "all")
  ft <- padding(ft, j=NULL, padding.right = 0, part = "all")
  ft <- padding(ft, j=NULL, padding.left = 0, part = "all")
  ft <- padding(ft, j=NULL, padding.left = 3, part = "all")
  ft <- line_spacing(ft, space = 0.6, part = "all")



  ft <- autofit(ft)
  ft <- hline_bottom(ft, border = officer::fp_border(width = 2), part = "header")
  #ft <- empty_blanks(ft, part = "header")
  ft <- hline_top(ft, border = officer::fp_border(width = 2), part = "header")

  ft <- vline(ft, i = c(1,2), j = c(1,3,5,7,9), border = officer::fp_border(color = "white", width = 3), part = "header")
  ft <- vline(ft, i = c(1), j = c(1,3,5,7,9),  border = officer::fp_border(color = "white", width = 3), part = "body")


  ft <- fix_border_issues(ft)



  ft

  # sect_properties <- officer::prop_section(
  #   page_size = officer::page_size(orient = "landscape"),
  #   type = "continuous",
  #   page_margins = officer::page_mar())


  sect_properties <- officer::prop_section(
    page_size = officer::page_size(orient = "portrait"),
    type = "continuous",
    page_margins = officer::page_mar())


  if(addDate){filename <- paste0(filename, "_", Sys.Date())}

  save_as_docx(ft, pr_section = sect_properties,  path = paste0(filename, ".docx"))

  cat(paste0("A doc-table file was created at: ", paste0(filename, ".docx\n")))

  return(ft)
}




#######################################################################


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









