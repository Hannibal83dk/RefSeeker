

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


