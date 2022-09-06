

#' Creates batch output from selected excel file
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
#' rs_batchExcel()
#' }
#'
#'
#'
rs_batchExcel <- function(){

  filepath <- file.choose()

  outdir <- rfoutdirselect()

  sheets <- excel_sheets(filepath)

  if(is.na(outdir)){outdir <- getwd()}

  for (i in 1: length(sheets)) {

    temp_data <- read_excel(filepath, i)
    ref_temp <- rs_reffinder(temp_data)
    rs_exceltable(ref_temp, paste(outdir, "/", sheets[i], sep = ""))
    cat("\n")
    rs_graph(ref_temp, paste(outdir, "/", sheets[i], sep = ""), outputPng = TRUE)
    cat("\n")
  }
}



#' Creates batch output from selected excel file
#'
#' @param input A path to an excel file containing properly formatted reffinder data tables. This means column representing genes/targets and rows representing samples,
#' data being Ct/Cp/Cq values, with no missing data since the validity of these results have not been confirmed.
#' @param outdir A selected output directory, if not set a dialog will appear to allow selection for output directory
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
#' rs_batchExcel()
#' }
#'
#'
#'
#'
rs_batchExcel2 <- function(input = "", outdir = "", printgraph = TRUE){

  filepath <- file.choose()

  rfres <- rs_reffinder(rs_loadexceldata(filepath))

  outdir <- rfoutdirselect()


  if(is.na(outdir)){outdir <- getwd()}

  graphtype <- rfgraphtypeselect()

  nms <- names(rfres)

  #exltype <- rfexltypeselect()

  if(graphtype == "single"){

    for (i in 1: length(nms)) {

      temp <- rfres[i]
      rs_graph(rfres[[i]], paste(outdir, "/", nms[i], sep = ""), outputPng = TRUE, forceSingle =TRUE)
    }

  } else if(graphtype == "multi"){
    rs_graph(rfres, paste(outdir, "/", tools::file_path_sans_ext(basename(filepath)), sep = ""), outputPng = TRUE)
  }


  for (i in 1: length(nms)) {

    rs_exceltable(rfres[[i]], paste(outdir, "/", nms[i], sep = ""))

  }
}

