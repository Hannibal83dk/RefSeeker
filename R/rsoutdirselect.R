

library(tcltk)


#' Dialog window to select a folder for output files
#'
#' @return A string representing a path to a selected output folder
#' @export
#'
#'
#' @import tcltk
#' @examples
#'
#'\dontrun{
#' path <- rsoutdirselect()
#'}
#'
#'
rsoutdirselect <- function(){

  dirselect <- ""
  dir <- tclVar("")
  tt <- tktoplevel()

  tkwm.title(tt,"Select an output folder")

  chswd <- function() {

    dirselect <<- getwd()
    tkdestroy(tt)
  }

  slctfldr <- function(){
    dirselect <<- tk_choose.dir()
    tkdestroy(tt)
  }

  choosewd.but <- tkbutton(tt, text="Choose current working directory", command=chswd)

  fldrslct.but <- tkbutton(tt, text="Browse for folder", command=slctfldr)

  tkgrid(tklabel(tt,text=paste("Please choose an output folder or select\ncurrent working directory for output files\n\nCurrent working directory:\n ", getwd())), columnspan = 2, pady = 10)

  tkgrid(choosewd.but, fldrslct.but, pady= 10, padx= 10)
  tkwait.window(tt)

  return(dirselect)
}


#' Dialog window to select a graph type
#'
#' @return A string representing the selected type
#' @export
#'
#'
#' @import tcltk
#' @examples
#'
#'\dontrun{
#' path <- rsoutdirselect()
#'}
#'
#'
rsgraphtypeselect <- function(){

  selection <- ""
  dir <- tclVar("")
  tt <- tktoplevel()

  tkwm.title(tt,"Select graph type")

  slctsnge <- function() {

    selection <<- "single"
    tkdestroy(tt)
  }

  slctmlt <- function(){
    selection <<- "multi"
    tkdestroy(tt)
  }

  choosewd.but <- tkbutton(tt, text="Single graphs", command=slctsnge)

  fldrslct.but <- tkbutton(tt, text="A multigraph", command=slctmlt)

  tkgrid(tklabel(tt,text=paste(" Please select the graph type.\n\n A single graph for each dataset or \n a faceted graph compairing all datasets \n")), columnspan = 2, pady = 15)

  tkgrid(choosewd.but, fldrslct.but, pady= 10, padx= 10)
  tkwait.window(tt)
  return(selection)
}

#rsgraphtypeselect()



#' Dialog window to select an excel file type
#'
#' @return A string representing the selected type
#' @export
#'
#'
#' @import tcltk
#' @examples
#'
#'\dontrun{
#' path <- rsoutdirselect()
#'}
#'
#'
rsexltypeselect <- function(){

  selection <- ""
  dir <- tclVar("")
  tt <- tktoplevel()

  tkwm.title(tt,"Select excel file type")

  slctmlt <- function() {

    selection <<- "multi"
    tkdestroy(tt)
  }

  slctsnge <- function(){
    selection <<- "single"
    tkdestroy(tt)
  }

  mltslct.but <- tkbutton(tt, text="Multiple excel files", command=slctmlt)

  sngslct.but <- tkbutton(tt, text="Single excel file", command=slctsnge)

  tkgrid(tklabel(tt,text=paste(" Please select the excel file type type.\n\n Multiple excelfiles each containing results from a single dataset or \na single excel file containg results from all datasets \n")), columnspan = 2, pady = 15)

  tkgrid(mltslct.but, sngslct.but, pady= 10, padx= 10)
  tkwait.window(tt)
  return(selection)
}

# rsexltypeselect()


