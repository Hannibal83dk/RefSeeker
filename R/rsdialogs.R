

#' File selectioin diolog for RefSeeker
#'
#' @return A character vector containing the selected files
#'
#' @import tcltk
#'
#'
#'
#' @examples
#'
#' \dontrun{
#'
#' rsselectfiles()
#' }
#'
#'
#'
rsselectfiles <- function(){


    filter <- c("{{Supported} {.txt}}",
              "{{Supported} {.csv}}",
              "{{Supported} {.tsv}}",
              "{{Supported} {.xlsx}}",
              "{{Supported} {.xls}}",
              "{{Supported} {.ods}}",
              "{{All files} {*}}")


    files <- tclvalue( tkgetOpenFile(multiple = TRUE, initialdir = getwd(),  filetypes = paste(filter, collapse = " "))  )

    if(files != ""){ files <- strsplit(files, " ")[[1]] }

    if(any(files == "")) {
        return(cat("No files selected, terminating"))
    }

  return(files)

}






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
  #dir <- tclVar("")
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

  selsingle.but <- tkbutton(tt, text="Single graphs", command=slctsnge)

  selmulti.but <- tkbutton(tt, text="A multigraph", command=slctmlt)

  tkgrid(tklabel(tt,text=paste(" Please select the graph type.\n\n A single graph for each dataset or \n a faceted graph compairing all datasets \n")), columnspan = 2, pady = 15)

  tkgrid(selsingle.but, selmulti.but, pady= 10, padx= 10)
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



##########################################################################

#' Dialog window to select a folder for output files
#'
#' @return A list of string representing answer given in the dialog
#' 1) Was the ok button pressed? - TRUE/FALSE
#' 2) Output directory selected
#' 3) Graphtype selected - individual/multi
#' 4) Sorting of targets on graph x-axis - reffinder/genorm/normfinder/bestkeeper/deltact/targets
#' 4) File type selected for graph output - png/tiff/jpeg/none
#' 5) File type for data output - xlsx/ods/csv/tsv/txt
#' 6) One or more input data file paths
#'
#' @export
#'
#'
#' @import tcltk
#' @examples
#'
#'\dontrun{
#' dialog.answers <- rsdialog()
#'}
#'
#'
rsdialog <- function(){

  outdir <- getwd()
  inputfile <- c("No selection")

  ok = FALSE

  grph <- "multi"
  graph <- tclVar("multi")

  srt <- "Comprehensive Rank"
  sort <- tclVar("Comprehensive Rank")

  img <- "png"
  image <- tclVar("png")

  datout <- "xlsx"
  dataout <- tclVar("xlsx")



  slctinfile <- function(){

    inputfile <<- rsselectfiles()
    #inputfile <<- tk_choose.files()
    tkconfigure(filelabel, text =  paste(basename(inputfile), sep = ", "))
  }

  slctfldr <- function(){
    outdir <<- tk_choose.dir()
    tkconfigure(outdirlabel, text = outdir)
  }


  radio1press <- function(){grph <<- "individual"}
  radio2press <- function(){grph <<- "multi"}

  # Sort radio functions
  sortradio1press <- function(){srt <<- "Comprehensive Rank"}
  sortradio2press <- function(){srt <<- "geNorm"}
  sortradio3press <- function(){srt <<- "Normfinder"}
  sortradio4press <- function(){srt <<- "BestKeeper"}
  sortradio5press <- function(){srt <<- "delta-Ct"}
  sortradio6press <- function(){srt <<- "Targets"}


  # Image type radio functions
  imgradio1press <- function(){img <<- "png"}
  imgradio2press <- function(){img <<- "tiff"}
  imgradio3press <- function(){img <<- "jpeg"}
  imgradio4press <- function(){img <<- "none"}

  # table type radio functions
  tabradio1press <- function(){datout <<- "xlsx"}
  tabradio2press <- function(){datout <<- "ods"}
  tabradio3press <- function(){datout <<- "csv"}
  tabradio4press <- function(){datout <<- "tsv"}
  tabradio5press <- function(){datout <<- "txt"}


  quit <- function(){
    tkdestroy(tt)
    ok <<- FALSE
  }

  okfunc <- function(){
    tkdestroy(tt)
    ok <<- TRUE
  }

  tt <- tktoplevel()
  tkwm.title(tt,"RefSeeker dialog")


  spacerlabel <- tklabel(tt, text = "")

  filelabel <- tklabel(tt, text = inputfile)
  outdirlabel <- tklabel(tt, text = outdir)


  fileslct.but <- tkbutton(tt, text="Select input file(s)", command=slctinfile)

  fldrslct.but <- tkbutton(tt, text="Change output folder", command=slctfldr)

  #filename.field <- tktext(tt, "Header", command = )

  # Graph type radio
  graphradio1 <- tkradiobutton(tt, text = "Individual", variable = graph, value = "individual", command = radio1press)
  graphradio2 <- tkradiobutton(tt, text = "Multi", variable = graph, value = "multi", command = radio2press)

  #Graph sorting radio
  sortradio1 <- tkradiobutton(tt, text = "Comprehensive Rank", variable = sort, value = "Comprehensive Rank", command = radio1press)
  sortradio2 <- tkradiobutton(tt, text = "geNorm", variable = sort, value = "geNorm", command = radio2press)
  sortradio3 <- tkradiobutton(tt, text = "Normfinder", variable = sort, value = "Normfinder", command = radio1press)
  sortradio4 <- tkradiobutton(tt, text = "BestKeeper", variable = sort, value = "BestKeeper", command = radio2press)
  sortradio5 <- tkradiobutton(tt, text = "delta-Ct", variable = sort, value = "delta-Ct", command = radio1press)
  sortradio6 <- tkradiobutton(tt, text = "Targets", variable = sort, value = "Targets", command = radio2press)

  # Image type radio
  imgradio1 <- tkradiobutton(tt, text = "PNG", variable = image, value = "png", command = imgradio1press)
  imgradio2 <- tkradiobutton(tt, text = "TIFF", variable = image, value = "tiff", command = imgradio2press)
  imgradio3 <- tkradiobutton(tt, text = "JPEG", variable = image, value = "jpeg", command = imgradio3press)
  imgradio4 <- tkradiobutton(tt, text = "None", variable = image, value = "none", command = imgradio4press)#, state = "disable")

  # Graph orientation





  # Data output radio
  tabradio1 <- tkradiobutton(tt, text = "XLSX", variable = dataout, value = "xlsx", command = tabradio1press)
  tabradio2 <- tkradiobutton(tt, text = "ODS", variable = dataout, value = "ods", command = tabradio2press)#, state = "disable")
  tabradio3 <- tkradiobutton(tt, text = "CSV", variable = dataout, value = "csv", command = tabradio3press)#, state = "disable")
  tabradio4 <- tkradiobutton(tt, text = "TSV", variable = dataout, value = "tsv", command = tabradio4press)#, state = "disable")
  tabradio5 <- tkradiobutton(tt, text = "TXT", variable = dataout, value = "txt", command = tabradio5press)#, state = "disable")


  q.but <- tkbutton(tt, text = "Quit", command = quit)
  ok.but <- tkbutton(tt, text = "Ok", command = okfunc)


  tkgrid(tklabel(tt, text = "Input file(s):"), filelabel, fileslct.but, columnspan = 7, rowspan = 2, pady = 10, padx = 10)
  tkgrid(tklabel(tt, text = "Output directory:"), outdirlabel, fldrslct.but, columnspan = 7, pady = 10, padx = 10)

  tkgrid(tklabel(tt, text = "Select type of graph:"), graphradio1, graphradio2, columnspan = 7, pady = 10, padx = 10, sticky = "w")

  tkgrid(tklabel(tt, text = "Select ordering of x-axis:"), sortradio1, sortradio2, sortradio3, sortradio4, sortradio5, sortradio6, columnspan = 7, pady = 10, padx = 10, sticky = "w")

  tkgrid(tklabel(tt, text = "Select graph output file format:"), imgradio1, imgradio2, imgradio3, imgradio4, columnspan = 7, pady = 10, padx = 10, sticky = "w")
  tkgrid(tklabel(tt, text = "Select data output format:"), tabradio1, tabradio2, tabradio3, tabradio4, tabradio5, columnspan = 7, pady = 10, padx = 10, sticky = "w")

  #tkgrid(q.but, spacerlabel, ok.but, columnspan = 10, pady= 20, padx = 10)

  tkgrid(q.but, columnspan = 7, pady= 20, padx = 10, row = 7, column = 2, sticky = "w")

  tkgrid(ok.but, columnspan = 7, pady= 20, padx = 10, row = 7, column = 7, sticky = "e")


  #tkgrid(ok.but, column = 3, columnspan = 1, pady= 10, padx= 10)
  #tkgrid(q.but, pady = 10, padx = 10)


  tkwait.window(tt)

  return(c(ok, outdir, grph, srt, img, datout, inputfile))


}


