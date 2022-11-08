

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
#' @param outdir A path to an folder to output graphs and tables
#' @param inputfile One or more paths to files containing expression data
#' @param filename A Prefix to use for file names.
#' @param graphtype A Type of graph, may be "individual" for one graph per data set, or "multi" for a combined graph of all data sets.
#' @param ordering Used to control sorting of the x axis, use "Target","delta-Ct","BestKeeper","Normfinder","geNorm" or "Comprehensive Rank".
#' @param imagetype A string determining which image file type the graph should be saved to. can be one of; "png", "tiff", "jpeg" or "svg".
#' @param orientation Selection of the orientation of the bars in the graph. May be "horizontal" or "vertical". Actually anything other than "horizontal" will be interpreted as vertical.
#' @param tabletype Select type of output may be one of ; "xlsx", "ods, csv", "tsv", "txt",  "docx-stability",  "docx-rank" or  "docx-combi"
#'
#' @return A list of string representing answer given in the dialog
#' 1) Was the ok button pressed? - TRUE/FALSE
#' 2) Output directory selected
#' 3) Graph type selected - individual/multi
#' 4) Ordering of targets on graph x-axis - reffinder/genorm/normfinder/bestkeeper/deltact/targets
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
rsdialog <- function(outdir = "", inputfile = c("No selection"), filename = "", graphtype = "multi", ordering = "Comprehensive Rank",  imagetype = "png", orientation = "horizontal", tabletype = "xlsx"){


  if(outdir == ""){outdir <- getwd()}

  #inputfile <- c("No selection")

  flnm <- tclVar(filename)
  # filename <- ""

  ok = FALSE

  # graphtype <- "multi"
  grph <- tclVar(graphtype)

  # ordering <- "Comprehensive Rank"
  srt <- tclVar(ordering)

  #imagetype <- "png"
  img <- tclVar(imagetype)

  # orientation <- "horizontal"
  ori <- tclVar(orientation)


  # tabletype <- "xlsx"
  tbltype <- tclVar(tabletype)


  slctinfile <- function(){

    inputfile <<- rsselectfiles()
    #inputfile <<- tk_choose.files()
    tkconfigure(filelabel, text =  paste(basename(inputfile), sep = ", "))
  }

  slctfldr <- function(){
    outdir <<- tk_choose.dir()

    ifelse(nchar(outdir) > 40, anch <- "e", anch <- "w")
    tkconfigure(outdirlabel, text = outdir, anchor = anch)
  }


  radio1press <- function(){graphtype <<- "individual"}
  radio2press <- function(){graphtype <<- "multi"}



  quit <- function(){
    tkdestroy(tt)
    ok <<- FALSE
  }

  okfunc <- function(){
    tkdestroy(tt)
    ok <<- TRUE
    filename <<- tclvalue( flnm)

  }

  tt <- tktoplevel()
  tkwm.title(tt,"RefSeeker dialog")


  spacerlabel <- tklabel(tt, text = "")

  filelabel <- tklabel(tt, text = inputfile, font = "size 10 bold")
  outdirlabel <- tklabel(tt, text = outdir, width = 40, font = "size 10 bold", anchor = "e")


  fileslct.but <- tkbutton(tt, text="Select input file(s)", font = "size 10 bold", command=slctinfile)

  fldrslct.but <- tkbutton(tt, text="Change output folder", font = "size 10 bold", command=slctfldr)

  flnm.field <- tkentry(tt, textvariable = flnm, width = 40)

  # graphtype type radio
  graphtyperadio1 <- tkradiobutton(tt, text = "Individual", font = "size 10 bold", variable = grph, value = "individual", command = radio1press)
  graphtyperadio2 <- tkradiobutton(tt, text = "Multi", font = "size 10 bold", variable = grph, value = "multi", command = radio2press)


  # ordering radio functions
  orderingradio1press <- function(){ordering <<- "Comprehensive Rank"}
  orderingradio2press <- function(){ordering <<- "geNorm"}
  orderingradio3press <- function(){ordering <<- "Normfinder"}
  orderingradio4press <- function(){ordering <<- "BestKeeper"}
  orderingradio5press <- function(){ordering <<- "delta-Ct"}
  orderingradio6press <- function(){ordering <<- "Targets"}
  #graphtype ordering radio
  orderingradio1 <- tkradiobutton(tt, text = "Comprehensive Rank", font = "size 10 bold", variable = srt, value = "Comprehensive Rank", command = orderingradio1press)
  orderingradio2 <- tkradiobutton(tt, text = "geNorm", font = "size 10 bold", variable = srt, value = "geNorm", command = orderingradio2press)
  orderingradio3 <- tkradiobutton(tt, text = "Normfinder", font = "size 10 bold", variable = srt, value = "Normfinder", command = orderingradio3press)
  orderingradio4 <- tkradiobutton(tt, text = "BestKeeper", font = "size 10 bold", variable = srt, value = "BestKeeper", command = orderingradio4press)
  orderingradio5 <- tkradiobutton(tt, text = "delta-Ct", font = "size 10 bold", variable = srt, value = "delta-Ct", command = orderingradio5press)
  orderingradio6 <- tkradiobutton(tt, text = "Targets", font = "size 10 bold", variable = srt, value = "Targets", command = orderingradio6press)


  # imagetype type radio functions
  imgradio1press <- function(){imagetype <<- "png"}
  imgradio2press <- function(){imagetype <<- "tiff"}
  imgradio3press <- function(){imagetype <<- "jpeg"}
  imgradio4press <- function(){imagetype <<- "svg"}
  imgradio5press <- function(){imagetype <<- "none"}

  # imagetype type radio
  imgradio1 <- tkradiobutton(tt, text = "PNG", font = "size 10 bold", variable = img, value = "png", command = imgradio1press)
  imgradio2 <- tkradiobutton(tt, text = "TIFF", font = "size 10 bold", variable = img, value = "tiff", command = imgradio2press)
  imgradio3 <- tkradiobutton(tt, text = "JPEG", font = "size 10 bold", variable = img, value = "jpeg", command = imgradio3press)
  imgradio4 <- tkradiobutton(tt, text = "SVG", font = "size 10 bold", variable = img, value = "svg", command = imgradio4press)#, state = "disable")
  imgradio5 <- tkradiobutton(tt, text = "None", font = "size 10 bold", variable = img, value = "none", command = imgradio5press)




  # Orientation type radio functions
  orientradio1press <- function(){orientation <<- "horizontal"}
  orientradio2press <- function(){orientation <<- "vertical"}
  # graphtype orientation
  orientradio1 <- tkradiobutton(tt, text = "Horizontal", font = "size 10 bold", variable = ori, value = "horizontal", command = orientradio1press)
  orientradio2 <- tkradiobutton(tt, text = "Vertical", font = "size 10 bold", variable = ori, value = "vertical", command = orientradio2press)


  # table type radio functions
  tabradio1press <- function(){tabletype <<- "xlsx"}
  tabradio2press <- function(){tabletype <<- "ods"}

  tabradio3press <- function(){tabletype <<- "csv"}
  tabradio4press <- function(){tabletype <<- "tsv"}
  tabradio5press <- function(){tabletype <<- "txt"}

  tabradio6press <- function(){tabletype <<- "docx-stability"}
  tabradio7press <- function(){tabletype <<- "docx-rank"}
  tabradio8press <- function(){tabletype <<- "docx-combi"}

  # Data output radio
  tabradio1 <- tkradiobutton(tt, text = "XLSX", font = "size 10 bold", variable = tbltype, value = "xlsx", command = tabradio1press)
  tabradio2 <- tkradiobutton(tt, text = "ODS", font = "size 10 bold", variable = tbltype, value = "ods", command = tabradio2press)#, state = "disable")
  tabradio3 <- tkradiobutton(tt, text = "CSV", font = "size 10 bold", variable = tbltype, value = "csv", command = tabradio3press)#, state = "disable")
  tabradio4 <- tkradiobutton(tt, text = "TSV", font = "size 10 bold", variable = tbltype, value = "tsv", command = tabradio4press)#, state = "disable")
  tabradio5 <- tkradiobutton(tt, text = "TXT", font = "size 10 bold", variable = tbltype, value = "txt", command = tabradio5press)#, state = "disable")
  tabradio6 <- tkradiobutton(tt, text = "DOCX-Stability", font = "size 10 bold", variable = tbltype, value = "docx-stability", command = tabradio6press)#, state = "disable")
  tabradio7 <- tkradiobutton(tt, text = "DOCX-Rank", font = "size 10 bold", variable = tbltype, value = "docx-rank", command = tabradio7press)#, state = "disable")
  tabradio8 <- tkradiobutton(tt, text = "DOCX-Combi", font = "size 10 bold", variable = tbltype, value = "docx-combi", command = tabradio8press)#, state = "disable")



  q.but <- tkbutton(tt, text = "Quit", font = "size 10 bold", command = quit)
  ok.but <- tkbutton(tt, text = "Ok", font = "size 10 bold", command = okfunc)

  # Set the style of the separator
  styl <- tcl("ttk::style", "configure", "TSeparator", "background", "yellow")

  # tkgrid(tklabel(tt, text = "Input file(s):", font = "size 10 bold"), filelabel, fileslct.but, column = 1, row = 1, columnspan = 5, rowspan = 1, pady = 10, padx = 10, sticky = "w")

  tkgrid(tklabel(tt, text = "Input file(s):", font = "size 10 bold"), column = 0, row = 0, columnspan = 1, rowspan = 1, pady = 10, padx = 10 , sticky = "w")
  tkgrid(filelabel, column = 1, row = 0, columnspan = 2, rowspan = 1, pady = 10, padx = 10, sticky = "w")
  tkgrid(fileslct.but, column = 3, row = 0, columnspan = 1, rowspan = 1, pady = 10, padx = 10)
  #
  # ###############33
  tkgrid(tklabel(tt, text = "Output directory:", font = "size 10 bold"), column = 0, row = 1, columnspan = 1, pady = 10, padx = 10, sticky = "w")
  #
  tkgrid(outdirlabel, column = 1, row = 1, columnspan = 2, pady = 10, padx = 10, sticky = "w")
  #
  #
  tkgrid(fldrslct.but, column = 3, row = 1, columnspan = 1, pady = 10, padx = 10)
  #
  # # tkgrid(tklabel(tt, text = "Output directory:"), outdirlabel, fldrslct.but, columnspan = 10, pady = 10, padx = 10)
  # ########################
  tkgrid(tklabel(tt, text = "File name prefix:", font = "size 10 bold"), flnm.field, column = 0, row = 2, columnspan = 1, pady = 10, padx = 10, sticky = "w")
  tkgrid(flnm.field, column = 1, row = 2, columnspan = 2, pady = 10, padx = 10, sticky = "w")
  #
  #tkgrid(ttkseparator(tt, orient = "horizontal", style = "TSeparator", class = "ttk.Separator", takefocus ="1", cursor = ""),
         #row = 8, column = 1, columnspan = 4, padx = 20, pady = 0, sticky = "ew")
  #
  tkgrid(tklabel(tt, text = "Select type of graph:", font = "size 10 bold"), row = 4, column = 0,  columnspan = 1, pady = 10, padx = 10, sticky = "w")
  tkgrid(graphtyperadio1, row = 4, column = 1, columnspan = 1, pady = 10, padx = 10, sticky = "w")
  tkgrid(graphtyperadio2, row = 4, column = 2, columnspan = 1, pady = 10, padx = 10, sticky = "w")
  #
  tkgrid(ttkseparator(tt, orient = "horizontal", style = "TSeparator", class = "ttk.Separator", takefocus ="1", cursor = ""),
         row = 5, column = 1, columnspan = 4, padx = 20, pady = 0, sticky = "ew")
  #
  tkgrid(tklabel(tt, text = "Select ordering of target-axis:", font = "size 10 bold"), row = 6, column = 0, columnspan = 1, pady = 10, padx = 10, sticky = "w")
  tkgrid(orderingradio1, row = 6, column = 1, columnspan = 1, pady = 10, padx = 10, sticky = "w")
  tkgrid(orderingradio2, row = 6, column = 2, columnspan = 1, pady = 10, padx = 10, sticky = "w")
  tkgrid(orderingradio3, row = 6, column = 3, columnspan = 1, pady = 10, padx = 40, sticky = "w")
  tkgrid(orderingradio4, row = 7, column = 1, columnspan = 1, pady = 10, padx = 10, sticky = "w")
  tkgrid(orderingradio5, row = 7, column = 2, columnspan = 1, pady = 10, padx = 10, sticky = "w")
  tkgrid(orderingradio6, row = 7, column = 3, columnspan = 1, pady = 10, padx = 40, sticky = "w")
  #
  tkgrid(ttkseparator(tt, orient = "horizontal", style = "TSeparator", class = "ttk.Separator", takefocus ="1", cursor = ""),
         row = 8, column = 1, columnspan = 4, padx = 20, pady = 0, sticky = "ew")
  #
  tkgrid(tklabel(tt, text = "Select orientation of bars:", font = "size 10 bold"), row = 9, column = 0, columnspan = 1, pady = 10, padx = 10, sticky = "w")
  tkgrid(orientradio1, row = 9, column = 1, columnspan = 1, pady = 10, padx = 10, sticky = "w")
  tkgrid(orientradio2, row = 9, column = 2, columnspan = 1, pady = 10, padx = 10, sticky = "w")
  #
  tkgrid(ttkseparator(tt, orient = "horizontal", style = "TSeparator", class = "ttk.Separator", takefocus ="1", cursor = ""),
         row = 10, column = 1, columnspan = 4, padx = 20, pady = 0, sticky = "ew")
  #
  tkgrid(tklabel(tt, text = "Select graph output file format:", font = "size 10 bold"), row = 11, column = 0, columnspan = 1, pady = 10, padx = 10, sticky = "w")
  tkgrid(imgradio1, row = 11, column = 1, columnspan = 1, pady = 10, padx = 10, sticky = "w")
  tkgrid(imgradio2, row = 11, column = 2, columnspan = 1, pady = 10, padx = 10, sticky = "w")
  tkgrid(imgradio3, row = 11, column = 3, columnspan = 1, pady = 10, padx = 40, sticky = "w")
  tkgrid(imgradio4, row = 11, column = 4, columnspan = 1, pady = 10, padx = 10, sticky = "w")
  #
  # #tkgrid(tklabel(tt, text = "Select data output format:"), tabradio1, tabradio2, tabradio3, tabradio4, tabradio5, tabradio6, tabradio7, tabradio8, row = 9, columnspan = 7, rowspan = 1, pady = 10, padx = 10, sticky = "w")
  #
  # themes <- as.character(tcl("ttk::style", "theme", "names"))
  # tcl("ttk::style", "theme", "use", themes[1])

  #styl <- as.character(tcl("ttk::style", "configure", "background", "black"))
  # styl <- as.character(tcl("ttk::style", "configure", "TSeparator", "background", "black"))
  #
  # tcl("ttk::style", "element", "names")



  tkgrid(ttkseparator(tt, orient = "horizontal", style = "TSeparator", class = "ttk.Separator", takefocus ="1", cursor = ""),
         row = 12, column = 1, columnspan = 4, padx = 20, pady = 0, sticky = "ew")

  tkgrid(tklabel(tt, text = "Select data output format:", font = "size 10 bold"), row = 13, column = 0, columnspan = 1, pady = 10, padx = 10, sticky = "w")
  tkgrid(tabradio1, row = 13, column = 1, columnspan = 1, pady = 10, padx = 10, sticky = "w")
  tkgrid(tabradio2, row = 13, column = 2, columnspan = 1, pady = 10, padx = 10, sticky = "w")
  tkgrid(tabradio3, row = 13, column = 3, columnspan = 1, pady = 10, padx = 40, sticky = "w")
  tkgrid(tabradio4, row = 13, column = 4, columnspan = 1, pady = 10, padx = 10, sticky = "w")
  tkgrid(tabradio5, row = 14, column = 1, columnspan = 1, pady = 10, padx = 10, sticky = "w")
  tkgrid(tabradio6, row = 14, column = 2, columnspan = 1, pady = 10, padx = 10, sticky = "w")
  tkgrid(tabradio7, row = 14, column = 3, columnspan = 1, pady = 10, padx = 40, sticky = "w")
  tkgrid(tabradio8, row = 14, column = 4, columnspan = 1, pady = 10, padx = 10, sticky = "w")
  #
  # #tkgrid(tklabel(tt, text = "Select data output format:"), tabradio1, tabradio2, tabradio3, tabradio4, tabradio5, tabradio6, tabradio7, tabradio8, row = 9, columnspan = 7, rowspan = 1, pady = 10, padx = 10, sticky = "w")
  #
  # #tkgrid(q.but, spacerlabel, ok.but, columnspan = 10, pady= 20, padx = 10)
  #
  #
  #
  # ####
  tkgrid(q.but, row = 15, column = 1, columnspan = 1, pady = 10, padx = 10, sticky = "w")
  #
  tkgrid(ok.but, row = 15, column = 3, columnspan = 1, pady= 20, padx = 40, sticky = "e")


  tkwait.window(tt)

  return(c(ok, outdir, filename, graphtype, ordering, orientation, imagetype, tabletype, inputfile))


}



# rsdialog()

