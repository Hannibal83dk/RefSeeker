
#' Perform stability analysis through interactive wizard
#'
#' @param outdir A path to an folder to output graphs and tables
#' @param inputfile One or more paths to files containing expression data
#' @param filename A Prefix to use for file names.
#' @param graphtype A Type of graph, may be "individual" for one graph per data set, or "multi" for a combined graph of all data sets.
#' @param ordering Used to control sorting of the x axis, use "Target","delta-Ct","BestKeeper","Normfinder","geNorm" or "Comprehensive Rank".
#' @param imagetype A string determining which image file type the graph should be saved to. can be one of; "png", "tiff", "jpeg" or "svg".
#' @param orientation Selection of the orientation of the bars in the graph. May be "horizontal" or "vertical". Actually anything other than "horizontal" will be interpreted as vertical.
#' @param tabletype Select type of output may be one of ; "xlsx", "ods, csv", "tsv", "txt",  "docx-stability",  "docx-rank" or  "docx-combi"
# #' @param ... Extra parameters passed to rs_graph. Namely width, height, units, res and colors.
#'
#' @return Creates graphs and result export to
#' @export
#'
#' @examples
#'
#' \dontrun{
#' rs_wizard()
#' }
rs_wizard <- function(outdir = "",
                      inputfile = c("No selection"),
                      filename = "RefSeeker",
                      graphtype = "multi",
                      ordering = "Comprehensive Rank",
                      imagetype = "png",
                      orientation = "horizontal",
                      tabletype = "xlsx"){


  answer <- rsdialog(outdir = outdir,
                     inputfile = inputfile,
                     filename = filename,
                     graphtype = graphtype,
                     ordering = ordering,
                     imagetype = imagetype,
                     orientation = orientation,
                     tabletype = tabletype)

  outdir <- answer[2]

  # Load in the data

  datalist <- rs_loaddata(answer[9:length(answer)])
  results <- rs_reffinder(datalist)


  if(answer[3] == ""){answer[3] == "RefSeeker"}

  if(answer[5] != "none"){
    rs_graph(refseekerlist = results,
             filename = paste0(answer[2], "/", answer[3]),
             filetype = answer[7],
             forceSingle = (answer[4] == "individual"),
             ordering = answer[5],
             orientation = answer[6])
  }

  rs_exporttable(results, filename = paste0(answer[2],"/", answer[3]), tabletype = answer[8])

}

# rs_graph(refseekerlist = results,
#          filename = paste0(answer[2], "/", answer[3]),
#          filetype = answer[7],
#          forceSingle = (answer[4] == "individual"),
#          ordering = answer[5],
#          orientation = answer[6])
# ,         height = 240)


# rs_wizard(outdir = "",
#                       inputfile = "/home/phdp/OneDrive/Dokumenter/R/refSeeker/inst/exdata/excel-ods-test/Reffinder_data_test.xlsx",
#                       filename = "RefSeeker",
#                       graphtype = "multi",
#                       ordering = "Comprehensive Rank",
#                       imagetype = "png",
#                       orientation = "horizontal",
#                       tabletype = "xlsx",
#                       ...)
#
#
#
# file.choose(
#
# )



