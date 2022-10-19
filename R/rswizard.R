



#' Perform stability analysis through interactive wizard
#'
#' @return Creates graphs and result export to
#' @export
#'
#' @examples
#'
#' \dontrun{
#' rs_wizard()
#' }
rs_wizard <- function(){

  answer <- rsdialog()

  outdir <- answer[2]

  # Load in the data

  datalist <- rs_loaddata(answer[7:length(answer)])
  results <- rs_reffinder(datalist)

  if(answer[5] != "none"){
    rs_graph(refseekerlist = results, filename = paste0(answer[2]), forceSingle = (answer[3] == "individual"), ordering = answer[4])
  }

  rs_exporttable(results, paste0(answer[2],"/testing"), tabletype = answer[6])

}

# rs_wizzard()
#
#
# input <- rs_loadexceldata()
#
#
#
# results <- rs_reffinder(input)
#
# plot <- rs_graph(results, filename <- "/home/patrick//Desktop/temp/testing", filetype = "tiff")
#
#
# p <- plot
#
# width = 2025
# height = 2156
#
#
#
# reffinderlist <- results
#
# filename <- "/home/patrick//Desktop/temp/testing"
#


