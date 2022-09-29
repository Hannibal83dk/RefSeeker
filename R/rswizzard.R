



#' Perform stability analysis through interactive wizzard
#'
#' @return Creates graphs and result export to
#' @export
#'
#' @examples
#'
#' \dontrun{
#' rs_wizzard()
#' }
rs_wizzard <- function(){
  answer <- rsdialog()

  outdir <- answer[2]

  # Load in the data
  datalist <- list()
  for (i in 7:length(answer)) {

    filext <- sub('.*\\.', '', answer[i])

    if(substring( filext, 1,3) ==  "xls" | filext == "ods"){ datalist <- c(datalist, rs_loadexceldata(answer[i])) }

    if(filext == "csv" | filext == "tsv" | filext == "txt"){ datalist <- c(datalist, rs_load.table(answer[i])) }

  }


  # Renaming duplicated names in the data list.
  if(any(duplicated(names(datalist)))){

    names(datalist) <- make.unique(names(datalist), ".")
    warning("Some datasets have douplicated names. Suffix has been added to douplicates")

  }


  results <- rs_reffinder(datalist)

  rs_graph(reffinderlist = results, filename = paste0(answer[2],"/testing"), forceSingle = (answer[3] == "individual"), ordering = answer[4])


  rs_exceltable(results, paste0(answer[2],"/testing"))

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


