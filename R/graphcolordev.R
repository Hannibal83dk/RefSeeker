
#
# rs_exporttable2 <- function(refseekerlist, filename = "Stability-table", tabletype = "xlsx", addDate = TRUE){
#
#   #print(substitute(refseekerlist)[[3]])
#
#   #return(substitute(refseekerlist))
#
#   if(!is.null(refseekerlist$stabilityTable)){
#
#     templist <- list(refseekerlist)
#
#     #print(typeof(substitute(refseekerlist)) == "symbol")
#     if(typeof(substitute(refseekerlist)) == "symbol"){
#       names(templist)  <- as.character(substitute(refseekerlist))
#     }
#
#
#
#     #print(typeof(substitute(refseekerlist)) == "language")
#     if(typeof(substitute(refseekerlist)) == "language"){
#       names(templist)  <- as.character(substitute(refseekerlist)[[3]])
#     }
#
#
#
#     #print(typeof(substitute(refseekerlist)))
#
#     #return(names(templist))
#
#     #names(templist)  <- as.character(substitute(refseekerlist))
#
#     refseekerlist <- templist
#
#     #print(refseekerlist)
#
#   }
#
#
#   if(tabletype == "xlsx"){
#     rs_exceltable(refseekerlist = refseekerlist, filename = filename, addDate = addDate)
#
#   }
#
#   if(tabletype == "ods"){
#     rsodstable(refseekerlist = refseekerlist, filename = filename, addDate = addDate)
#
#   }
#
#   if(tabletype == "csv"){
#
#     rscsvtable(refseekerlist, filename, addDate = addDate)
#
#   }
#
#   if(tabletype == "tsv"){
#     #cat("running tsvtable\n")
#     rstsvtable(refseekerlist, filename, addDate = addDate)
#
#   }
#
#
#   if(tabletype == "txt"){
#     #cat("running txttable\n")
#     rstxttable(refseekerlist, filename, addDate = addDate)
#
#   }
#
#   if(tabletype == "docx-stability"){
#     #cat("running txttable\n")
#     rsdoctable(refseekerlist, filename = filename, tabletype = "stability", addDate = addDate)
#
#   }
#
#   if(tabletype == "docx-rank"){
#     #cat("running txttable\n")
#     rsdoctable(refseekerlist, filename = filename, tabletype = "rank", addDate = addDate)
#
#   }
#
#   if(tabletype == "docx-combi"){
#     #cat("running txttable\n")
#     rsdoctable(refseekerlist, filename = filename, tabletype = "both", addDate = addDate)
#
#   }
#
# }
#
#

#
#
#
#
#
# data <- rs_loaddata()
#
#
#
#
# res <- rs_reffinder(data)
#
#
#
#
#
# # colors <- data.frame(target = names(data$FFPE),
# #
# #                      color = c("#2271b2",
# #                                "#2271b2",
# #                                "#2271b2",
# #                                "#2271b2",
# #                                "#359b73",
# #                                "#d55e00",
# #                                "#d55e00",
# #                                "#d55e00",
# #                                "#d55e00",
# #                                "#d55e00"))
#
# colors <- data.frame(target = c("UniSp2",
#                                 "UniSp4",
#                                 "UniSp6",
#                                 "cel-miR-39-3p",
#                                 "UniSp3_IPC",
#                                 "hsa-miR-30c-5p",
#                                 "hsa-miR-103a-3p",
#                                 "hsa-miR-191-5p",
#                                 "hsa-miR-23a-3p",
#                                 "hsa-miR-451a"),
#
#                      color = c("#2271b2",
#                                 "#2271b2",
#                                 "#2271b2",
#                                 "#2271b2",
#                                 "#359b73",
#                                 "#d55e00",
#                                 "#d55e00",
#                                 "#d55e00",
#                                 "#d55e00",
#                                 "#d55e00"))
#
#
#
#
#
#
#
#
#
# rs_graph(res, colors = colors)
#


# #' Creates kable tables for refseeker results
# #'
# #' @param refseekerlist A RefSeekerlist created by the rs_reffinder function
# #' @param tabletype A string representing the table to be printer can be; "stability", "rank" or "both"
# #  '
# #' @return A kable object in the form of html code string
# #' @export
# #'
# #' @examples
# #'
# #' \dontrun{
# #'   rs_ktable(results, "both")
# #' }
# #'
# #'
# rs_doctable <- function(refseekerlist, filename = "", tabletype = "stability",  addDate = "TRUE"){
#
#   tbls <- list()
#
#   if(!is.null(refseekerlist$stabilityTable)){ # Simple one dimensional list, just one data set (refseekerlist$stabilityTable exists)
#
#     #cat( deparse(substitute(refseekerlist)) )
#     #print("\n")
#     #print(names(refseekerlist))
#
#
#     #print(refseekerlist)
#
#     head <- unlist(base::strsplit(deparse(substitute(refseekerlist)), "[$]"))
#
#     head <- head[length(head)]
#
#     head <- gsub("_", " ", head)
#
#     refseekerlist <- list(refseekerlist)
#
#     names(refseekerlist) <- head
#
#
#     #print(paste0("head is ",head))
#
#
#
#   }
#
#
#   if(!is.null(refseekerlist[[1]]$stabilityTable)){ # A list of lists of results tables (refseekerlist[[1]]$stabilityTable exists)
#
#
#     names(refseekerlist) <- gsub("_", " ", names(refseekerlist))
#
#     #return(names(refseekerlist))
#   }
#
#   #print(names(refseekerlist))
#
#   if(tabletype == "both"){
#
#     for (i in 1:length(refseekerlist)) {
#
#       tbls[[i]] <- rsdoctable2(refseekerlist[[i]],
#                                 filename = paste0(filename, names(refseekerlist)[i]),
#                                 caption = names(refseekerlist)[i],
#                                 addDate = addDate)
#     }
#
#   } else{
#
#     for (i in 1:length(refseekerlist)) {
#
#       cat("test1")
#       tbls <- rsdoctable1(refseekerlist[[i]],
#                               filename = paste0(filename, names(refseekerlist)[i]),
#                               caption = names(refseekerlist)[i],
#                               addDate = addDate,
#                               type = tabletype)
#
#
#       #tbls[[i]] <- rs_ktable1(refseekerlist[i], type = tabletype)
#
#     }
#
#   }
#
#   return(tbls)
#
# }
#
#
# tbl <- rs_doctable(results, filename = "~/Desktop/test/doc2_", tabletype = "rank")
#
#
#
# caption = "Fresh Frozen"


#######################################################################
















































#
# tbl <- kableExtra::kbl(results[[1]]$stabilityTable, align = "c", caption = paste0(head, " - Stabiltiy"))
#
#
# tbls <- c(tbl)
#
# tbl <- rs_ktable1(results[1])
#
# tbls
#
#
#
# tbl <- rs_ktable(results$Fresh_Frozen, "both")
# print(tbl)
#
#
# kableExtra:::graphics_dev("C:/Users/PPET0123/Desktop/refseektest/kabletest800.png", width = 720, height = 360, res = 300, dev=grDevices::png)
#
#
# kableExtra:::kable_as_image()
# kableExtra:::print.kableExtra()
#
#
# dev.off()
#
# file <- rsoutdirselect()
#
# library(refSeeker)
# library(kableExtra)
#
#
#
#
#
#
#
# save_kable(tbl, "C:/Users/PPET0123/Desktop/refseektest/kabletest800.png", density = 800)
# save_kable(tbl, "~/Desktop/test/kabletable.png")
#
# grDevices::png
#
#
#
# refseekerlist <- results$Fresh_Frozen
#
# ###########################################
#
# install.packages("flextable")
#
#
# library(flextable)
# library(officer)
#
# table <- data.frame( matrix(nrow = nrow(refseekerlist[[1]]), ncol = 11 ) )
# table[1] <- refseekerlist[[1]][1]
#
# for(i in 1:5){                    #       (ncol(results$Fresh_Frozen$stabilityTable))){
#
#   table[i*2] <- refseekerlist[[1]][i+1]
#
#   table[i*2+1] <- refseekerlist[[2]][i+1]
#
# }
#
# names(table) <- c("Target",  "Avg. STDEV.", "Rank", "MAD", "Rank", "Stability", "Rank", "Avg.M", "Rank", "Geom. mean value", "Rank" )
# names(table) <- NULL
#
#
# my_header <- data.frame(
#   col_keys = c('X1', 'X2', 'X3', 'blank1', 'X4', 'X5', 'blank2', 'X6', 'X7', 'blank', 'X8', 'X9', 'blank3', 'X10', 'X11'),
#   line2 = c(" ", "delta-Ct", "delta-Ct", "", "BestKeeper","BestKeeper", "", "Normfinder", "Normfinder", "", "geNorm", "geNorm", "", "Comprehensive Rank", "Comprehensive Rank"),
#   line3 = c("Target",  "Avg. STDEV.", "Rank", "", "MAD", "Rank", "", "Stability", "Rank", "", "Avg.M", "Rank", "", "Geom. mean value", "Rank" )
# )
#
#
# ft <- flextable(table, col_keys = my_header$col_keys) %>%
#   set_header_df(mapping = my_header, key = "col_keys") %>%
#   theme_booktabs() %>%
#   merge_v(part = "header") %>%
#   merge_h(part = "header") %>%
#   align(align = "center", part = "all") %>%
#   autofit() %>%
#   hline_bottom(border = fp_border(width = 2), part = "header") %>%
#   empty_blanks(part = "header")  %>%
#   fix_border_issues() %>%
#   hline_top(border = fp_border(width = 2), part = "header")
#
#
# sect_properties <- prop_section(
#   page_size = page_size(orient = "landscape"),
#   type = "continuous",
#   page_margins = page_mar())
#
#
# save_as_docx(ft, pr_section = sect_properties,  path = "C:/Users/PPET0123/Desktop/refseektest/flextab.docx")
#
#
#

# rf_results$rankTable %>%
#   kbl() %>% kable_styling()
#
# rf_results$rankTable %>%
#   flextable() %>%
#   theme_vanilla() %>%
#   autofit() %>%
#   save_as_docx(path="H:/miRNAvalidation/Reffinder_projects/rankTable.docx")
#
#
# library(refSeeker)
# library(kableExtra)
# library(flextable)
#
# dat <- rs_loaddata()
#
# results <- rs_reffinder(dat)
#
# test <- results$Fresh_Frozen
#
# table <- data.frame( matrix(nrow = nrow(results$Fresh_Frozen$stabilityTable), ncol = 11 ) )
# table[1] <- test[[1]][1]
#
# for(i in 1:5){                    #       (ncol(results$Fresh_Frozen$stabilityTable))){
#
#
#   table[i*2] <- test[[1]][i+1]
#
#   table[i*2+1] <- test[[2]][i+1]
#
# }
#
#
#
# tablnames <- data.frame(matrix(ncol = 11, nrow = 1))
#
# tablnames[1,] <- c("Target",  "Avg. STDEV.", "Rank", "MAD", "Rank", "Stability", "Rank", "Avg.M", "Rank", "Geom. mean value", "Rank" )
#
# gtbl <- rbind(tablnames, table)
#
#
# library(gt)
#
# gt(gtbl) %>%
#   tab_header(title = md("Fresh Frozen")) %>%
#   opt_align_table_header(align = "left")
#
#
# names(table) <- tablnames
#
# ktbl <- kbl(table, align = "c", caption = "Fresh Frozen")
# ktbl
#
# ktbl <- kable_classic(ktbl)
#
# ktbl2 <- add_header_above(ktbl,c(" " = 1, "delta-Ct" = 2, "BestKeeper" = 2, "Normfinder" = 2, "geNorm" = 2, "Comprehensive Rank" = 2), bold = TRUE )
#
# ktbl2
#
# refseekerlist <-results
#
# rs_ktable2(results$Fresh_Frozen)
# rs_ktable2 <- function(refseekerlist){
#
#   require(kableExtra)
#
#   nms <- names(refseekerlist)
#   nms <- gsub("_", " ", nms)
#
#
#   refseekerlist <- refseekerlist[[1]]
#
#   table <- data.frame( matrix(nrow = nrow(refseekerlist[[1]]), ncol = 11 ) )
#   table[1] <- refseekerlist[[1]][1]
#
#   for(i in 1:5){                    #       (ncol(results$Fresh_Frozen$stabilityTable))){
#
#     table[i*2] <- refseekerlist[[1]][i+1]
#
#     table[i*2+1] <- refseekerlist[[2]][i+1]
#
#   }
#
#   names(table) <- c("Target",  "Avg. STDEV.", "Rank", "MAD", "Rank", "Stability", "Rank", "Avg.M", "Rank", "Geom. mean value", "Rank" )
#
#   ktbl <- kbl(table, align = "c", caption = nms)
#
#   ktbl <- kable_classic(ktbl)
#
#   ktbl2 <- add_header_above(ktbl,c(" " = 1, "delta-Ct" = 2, "BestKeeper" = 2, "Normfinder" = 2, "geNorm" = 2, "Comprehensive Rank" = 2), bold = TRUE )
#
#   kableExtra::as_image(ktbl2, "~/Desktop/test/tabletest.png", debug = TRUE)
#
#   save_kable(ktbl2, "~/Desktop/test/tabletest.png")
#
#   return(ktbl2)
# }
#
#
#
# rs_ktable1 <- function(refseekerlist, type = "stability"){
#
#   require(kableExtra)
#
#   #head <- unlist(base::strsplit(deparse(substitute(refseekerlist)), "[$]"))
#
#
#   head <- unlist(base::strsplit(names(refseekerlist), "[$]"))
#
#   head <- head[length(head)]
#
#   head <- gsub("_", " ", head)
#
#
#   if(type == "stability"){ tbl <- kbl(refseekerlist[[1]]$stabilityTable, align = "c", caption = paste0(head, " - Stabiltiy")) }
#   if(type == "rank"){ tbl <- kbl(refseekerlist[[1]]$rankTable, align = "c", caption = paste0(head, " - Rank")) }
#
#   return(kable_classic(tbl))
# }
#
#
#
#
# rs_ktable <- function(refseekerlist, tabletype = "stability"){
#
#
#   if(grepl("[$]", deparse(substitute(refseekerlist)))){
#
#     head <- unlist(base::strsplit(deparse(substitute(refseekerlist)), "[$]"))
#
#     head <- head[length(head)]
#
#     head <- gsub("_", " ", head)
#
#     refseekerlist <- list(refseekerlist)
#
#     names(refseekerlist) <- head
#
#   }
#
#
#   if(tabletype == "both"){
#
#     for (i in 1:length(refseekerlist)) {
#       print(rs_ktable2(refseekerlist[i]))
#     }
#
#   } else{
#
#       for (i in 1:length(refseekerlist)) {
#         print(rs_ktable1(refseekerlist[i], type = tabletype))
#       }
#
#   }
#
# }
#
# rs_ktable(results$Fresh_Frozen, tabletype = "stability")
# rs_ktable(results$Fresh_Frozen, tabletype = "rank")
# rs_ktable(results$Fresh_Frozen, tabletype = "both")
#
# rs_ktable(results, tabletype = "stability")
# rs_ktable(results, tabletype = "rank")
# rs_ktable(results, tabletype = "both")
#
#
#
#
# refseekerlist <- results$Fresh_Frozen
#
# refseekerlist <- refseekerlist
#
#
#
#
# name <- "results$Fresh_frozen$stability"
# name <- "results"
#
# grepl("[$]", name)
#
#
#
#
#
#
#
#
# results[1]
#
# unlist(base::strsplit(name, "[$]"))[-1]
#
#
# rs_ktabel(results, type = 3)
#
#
# grep("$", name, value = TRUE)
# grepl("$", name)
# regexpr("$", name)
# gregexpr("$", name)
# regexec("$", name)
# gregexec("$", name)
#
#
# grep("$", name)
# grepl("$", name)
# regexpr("$", name)
# gregexpr("$", name)
# regexec("$", name)
#
#
#
#
#
# nchar(name)
#
#
#
#
# results
#
# tbl <- rs_ktable1(results[1][1]$stabilityTable)
#
#
# tbl
#
#
#
#
#
# rs_ktable1(test$stabilityTable)
#
#
#
# rs_ktable1(results$Fresh_Frozen)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# rs_bestkeeper(dat$Fresh_Frozen)
#
#
# tbl <- kableExtra::kbl(test)
#
#
# tbl <- kableExtra::kable_classic(tbl)
#
#
# tbl <-   kableExtra::add_header_above(tbl,c(" " = 1, "Group 1" = 2, "Group 2" = 2, "Group 3" = 1))
#
# tbl <-   kableExtra::add_header_above(tbl,c("Group 1" = 1, "Group 2" = 1))
#
# tbl
#
# kableExtra::save_kable(tbl, "table2.pdf")
#
#
# "/home/patrick/Desktop/test/table.png"
# rsoutdirselect()
#
# png("table.png", width = 1024, height = 512)
#
# print(p)
#
# dev.off()
#
# kbl(test, "pipe")
#
# kable_styling()


# Cancer <- read.table("http://users.stat.ufl.edu/~aa/cat/data/Cancer.dat",
#                      header = TRUE, stringsAsFactors = TRUE)
# library(dplyr)
# library(tidyr)
# library(scales)
# library(flextable)
#
# cancerCountWide <- Cancer %>%
#   select(-risktime) %>%
#   pivot_wider(id_cols = time, names_from = c(histology, stage),
#               values_from=count) %>%
#   mutate(`histo` = " ") %>%
#   select(time, histo, `1_1`, `2_1`, `3_1`, everything())
#
#



