#
#
#
# data <- rs_loadexceldata("./inst/exdata/excel-ods-test/Reffinder_data_test.xlsx")
# results <- rs_reffinder(data$Fresh_frozen)
# rs_graph(res, "OC_miRNA_stabiltiy", ordering = "Comprehensive Rank")
# rs_exceltable(res, "OC_miRNA_stabiltiy")
#
#
# refseekerlist <- results
# filename <- "testing"
#
#
#
#
# rs_graph2 <- function (refseekerlist, filename = "", filetype = "png",
#           forceSingle = FALSE, width, height, units = "px", res = 250,
#           ordering = "Comprehensive Rank", colors = "")
# {
#   if (!is.null(refseekerlist$stabilityTable)) {
#     cat("simplelist, only one dataset\n")
#     name <- deparse(substitute(refseekerlist))
#     refseekerlist <- list(refseekerlist)
#     cat("\nrefseekerlist converted\n")
#     cat(name)
#     names(refseekerlist) <- gsub("_", " ", name)
#     cat("\nrefseekerlist renamed\n")
#   }
#   if (!is.null(refseekerlist[[1]]$stabilityTable)) {
#     cat("list of lists, maybe more than one dataset\n")
#     names(refseekerlist) <- gsub("_", " ", names(refseekerlist))
#     if (forceSingle == FALSE) {
# #      if (filename != "") {
#         if (missing(width)) {
#           width = length(refseekerlist) * 675
#           cat(paste("width set to", width, "\n"))
#         }
#         if (missing(height)) {
#           height = 2156
#           cat(paste("height set to", height, "\n"))
#         }
# #      }
#       return(list(refseekerlist, filename, width = width, height = height, units = units, res = res, ordering = ordering, filetype = filetype, colors = colors))
#     }
#     else {
#       names <- names(refseekerlist)
#       if (missing(width)) {
#         width = 675
#       }
#       if (missing(height)) {
#         height = 2156
#       }
#       for (i in 1:length(names)) {
#         cat(names[i])
#         cat("\n")
#         rsgraphdraw(refseekerlist[i], paste0(filename,
#                                              "_", gsub(" ", "_", names[i])),
#                     width = width, height = height, units = "px",
#                     res = 250, ordering = ordering, filetype = filetype)
#       }
#     }
#   }
# }
#
# params <- rs_graph2(res, colors = coldict)
#
#
# refseekerlist <- params[[1]]
# filename <- params[[2]]
# width <- params$width
# height <- params$height
# colors = params$colors
# ordering = params$ordering
#
# #
# # rsgraphdraw2 <- function (refseekerlist, filename = "", filetype = "png",
# #           width, height, units = "px", res = 250, ordering = "Comprehensive Rank")
# # {
# #   if (!requireNamespace("ggplot2", quietly = TRUE)) {
# #     stop(cat("function stopped due to ggplot2"), "Package \"ggplot2\" must be installed to use this function.",
# #          call. = FALSE)
# #   }
# #   if (ordering[1] != "Targetorder") {
# #     for (i in 1:length(refseekerlist)) {
# #       algonames <- names(refseekerlist[[i]]$stabilityTable)
# #       sortcol <- which(algonames == ordering[1])
# #       refseekerlist[[i]]$stabilityTable <- refseekerlist[[i]]$stabilityTable[order(refseekerlist[[i]]$stabilityTable[,
# #                                                                                                                      sortcol]), ]
# #     }
# #   }
# #   rftable <- data.frame()
# #   if (is.null(names(refseekerlist))) {
# #     for (i in 1:length(refseekerlist)) {
# #       refseekerlist[[i]]$stabilityTable$dataID <- paste0("ID_",
# #                                                          i)
# #       rftable <- rbind(rftable, refseekerlist[[i]]$stabilityTable)
# #     }
# #   } else {
# #     for (i in 1:length(refseekerlist)) {
# #       refseekerlist[[i]]$stabilityTable$dataID <- names(refseekerlist)[i]
# #       rftable <- rbind(rftable, refseekerlist[[i]]$stabilityTable)
# #     }
# #   }
# #   algorithmlist <- names(rftable[2:6])
# #   rftable$TargetID <- paste(rftable$Target, "__", rftable$dataID,
# #                             sep = "")
# #   rftable$TargetID <- factor(rftable$TargetID, levels = rftable$TargetID)
# #   rftable <- reshape2::melt(rftable, c(names(rftable[1]), "dataID",
# #                                        "TargetID"), measure.vars = algorithmlist, variable.name = "algorithm",
# #                             value.name = "Stability")
# #   rftable$algorithm <- factor(rftable$algorithm, algorithmlist)
# #   rftable$Stability <- as.numeric(rftable$Stability)
# #   rftable <- rftable[stats::complete.cases(rftable), ]
# #   ghostframe <- rftable
# #   #ghostframe$Stability <- 3
# #
# #
# #   p <- ggplot2::ggplot(rftable, ggplot2::aes_string("TargetID", "Stability", fill = "Target")) +
# #         ggplot2::geom_bar(stat = "identity") +
# #         ggplot2::geom_point(aes_string("TargetID", "Stability"), ghostframe, alpha = 0) +
# #         ggplot2::facet_grid(algorithm ~ dataID, scales = "free") +
# #         ggplot2::scale_x_discrete(labels = function(x) {gsub("__.+$", "", x)}) +
# #         ggplot2::theme_bw() +
# #         ggplot2::theme(axis.text.x = element_text(angle = 270, vjust = 0.2, hjust = 0)) +
# #         ggplot2::ylab("Stability value") +
# #         ggplot2::xlab("Target") +
# #         #ggplot2::scale_fill_manual(values= c("green", rep("blue",5), rep("red", 4) ))
# #         ggplot2::scale_fill_manual(values=c("#2271b2",  "#d55e00", "#d55e00", "#d55e00", "#d55e00", "#d55e00", "#2271b2", "#359b73", "#2271b2", "#2271b2"))
# #
# #
# #   p
# #
# # #
# #
# #   print(p)
# #   if (filename != "") {
# #     path <- paste0(filename, "_", Sys.Date(), ".",
# #                    filetype)
# #     if (filetype == "tiff") {
# #       grDevices::tiff(path, width = width, height = height,
# #                       units = units, compression = "none", res = res)
# #     }
# #     if (filetype == "png") {
# #       grDevices::png(path, width = width, height = height,
# #                      units = units, res = res)
# #     }
# #     if (filetype == "jpeg") {
# #       grDevices::jpeg(path, width = width, height = height,
# #                       units = units, res = res)
# #     }
# #     print(p)
# #     grDevices::dev.off()
# #     cat(paste0("A ", filetype, " file was created at: ",
# #                normalizePath(path)))
# #     cat("\n")
# #   }
# #   return(p)
# # }
#
#
# #
# # cols <- c("#2271b2",  "#d55e00", "#d55e00", "#d55e00", "#d55e00", "#d55e00", "#2271b2", "#359b73", "#2271b2", "#2271b2")
# # colortable <- data.frame(Target = names(data$FFPE))
# #
# # colortable$color <- c("#2271b2", "#2271b2",  "#2271b2", "#2271b2","#359b73", "#d55e00", "#d55e00", "#d55e00", "#d55e00", "#d55e00")
# #
# # ord <- unique(rftable$Target)
# #
# #
# # colortable$Target <- factor(colortable$Target, levels = ord)
# #
# # colortable[order(colortable$Target),]
# #
# #
# # color <- colortable$color
# #
# #
# #
# # colors <- colortable
#
#
