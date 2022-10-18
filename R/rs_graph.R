

#' Wrapper for selecting single or multifaceted graph output.
#'
#' @param refseekerlist A refseekerlist list or a list of refseekerlist lists obtained frpm rs_reffinder() function
#' @param filename String. A name of the output file. Can contain an optional relative path.
#' @param forceSingle If TRUE will make a single graph for each data set provided.
#' @param filetype Selection of image file type may be "png", "tiff", "jpeg".
#' @param width Integer value parameter passed to the PNG graphics device. Width of the output png. True width is dependent on the selected units-type.
#' @param height Integer value parameter passed to the PNG graphics device. Height of the output png. True Height is dependent on the selected units-type
#' @param units Integer value parameter passed to the PNG graphics device. The units in which height and width are given. Can be "px" (pixels, the default), "in" (inches), "cm" or "mm".
#' @param res Integer value parameter passed to the PNG graphics device. The nominal resolution in ppi which will be recorded in the bitmap file, if a positive integer. Also used for units other than the default, and to convert points to pixels.
#'
#' @param ordering Used to control sorting of the x axis, use "Target","delta-Ct","BestKeeper","Normfinder","geNorm" or "Comprehensive Rank".
#' @param colors A data frame mapping target names to a hex color with targets in column 1 and colors in columns 2
#'  target1       "#2271b2"
#'  target2       "#d55e00"
#'
#' @param  orientation Selection of the orientation of the bars in the graph. May be "horizontal" or "vertical". Actually anything other than "horizontal" will be interpreted as vertical.
#' @return Creates a plot shown in the active device, usually the plots pane. Optionally outputs the plot to a file if filename has been given.
#' @export
#'
#' @examples
#' set.seed(100)
#' ct_vals <- matrix(rnorm(5*20, mean = 25), ncol = 5, nrow = 20)
#' dimnames(ct_vals)[[2]] <-  c("gene1", "gene2", "gene3", "gene4", "gene5")
#' \dontrun{
#' rffndr <- rs_reffinder(ct_vals)
#'
#' rs_graph(rffndr)
#' }
#'
#'
#' ###
#' data(tble2)
#' \dontrun{
#' rs_graph(rs_reffinder(tble2))
#' }
#'

rs_graph <- function(refseekerlist, filename = "",
                     filetype = "png",
                     forceSingle = FALSE,
                     width, height, units = "px", res = 250,
                     ordering = "Comprehensive Rank",
                     colors = "",
                     orientation = "horizontal"){
  # 2048, 2156, units = "px", res = 250

  # names <- names(refseekerlist)

  # if(filename==""){
  #   cat("No file name provided, attempting to substract name\n")
  #   filename <- deparse(substitute(refseekerlist))
  #   cat("New file name given\n")
  # }


  # refseekerlist = rfres
  # filename <- paste0(outdir, "/")
  # forceSingle = TRUE

  # Check to see if the provided list is a list of list or "just" a list (one data set).
  ## Will evaluate to TRUE if we are dealing with a simple list (one data set).
  if(!is.null(refseekerlist$stabilityTable)){
    cat("simplelist, only one dataset\n")


    # name <- deparse(substitute(refseekerlist))
    # cat(name)
    name <- deparse(substitute(refseekerlist))

    refseekerlist <- list(refseekerlist)

    cat("\nrefseekerlist converted\n")


    cat(name)
    names(refseekerlist) <- gsub("_", " ", name)


    cat("\nrefseekerlist renamed\n")


  }


  if(!is.null(refseekerlist[[1]]$stabilityTable)){

    cat("list of lists, maybe more than one dataset\n")

    names(refseekerlist) <- gsub("_", " ", names(refseekerlist))


    if (forceSingle == FALSE) {

      # if(filename != ""){
        if (missing(width)) {

          if(orientation == "horizontal"){
            width <- 2400
          } else { width = length(refseekerlist) * (nrow(refseekerlist[[1]][[1]]) * 67.5) }

          cat(paste("width set to", width, "\n"))
        }

        if (missing(height)) {

          if(orientation == "horizontal"){
            height <- length(refseekerlist) * (nrow(refseekerlist[[1]][[1]]) * 67.5) * 0.7
          } else { height = 2156 }

          cat(paste("height set to", height, "\n"))

        }
     # }

      # return(list(refseekerlist, filename,
      #             width = width,
      #             height = height,
      #             units = units,
      #             res = res,
      #             ordering = ordering,
      #             filetype = filetype,
      #             colors = colors,
      #             orientation = orientation))


      rsgraphdraw(refseekerlist, filename, width = width, height = height, units = units, res = res,  ordering = ordering, filetype = filetype, colors = colors, orientation = orientation)


    } else { # forceSingle is TRUE

      names <- names(refseekerlist)


      for (i in 1:length(names)) {


        if (missing(width)) {

          if(orientation == "horizontal") {

            width = 2156

          } else {  width = 70 * nrow( refseekerlist[[i]][[1]] ) }

        }




        if (missing(height)) {

          if(orientation == "horizontal") {

            height = 70 * nrow( refseekerlist[[i]][[1]] )

          } else { height = 2156 }
        }


        cat(names[i])
        cat("\n")


        # return(list(refseekerlist[i],
        #             paste0(filename, "_", gsub(" ", "_", names[i])),
        #             width = width,
        #             height = height,
        #             units = units,
        #             res = res,
        #             ordering = ordering,
        #             filetype = filetype,
        #             colors = colors,
        #             orientation = orientation))

        rsgraphdraw(refseekerlist[i], paste0(filename, "_", gsub(" ", "_", names[i])), width = width, height = height, units = units, res = res, ordering = ordering, filetype = filetype, colors = colors, orientation = orientation)
      }

    }

  }

}


#' Multifaceted bar graph of stability values from multiple data sets
#'
#' @param refseekerlist A list of data frames from multiple data sets generated by the reffinder function
#'
#' @param filename A file name to identify the data. Do not need file extension. Can contain a path if working directory is not the desired destination.
#' @param filetype Selection of image file type may be "png", "tiff", "jpeg".
#' @param width Parameter passed to the PNG graphics device. Width of the output png. True width is dependent on the selected units-type
#' @param height Parameter passed to the PNG graphics device. Height of the output png. True Height is dependent on the selected units-type
#' @param units Parameter passed to the PNG graphics device. The units in which height and width are given. Can be "px" (pixels, the default), "in" (inches), "cm" or "mm".
#' @param res Parameter passed to the PNG graphics device. The nominal resolution in ppi which will be recorded in the bitmap file, if a positive integer. Also used for units other than the default, and to convert points to pixels.
#' @param ordering Used to control sorting of the x axis, use "Target","Delta-Ct","BestKeeper","Normfinder","geNorm" or "Comprehensive Rank".
#' @param colors A data frame mapping target names to a hex color with targets in column 1 and colors in columns 2
#'  target1       "#2271b2"
#'  target2       "#d55e00"
#' @param  orientation Selection of the orientation of the bars in the graph. May be "horizontal" or "vertical". Actually anything other than "horizontal" will be interpreted as vertical.
#'Gene Delta Ct Bestkeeper Normfinder Genorm Comprehensive Rank
#' @return Creates a png file with the bar graph and put it in an Output folder in the working directory
#'
#' @export
#'
#' @import ggplot2
#'
#' @examples
#' set.seed(100)
#' ct_vals <- matrix(rnorm(5*20, mean = 25), ncol = 5, nrow = 20)
#' dimnames(ct_vals)[[2]] <-  c("gene1", "gene2", "gene3", "gene4", "gene5")
#' \dontrun{
#' rffndr <- rs_reffinder(ct_vals)
#'
#' rs_graphdraw(rffndr)
#' }
#'
#'
#' ###
#' data(tble2)
#' \dontrun{
#' rs_graph(rs_reffinder(tble2))
#' }

rsgraphdraw <- function(refseekerlist, filename = "", filetype = "png",
                        width, height, units = "px", res = 250, ordering = "Comprehensive Rank",
                        colors = c(""),
                        orientation = "horizontal"
  ){                    #c("#2271b2",  "#d55e00", "#d55e00", "#d55e00", "#d55e00", "#d55e00", "#2271b2", "#359b73", "#2271b2", "#2271b2")

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      cat("function stopped due to ggplot2"),
      "Package \"ggplot2\" must be installed to use this function.",
      call. = FALSE
    )
  }

  #print(refseekerlist)

  #if(ordering[1] != "Targetorder"){

    for (i in 1:length(refseekerlist)) {

      algonames <- names(refseekerlist[[i]]$stabilityTable)

      sortcol <- which(algonames == ordering[1])

      refseekerlist[[i]]$stabilityTable <- refseekerlist[[i]]$stabilityTable[order(refseekerlist[[i]]$stabilityTable[,sortcol]),]
    }

  #}




  rftable <- data.frame()
  ## Adding specific identifiers for each stability value
  if(is.null(names(refseekerlist))){ # Target names have not been provided

    # Add identifier to tables and add them to the new rftable
    ## If dataset are not named, use ID numbers
    for (i in 1:length(refseekerlist)) {
      # Add identifier
      refseekerlist[[i]]$stabilityTable$dataID <- paste0("ID_", i)

      # Add the tables to the rftable
      rftable <- rbind(rftable, refseekerlist[[i]]$stabilityTable)




    }

  } else{ # Target names have been provided
    # Add identifier to tables and add them to the new rftable
    for (i in 1:length(refseekerlist)) {
      # Add identifier
      refseekerlist[[i]]$stabilityTable$dataID <- names(refseekerlist)[i]
      # Add the tables to the rftable
      rftable <- rbind(rftable,refseekerlist[[i]]$stabilityTable)
    }

  }



  # Create a holder for the algorithms list
  algorithmlist <- c("Comprehensive Rank", "geNorm", "Normfinder", "delta-Ct", "BestKeeper")                 #   names(rftable[2:6])

  # Rename targets in each table by adding identifier (normally tissue type)
  ## necessary for arranging the graphs x-axis individually
  rftable$TargetID <- paste(rftable$Target, "__", rftable$dataID, sep = "")
  # Factorize each target based on the unique identifier
  rftable$TargetID <- factor(rftable$TargetID, levels = rftable$TargetID)

  # Melt the table
  rftable <- reshape2::melt(rftable, c(names(rftable[1]), "dataID", "TargetID"),
                            measure.vars = algorithmlist,
                            variable.name = "algorithm",
                            value.name = "Stability"
  )


  # Factorize the algorithms for ordering in the graph
  rftable$algorithm <- factor(rftable$algorithm, algorithmlist)

  rftable$Stability <- as.numeric(rftable$Stability)

  # Remove incomplete entries, should not be necessary
  rftable <- rftable[stats::complete.cases(rftable),]

  # Create ghost data frame to hold invisible values for setting the first 4 rows max value to the same value
  ghostframe <- rftable
  #ghostframe$Stability <- 3


if(orientation == "horizontal"){

  p <- ggplot2::ggplot(rftable, ggplot2::aes_string('Stability', 'TargetID', fill = "TargetID")) +
          ggplot2::geom_bar (stat = "identity") +
          # Invisible layer, alpha set to 0
          ggplot2::geom_point(aes_string('Stability', 'TargetID'), ghostframe, alpha = 0) +
          ggplot2::facet_grid(dataID ~ algorithm,  scales = "free") +
          ggplot2::scale_y_discrete(labels = function(x){gsub("__.+$", "", x)}) + # Remove prefix on target names
          ggplot2::theme_bw() +
          ggplot2::theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5)) +
          ggplot2::xlab("Stability value") +
          ggplot2::ylab("Target") +
          ggplot2::theme(legend.position="none")



} else{

    p <- ggplot2::ggplot(rftable, ggplot2::aes_string('TargetID', 'Stability', fill = "TargetID")) +
          ggplot2::geom_bar(stat = "identity") +
          # Invisible layer, alpha set to 0
          ggplot2::geom_point(aes_string('TargetID', 'Stability'), ghostframe, alpha = 0) +
          ggplot2::facet_grid(algorithm ~ dataID, scales = "free") +
          ggplot2::scale_x_discrete(labels = function(x){gsub("__.+$", "", x)}) + # Remove prefix on target names
          ggplot2::theme_bw() +
          ggplot2::theme(axis.text.x = element_text(angle = 270, vjust = 0.2, hjust=0)) +
          ggplot2::ylab("Stability value") +
          ggplot2::xlab("Target") +
          ggplot2::theme(legend.position="none")
          #ggplot2::scale_fill_manual(values=c("#000000", "#000000",  "#2271b2", "#2271b2","#359b73", "#d55e00", "#d55e00", "#d55e00", "#d55e00", "#d55e00"))

}

############################################



#############################################







  # A set of custom colors was provided as a vector
  if( length(colors) == length(unique(rftable$Target)) ){

    colors <- data.frame(target = unique(rftable$Target[order(rftable$Target)]),
                         color = colors)

    message("Please note colors has been applied to tagets based on factorization. \nThis factorization is likely provided by the rs_reffinder() function and obtained based on appearence of targets in the input data. \nIf multiple datasets are provided and a multigraph has been selected, a custom color scheme can be provided by a full lenght color vector.")
    }

  if(is.data.frame(colors)){

    #colors <- colortable

    #order <- unique(rftable$TargetID)
    temp <- rftable[rftable$algorithm == "Comprehensive Rank", ]

    temp$color <- NA

    for (i in 1:nrow(colors)) {
      for (j in 1:nrow(temp)) {
        if(  temp$Target[j] == colors$target[i] ){
          temp$color[j] = colors$color[i]
        }
      }
    }

    #names(colors) <- c("target", "color")

    #colors$target <- factor(colors$target, levels = order)

    #colors <- colors[ order(colors$target) , ]


    colors <- temp$color

  }


  # If no colors are provided the default empty string still counts as one
  if( length(colors)==1 && colors=="" ){
    p <- p + ggplot2::scale_fill_manual(values=c( rep("grey45" , length(unique(rftable$TargetID))) ))
  }

  # One color provided meaning all bars should have this color
  if(length(colors)==1 && colors !=""){
      p <- p + ggplot2::scale_fill_manual(values=c( rep(colors , length(unique(rftable$TargetID))) ))
  }

  # A set of custom colors was provided as a vector
  if( length(colors) == length(unique(rftable$TargetID)) ){
    p <- p + ggplot2::scale_fill_manual(values=colors)
  }


  print( p )

  # color = ""
  # colors = "#000000"
  if(filename != "") {

    path <- paste0(filename, "_", Sys.Date(),".", filetype)

    if(filetype == "tiff"){
      grDevices::tiff(path, width = width, height = height, units = units,  compression = "none", res = res)
    }

    if(filetype == "png"){
      grDevices::png(path, width = width, height = height, units = units, res = res)
    }

    if(filetype == "jpeg"){
      grDevices::jpeg(path, width = width, height = height, units = units, res = res)
    }

    print(p)
    grDevices::dev.off()
    cat( paste0("A ", filetype, " file was created at: ", normalizePath(path)) )
    cat("\n")

  }
  return(p)
}





