




#' Calculate reffinder stability tables
#' @description Uses Normfinder, geNorm, BestKeeper and delta-Ct methods to calculate ranked stabilities and the comprehensive ranking from reffinder
#' @param expression A matrix, data frame or tibble with columns for each targets and rows for each samples.
#'
#' @return A list of two data frame tables containing ranked stabilities and stability values from Normfinder, geNorm, BestKeeper and the reffinders comprehensive ranking
#' @export
#'
#' @examples
#' set.seed(100)
#' ct_vals <- matrix(rnorm(5*20, mean = 25), ncol = 5, nrow = 20)
#' dimnames(ct_vals)[[2]] <-  c("gene1", "gene2", "gene3", "gene4", "gene5")
#'
#' \dontrun{
#' rffndr <- rsfinder(ct_vals)
#' rffndr
#'
#' }

#'
#'
#' @references F Xie, P Xiao, D Chen, L Xu, B Zhang. 2012. miRDeepFinder: a miRNA analysis tool for deep sequencing of plant small RNAs. Plant molecular biology 80 (1), 75-84.
#'
#'
#'
# data("rs_miRNA")
# expression <- rs_miRNA

rsfinder <- function(expression){


  #Targetfacor <- factor(names(expression), levels = names(expression))


  expression <- as.data.frame(expression)
  #targets <- names(expression)
  targets <- factor(names(expression), levels = names(expression))

  #starttime <- Sys.time()
  DC <- rs_deltact(expression)
  #cat(paste("\nDelta-Ct completed", round(difftime(Sys.time(), starttime, units = "secs"), 3), "secs"))

  #starttime <- Sys.time()
  GN <- rs_genorm(expression)
  #cat(paste("\ngeNorm completed",  round(difftime(Sys.time(), starttime, units = "secs"), 3), "secs"))


  #starttime <- Sys.time()
  BK <- rs_bestkeeper(expression)
  #cat(paste("\nBestKeeper completed",  round(difftime(Sys.time(), starttime, units = "secs"), 3), "secs"))



  #starttime <- Sys.time()
  NF <- rs_normfinder(expression)
  #cat(paste("\nNormfinder completed",  round(difftime(Sys.time(), starttime, units = "secs"), 3), "secs\n"))


  names(DC) <- c("Target", "Stability", "Rank")
  names(BK) <- c("Target", "Stability", "Rank")
  names(NF) <- c("Target", "Stability", "Rank")
  names(GN) <- c("Target", "Stability", "Rank")
  #################################################################################

  stabilityTable <- data.frame(matrix(nrow = length(targets), ncol = 6))

  names(stabilityTable) <- c("Target", "delta-Ct", "BestKeeper", "Normfinder", "geNorm", "Comprehensive Rank")

  DC$Target <- factor(DC$Target, levels = targets)
  BK$Target <- factor(BK$Target, levels = targets)
  NF$Target <- factor(NF$Target, levels = targets)
  GN$Target <- factor(GN$Target, levels = targets)

  DC <- DC[order(DC$Target),]
  BK <- BK[order(BK$Target),]
  NF <- NF[order(NF$Target),]
  GN <- GN[order(GN$Target),]



  stabilityTable$Target <- targets

  # Order the tables based on gene position, necessary to add stability in correct order
  stabilityTable$'delta-Ct'<- DC$Stability
  stabilityTable$BestKeeper <- BK$Stability
  stabilityTable$Normfinder <- NF$Stability
  stabilityTable$geNorm <- GN$Stability

  #################################################################################

  rankTable <- data.frame(matrix(nrow = length(targets), ncol = 5))

  names(rankTable) <- c("Target", "delta-Ct", "BestKeeper", "Normfinder", "geNorm")

  rankTable$Target <- targets

  rankTable$'delta-Ct' <- DC$Rank
  rankTable$BestKeeper <- BK$Rank
  rankTable$Normfinder <- NF$Rank
  rankTable$geNorm <- GN$Rank

  for (i in 1:nrow(rankTable)){

    stabilityTable$'Comprehensive Rank'[i] <- round(exp(mean(log(unlist(rankTable[i ,2:5])))), 3)
  }


  stabilityTable <- stabilityTable[order(stabilityTable$`Comprehensive Rank`),]
  row.names(stabilityTable) <- NULL


  rankTable <- rankTable[order(rankTable$Target),]
  row.names(rankTable) <- NULL


  rankTable$'Comprehensive Rank' <- order(stabilityTable$Target)
  rankTable <- rankTable[order(rankTable$'Comprehensive Rank'),]
  row.names(rankTable) <- NULL

  return(list(stabilityTable = stabilityTable, rankTable = rankTable))

}


#' Calculate reffinder stability tables from data frame, tibble or matrix or a list of these
#'
#' @param expression A data frame, tibble or matrix or a list of these
#'
#' @return A list of two data frame tables containing ranked stabilities and stability values from Normfinder, geNorm, BestKeeper and the reffinders comprehensive ranking.
#' if a list is passed a list of lists containing the above data for each data set
#' @export
#'
#' @examples
#'
#'
#' set.seed(100)
#' ct_vals <- matrix(rnorm(5*20, mean = 25), ncol = 5, nrow = 20)
#' dimnames(ct_vals)[[2]] <-  c("gene1", "gene2", "gene3", "gene4", "gene5")
#'
#' \dontrun{
#'
#' rffndr <- rs_reffinder(ct_vals)
#' rffndr
#'
#' }

# Add example with list
#
#
rs_reffinder <- function(expression){

  message("Please note that the target names has been factorized based on the appearance in the input dataset")

  if(class(expression)[1] == "list"){

    outputData <- list()

    for (i in 1:length(expression)){
      outputData[[i]] <- rsfinder(expression[[i]])
      names(outputData)[i] <- names(expression)[i]
    }

    return(outputData)

  }



  return(rsfinder(expression))

}



