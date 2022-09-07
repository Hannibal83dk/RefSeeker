




#' Calculate reffinder stability tables
#' @description Uses Normfinder, GeNorm, bestKeeper and delta-Ct methods to calculate ranked stabilities and the comprehensive ranking from reffinder
#' @param expression A matrix, data frame or tibble with columns for each genes and rows for each samples.
#'
#' @return A list of two data frame tables containing ranked stabilities and stability values from Normfinder, GeNorm, bestKeeper and the reffinders comprehensive ranking
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



  expression <- as.data.frame(expression)
  genes <- names(expression)



  DC <- rs_deltaCt(expression)
  GN <- rs_genorm(expression)
  BK <- rs_bestKeeper(expression)
  NF <- rs_normfinder(expression)


  names(DC) <- c("Gene", "Stability", "Rank")
  names(BK) <- c("Gene", "Stability", "Rank")
  names(NF) <- c("Gene", "Stability", "Rank")
  names(GN) <- c("Gene", "Stability", "Rank")
  #################################################################################

  stabilityTable <- data.frame(matrix(nrow = length(genes), ncol = 6))

  names(stabilityTable) <- c("Gene", "Delta Ct", "Bestkeeper", "Normfinder", "Genorm", "Comprehensive Rank")

  DC$Gene <- factor(DC$Gene, levels = genes)
  BK$Gene <- factor(BK$Gene, levels = genes)
  NF$Gene <- factor(NF$Gene, levels = genes)
  GN$Gene <- factor(GN$Gene, levels = genes)

  DC <- DC[order(DC$Gene),]
  BK <- BK[order(BK$Gene),]
  NF <- NF[order(NF$Gene),]
  GN <- GN[order(GN$Gene),]



  stabilityTable$Gene <- genes

  # Order the tables based on gene position, necessary to add stability in correct order
  stabilityTable$'Delta Ct' <- DC$Stability
  stabilityTable$Bestkeeper <- BK$Stability
  stabilityTable$Normfinder <- NF$Stability
  stabilityTable$Genorm <- GN$Stability

  #################################################################################

  rankTable <- data.frame(matrix(nrow = length(genes), ncol = 5))

  names(rankTable) <- c("Gene", "Delta Ct", "Bestkeeper", "Normfinder", "Genorm")

  rankTable$Gene <- genes

  rankTable$'Delta Ct' <- DC$Rank
  rankTable$Bestkeeper <- BK$Rank
  rankTable$Normfinder <- NF$Rank
  rankTable$Genorm <- GN$Rank

  for (i in 1:nrow(rankTable)){

    stabilityTable$'Comprehensive Rank'[i] <- round(exp(mean(log(unlist(rankTable[i ,2:5])))), 3)
  }


  stabilityTable <- stabilityTable[order(stabilityTable$`Comprehensive Rank`),]
  row.names(stabilityTable) <- NULL


  rankTable <- rankTable[order(rankTable$Gene),]
  row.names(rankTable) <- NULL

  order(stabilityTable$Gene)


  rankTable$'Comprehensive Rank' <- order(stabilityTable$Gene)
  rankTable <- rankTable[order(rankTable$'Comprehensive Rank'),]
  row.names(rankTable) <- NULL

  return(list(stabilityTable = stabilityTable, rankTable = rankTable))

}


#' Calculate reffinder stability tables from data frame, tibble or matrix or a list of these
#'
#' @param expression A data frame, tibble or matrix or a list of these
#'
#' @return A list of two data frame tables containing ranked stabilities and stability values from Normfinder, GeNorm, bestKeeper and the reffinders comprehensive ranking.
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

  if(class(expression)[1] == "list"){

    outputData <- list()

    for (i in 1:length(expression)){
      outputData[[i]] <- rs_reffinder(expression[[i]])
      names(outputData)[i] <- names(expression)[i]
    }

    return(outputData)

  }

  return(rsfinder(expression))

}



