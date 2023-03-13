#' Calculates stabilites based on the BestKeeper method
#'
#' @param expression A matrix, data frame or tibble with columns for each genes and rows for each samples.
#'
#' @param decimals An integer value indicating result precision. Used for the rounding of results.
#'
#'
#' @return A data frame with stability values - std dev +/- CP
#' @export
# #' @import ctrlGene
#'
#' @examples
#' set.seed(100)
#' ct_vals <- data.frame(matrix(rnorm(5*20, mean = 25), ncol = 5, nrow = 20))
#'
#' names(ct_vals) <- c("gene1", "gene2", "gene3", "gene4", "gene5")
#' bks <- rs_bestkeeper(ct_vals)
#' bks
#'
#' @references Pfaffl MW, Tichopad A, Prgomet C, Neuvians TP. 2004. Determination of stable housekeeping genes, differentially regulated target genes and sample integrity: BestKeeper--Excel-based tool using pair-wise correlations. Biotechnology letters 26:509-515.
#'
rs_bestkeeper <- function(expression, decimals = 3){

  # Make sure the input is in the form of a data frame
  input <- as.data.frame(expression)

  # Get the gene names from the column names
  genes <- names(input)
  # Create a new data frame for the stability values
  bk <- data.frame(matrix(nrow = length(genes), ncol = 3))

  # Set names of the three columns
  names(bk) <- c("Target", "MAD", "Stability Rank")

  # Write targets to bestkeeper table
  bk$Target <- genes

  # Calculate the mean absolute deviation
  for (i in 1:ncol(input)) {
    # Go through each input data column and save results in bestkeeper table
    bk[i,2] <- mean(abs(input[,i]-mean(input[,i])))
  }

  bk <- rsorderbystability(bk)

  bk$MAD <- round(bk$MAD, digits = decimals)

  # Write the ranking column
  bk <- rsaddstabilityrank(bk, 2)



  return(bk)

}




####################################################################################


#' Determines stability of genes
#'
#' This function is a wrapper calling the bestKeeper function from the ctrlGene package which combines the results of rs_cpSta(), pearsonCor() and bki().
#'
#' @note This function was originally published by Shanliang Zhong as part of th ctrlGene package.
#'
#'
#' @param expression A matrix of expression levels. Each row corresponds to a sample and each column to a gene.
#' @param ctVal A logical value indicating data type. If ct-values are input, ctVal=TRUE, otherwise, ctVal=FALSE.
#' @return A list containing CP.statistics, pair.Wise.cor and HKG.vs.BestKeeper, which are returned by cpSta(), pearsonCor() and bki(), respectively.
#' @export
#' @references
#' Pfaffl MW, Tichopad A, Prgomet C, Neuvians TP. Biotechnol Lett (2004) <doi: 10.1023/B:BILE.0000019559.84305.47>
#' @examples
#'
#' data(rs_miRNA)
#'
#'
#' rs_bestkeeperFull(as.data.frame(rs_miRNA))
#' rs_bestkeeperFull(rs_miRNA)
#'
#'
rs_bestkeeperFull=function(expression,ctVal=TRUE)
{
  if(class(expression)[1] != "data.frame"){
    expression <- as.data.frame(expression)
  }

  return(ctrlGene::bestKeeper(expression,ctVal))

}


####################################################################################




#' Calculates expression stabilities by the Delta-Ct method
#'
#' @param expression A matrix, data frame or tibble with columns for each genes and rows for each samples.
#'
#' @return Data frame with mean standard deviations for pairwise delta Ct values
#' @export
#'
#' @examples
#' set.seed(100)
#' ct_vals <- matrix(rnorm(5*20, mean = 25), ncol = 5, nrow = 20)
#' dimnames(ct_vals)[[2]] <-  c("gene1", "gene2", "gene3", "gene4", "gene5")
#'
#' dct <- rs_deltact(ct_vals)
#' dct
#'
#'########################
#'
#' set.seed(100)
#' ct_vals <- data.frame(matrix(rnorm(5*20, mean = 25), ncol = 5, nrow = 20))
#'
#' names(ct_vals) <- c("gene1", "gene2", "gene3", "gene4", "gene5")
#' dct <- rs_deltact(ct_vals)
#' dct
#'
#' @references Silver N, Best S, Jiang J, Thein SL. 2006. Selection of housekeeping genes for gene expression studies in human reticulocytes using real-time PCR. BMC molecular biology 7:33.
#'
#'
rs_deltact <- function(expression){

  # Convert to matrix
  expression <- as.matrix(expression)

  # Get the gene names for later use
  genes <- dimnames(expression)[[2]]

  # Create a square data frame to hold all combinations of standard deviations.
  sddf <- data.frame(matrix(nrow = ncol(expression), ncol = ncol(expression)))
  row.names(sddf) <- genes
  names(sddf) <- genes

  # Calculate standard deviations of all combinations of deltaCt and save them in the data frame
  for (i in 1:(ncol(expression)-1)){
    for (j in (i+1):ncol(expression)){
      sddf[j,i] = stats::sd( expression[,i] - expression[,j] )
    }
  }

  # Create a new data frame for the mean standard deviations for each gene
  DC_tbl <- data.frame(matrix(ncol = 3, nrow = ncol(expression)))
  names(DC_tbl) <- c("Target", "Average of STDEV", "Stability Rank")
  DC_tbl$Target <- genes

  # Calculate the mean standard deviations for each gene, save it in the data frame
  for (i in 1:nrow(sddf)){
    # mean is calculated by combining row and column for each gene in a vector.
    DC_tbl[i,2] <- round(mean( unlist(  c(sddf[i, ] , sddf[, i] )   ), na.rm = TRUE), 3)

  }

  DC_tbl <- rsaddstabilityrank(DC_tbl, 2)
  # DC_tbl <- rsorderbystability(DC_tbl)
  #
  # DC_tbl$`Stability Rank` <- order(DC_tbl$`Average of STDEV`)


  return(DC_tbl)

}


####################################################################################


#' Gene expression stability by geNorm
#'
#'
#'
#' @param expression A matrix, data frame or tibble with columns for each genes and rows for each samples.
#' @param decimals An integer value indicating result precision. Used for the rounding of results.
#' @return Data frame with average pairwise variation for each gene
#' @export
#'
#' @examples
#' set.seed(100)
#' ct_vals <- matrix(rnorm(5*20), ncol = 5, nrow = 20)
#' dimnames(ct_vals)[[2]] <-  c("gene1", "gene2", "gene3", "gene4", "gene5")
#'
#' gnm <- rs_genorm(ct_vals)
#' gnm
#'
#'########################
#'
#' set.seed(100)
#' ct_vals <- data.frame(matrix(rnorm(5*20, mean = 25), ncol = 5, nrow = 20))
#'
#' names(ct_vals) <- c("gene1", "gene2", "gene3", "gene4", "gene5")
#' gnm <- rs_genorm(ct_vals)
#' gnm
#'
#'
#' @references Vandesompele J, De Preter K, Pattyn F, Poppe B, Van Roy N, De Paepe A, Speleman F. 2002. Accurate normalization of real-time quantitative RT-PCR data by geometric averaging of multiple internal control genes. Genome biology 3:RESEARCH0034.
#'
rs_genorm <- function(expression, decimals = 3){


  #gnex <- as.data.frame(expression)
  gnrm <- ctrlGene::geNorm2(expression)
  # gnrm$Avg.M <- round(gnrm$Avg.M, 3)


  gnrm$Avg.M[nrow(gnrm)] <- gnrm$Avg.M[nrow(gnrm)-1]

  names(gnrm)[1] <- "Target"

  #gnrm <- rsorderbystability(gnrm)

  gnrm$Avg.M <- round(gnrm$Avg.M, digits = decimals)

  gnrm <- rsaddstabilityrank(gnrm, 2)



  return(gnrm)

}


####################################################################################

#' Gene expresssion stabilities by NormFinder
#'
#' @param expression A matrix, data frame or tibble with columns for each genes and rows for each samples.
#'
#' @return Data frame with stability values based on the Normfinder method
#' @export
#'
# #' @source "https://moma.dk/files/r.NormOldStab5.txt"
#' @examples
#' set.seed(100)
#' ct_vals <- matrix(rnorm(5*20, mean = 25), ncol = 5, nrow = 20)
#' dimnames(ct_vals)[[2]] <-  c("gene1", "gene2", "gene3", "gene4", "gene5")
#' \dontrun{
#'
#' nrmfndr <- rs_normfinder(ct_vals)
#' nrmfndr
#'
#' }

#'
#'########################
#'
#' set.seed(100)
#' ct_vals <- data.frame(matrix(rnorm(5*20, mean = 25), ncol = 5, nrow = 20))
#'
#' names(ct_vals) <- c("gene1", "gene2", "gene3", "gene4", "gene5")
#'
#' \dontrun{
#' nrmfndr <- rs_normfinder(ct_vals)
#' nrmfndr
#' }
#'
#'
#'
#' @references Andersen C.L., Ledet-Jensen J., Ørntoft T.: Normalization of real-time quantitative RT-PCR data: a model based variance estimation approach to identify genes suited for normalization - applied to bladder- and colon-cancer data-sets.
#' Cancer Research. 2004 (64): 5245-5250
#'
#'
#'
#'


rs_normfinder <- function(expression){

  # Look for the exdata folder check if r.NormOldStab5.txt exists in this folder, if not download it to that location

  if(!file.exists(
    paste0(dir(find.package("RefSeeker", lib.loc=NULL, quiet = TRUE), pattern = "exdata", recursive = TRUE, full.names = TRUE, include.dirs = TRUE), "/r.NormOldStab5.txt")

  )){

    utils::download.file("https://moma.dk/files/r.NormOldStab5.txt",
                         paste0(dir(find.package("RefSeeker", lib.loc=NULL, quiet = TRUE), pattern = "exdata", recursive = TRUE, full.names = TRUE, include.dirs = TRUE), "/r.NormOldStab5.txt"),
                         quiet = FALSE)

    rsadjustnmfRounding(decimals = 3)
  }
  # source the r.NormOldStab5.txt
  source(paste0(dir(find.package("RefSeeker", lib.loc=NULL, quiet = TRUE), pattern = "exdata", recursive = TRUE, full.names = TRUE, include.dirs = TRUE), "/r.NormOldStab5.txt"))


  ###################################################################################################################

  # write the temporary txt file table for the normfinderfuncton
  utils::write.table(data.frame(t(expression)), "expression_temp.txt")

  # Calculate the stabilities and save in Results
  Results = Normfinder("expression_temp.txt", Groups = FALSE)

  # Remove the temporary txt file
  unlink("expression_temp.txt")


  # show the results of the analysis based on individual targets
  nrmfndr <- data.frame(Target = row.names(Results$Ordered), Stability = Results$Ordered$GroupSD)

  nrmfndr <- rsorderbystability(nrmfndr)

  nrmfndr <- rsaddstabilityrank(nrmfndr, 2)

  return(nrmfndr)

}




#' Adjust rounding in the normfinder algorithm to 3 decimals
#'
#' @param decimals An integer value indicating desired precision rounding of the Normfinder results
#' @return Void
#' @export
#'
#'
rsadjustnmfRounding <- function(decimals = 3) {

  decimals <- as.character(decimals)
  tx <- readLines(paste0(dir(find.package("RefSeeker", lib.loc=NULL, quiet = TRUE), pattern = "exdata", recursive = TRUE, full.names = TRUE, include.dirs = TRUE), "/r.NormOldStab5.txt"))

  substring(tx[193], 41, 41) <- decimals

  substring(tx[193], 71, 71) <- decimals

  substring(tx[194], 32, 32) <- decimals

  substring(tx[196], 38, 38) <- decimals

  substring(tx[196], 65, 65) <- decimals

  substring(tx[197], 28, 28) <- decimals

  substring(tx[198], 45, 45) <- decimals

  substring(tx[199], 48, 48) <- decimals

  substring(tx[203], 28, 28) <- decimals

  substring(tx[229], 40, 40) <- decimals

  substring(tx[232], 26, 26) <- decimals

  substring(tx[236], 40, 40) <- decimals

  substring(tx[196], 38, 38) <- decimals

  writeLines(tx, con = paste0(dir(find.package("RefSeeker", lib.loc=NULL, quiet = TRUE), pattern = "exdata", recursive = TRUE, full.names = TRUE, include.dirs = TRUE), "/r.NormOldStab5.txt"))

}




#' Title
#'
#' @param expression A matrix, data frame or tibble with columns for each genes and rows for each samples.
#' @param Groups Does the data consist of different treatment groups, if so indicate group ID in the last column of the expression data set
#' @param ctVal Is the provided expression data ca values or relative log(2) transformed values?
#' @param pStabLim Stability limit, report only stabilities above this value
#'
#' @return A list of lists of result tables depending on the Groups parameter
#' @export
#'
#'
#' @references Andersen C.L., Ledet-Jensen J., Ørntoft T.: Normalization of real-time quantitative RT-PCR data: a model based variance estimation approach to identify genes suited for normalization - applied to bladder- and colon-cancer data-sets.
#' Cancer Research. 2004 (64): 5245-5250
#'
# #' @examples
rs_normfinderFull <- function(expression, Groups=TRUE, ctVal=TRUE, pStabLim=0.25){
  if(!file.exists(
    paste0(dir(find.package("RefSeeker", lib.loc=NULL, quiet = TRUE), pattern = "exdata", recursive = TRUE, full.names = TRUE, include.dirs = TRUE), "/r.NormOldStab5.txt")

  )){

    utils::download.file("https://moma.dk/files/r.NormOldStab5.txt",
                         paste0(dir(find.package("RefSeeker", lib.loc=NULL, quiet = TRUE), pattern = "exdata", recursive = TRUE, full.names = TRUE, include.dirs = TRUE), "/r.NormOldStab5.txt"),
                         quiet = FALSE)

    rsadjustnmfRounding(decimals = 3)
  }
  # source the r.NormOldStab5.txt
  source(paste0(dir(find.package("RefSeeker", lib.loc=NULL, quiet = TRUE), pattern = "exdata", recursive = TRUE, full.names = TRUE, include.dirs = TRUE), "/r.NormOldStab5.txt"))

  ###################################################################################################################

  # write the temporary txt file table for the normfinderfuncton
  utils::write.table(data.frame(t(expression)), "expression_temp.txt")

  # Calculate the stabilities and save in Results
  Results = Normfinder("expression_temp.txt", Groups = Groups, ctVal = ctVal, pStabLim = pStabLim)

  # Remove the temporary txt file
  unlink("expression_temp.txt")

  return(Results)
}




####################################################################################


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

  message("Please note that the target names has been factorized based on the appearance in the input dataset. \nThis effects sorting of the target column and target sorting during graph creation if sorting by target is chosen.")

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

