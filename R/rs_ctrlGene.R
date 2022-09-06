



# #' Calculates descriptive statistics for bestKeeper
# #'
# #' This function calculates descriptive statistics of genes.
# #'
# #' @note  This function was originally published by Shanliang Zhong as part of th ctrlGene package.
# #' In this function, rounding of the rz table values has been removed for better ranking of the stabilities.
# #'
# #' This function has been deprecated please use rs_bestkeeperful() or see the ctrlGene package documentation for more functions pertaining to bestKeeper
# #'
# #' @param expression a matrix of expression levels. Each row corresponds to a sample and each column to a gene.
# #' @param ctVal a logical value indicating data type. If ct-values are input, ctVal=TRUE, otherwise, ctVal=FALSE.
# #' @return A matrix of descriptive statistics:
# #' @return N: number of samples;
# #' @return GM[CP]: the geometric mean of CP;
# #' @return AM[CP]: the arithmetic mean of CP;
# #' @return Min[CP] and Max [CP]: the extreme values of CP;
# #' @return SD[+/- CP]: the standard deviation of the CP;
# #' @return CV[CP]: the coefficient of variance expressed as a percentage on the CP level;
# #' @return Min[x-fold] and Max [x-fold]: the extreme values of expression levels expressed as an absolute x-fold over- or under-regulation coefficient;
# #' @return SD[+/- x-fold]: standard deviation of the absolute regulation coefficients.
# #'
# #' @references
# #' Pfaffl MW, Tichopad A, Prgomet C, Neuvians TP. Biotechnol Lett (2004) <doi: 10.1023/B:BILE.0000019559.84305.47>
# #'
# #'
# #' @examples
# #' set.seed(100)
# #' ct_vals <- data.frame(matrix(rnorm(5*20, mean = 25), ncol = 5, nrow = 20))
# #'
# #' names(ct_vals) <- c("gene1", "gene2", "gene3", "gene4", "gene5")
# #'
# #' rs_cpSta(ct_vals)
# #'
# #' @importFrom psych geometric.mean
# #'
# rs_cpSta <- function (expression, ctVal = TRUE)
# {
#   if (!ctVal) {
#     expression = log2(expression)
#   }
#   N = rep(nrow(expression), times = ncol(expression))
#   GM_CP = apply(expression, 2, psych::geometric.mean)
#   AM_CP = apply(expression, 2, mean)
#   Min_CP = apply(expression, 2, min)
#   Max_CP = apply(expression, 2, max)
#   AVEDEV = function(x) {
#     return(sum(abs(x - mean(x)))/length(x))
#   }
#   SD_CP = apply(expression, 2, AVEDEV)
#   CV_CP = 100 * SD_CP/AM_CP
#   Min_x_fold = -1/2^(Min_CP - GM_CP)
#   Max_x_fold = 2^(Max_CP - GM_CP)
#   SD_x_fold = 2^SD_CP
#   rz = matrix(c(N, GM_CP, AM_CP, Min_CP, Max_CP, SD_CP, CV_CP,
#                 Min_x_fold, Max_x_fold, SD_x_fold), ncol = length(GM_CP),
#               nrow = 10, byrow = T)
#   colnames(rz) = colnames(expression)
#   rownames(rz) = c("N", "GM[CP]", "AM[CP]",
#                    "Min[CP]", "Max[CP]", "SD[+/- CP]",
#                    "CV[%CP]", "Min[x-fold]", "Max[x-fold]",
#                    "SD[+/- x-fold]")
#   return(rz)
# }



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
#' rs_bestKeeperFull(as.data.frame(rs_miRNA))
#' rs_bestKeeperFull(rs_miRNA)
#'
#'
rs_bestKeeperFull=function(expression,ctVal=TRUE)
{
  if(class(expression)[1] != "data.frame"){
    expression <- as.data.frame(expression)
  }

  return(ctrlGene::bestKeeper(expression,ctVal))

}

#####################################3

# if (!ctVal) {
#   expression = log2(expression)
# }
# rz = list(CP.statistics = cpSta(expression, ctVal), pair.Wise.cor = pearsonCor(expression,
#                                                                                ctVal), HKG.vs.BestKeeper = bki(expression, ctVal))
# return(rz)
#
# expression <- as.data.frame(as.data.frame(rs_miRNA))
#
# rs_be


# test <- as.data.frame(rs_miRNA)
# class(test)
