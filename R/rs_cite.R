# Printing citations:

# install.packages("RefManageR")
#
# library(RefManageR)
#
# RefFinder <- GetBibEntryWithDOI("10.1007/s11103-012-9885-2")
# deltaCt <- GetBibEntryWithDOI("10.1186/1471-2199-7-33")
# geNorm <- GetPubMedByID("12184808")
# BestKeeper <- GetPubMedByID("15127793")
# Normfinder <- GetPubMedByID("15289330")

#' Cite to cite when using this package.
#'
#' @return No return value
#' @export
#'
#' @examples
#' rs_cite()
#'
#'
rs_cite <- function(){

  message("
        geNorm:
        J. Vandesompele, K. De Preter, F. Pattyn, et al. “Accurate normalization of real-time quantitative RT-PCR data by geometric
        averaging of multiple internal control genes”. Eng. In: _Genome biology_ 3.7 (Jun. 2002), p. RESEARCH0034. ISSN: 1474-760X. DOI:
        10.1186/gb-2002-3-7-research0034. PMID: 12184808.

        BestKeeper:
        M. W. Pfaffl, A. Tichopad, C. Prgomet, et al. “Determination of stable housekeeping genes, differentially regulated target genes and
        sample integrity: BestKeeper-Excel-based tool using pair-wise correlations”. Eng. In: _Biotechnology letters_ 26.6 (Mar. 2004), pp.
        509-15. ISSN: 0141-5492. DOI: 10.1023/b:bile.0000019559.84305.47. PMID: 15127793.

        Normfinder:
        C. L. Andersen, J. L. Jensen, and T. F. Ørntoft. “Normalization of real-time quantitative reverse transcription-PCR data: a
        model-based variance estimation approach to identify genes suited for normalization, applied to bladder and colon cancer data sets”.
        Eng. In: _Cancer research_ 64.15 (Aug. 2004), pp. 5245-50. ISSN: 0008-5472. DOI: 10.1158/0008-5472.CAN-04-0496. PMID: 15289330.

        delta-Ct:
        N. Silver, S. Best, J. Jiang, et al. “Selection of housekeeping genes for gene expression studies in human reticulocytes using
        real-time PCR”. In: _BMC Molecular Biology_ 7.1 (Oct. 2006). DOI: 10.1186/1471-2199-7-33. <https://doi.org/10.1186/1471-2199-7-33>.

        RefFinder:
        F. Xie, P. Xiao, D. Chen, et al. “miRDeepFinder: a miRNA analysis tool for deep sequencing of plant small RNAs”. In: _Plant
        Molecular Biology_ 80.1 (Jan. 2012), pp. 75-84. DOI: 10.1007/s11103-012-9885-2. <https://doi.org/10.1007/s11103-012-9885-2>.)"
  )

}




