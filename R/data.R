
utils::globalVariables(".")

#' Effect matrix for the crisp multilevel tutorial
#'
#' Untargeted plasma metabolomics from a crossover dietary intervention: 21
#' subjects each received two different breakfast meals. The data is supplied as
#' an *effect matrix*, i.e. the within-individual difference in feature
#' area-under-the-curve between the two meals, which is the form multilevel
#' analysis needs (see `vignette("multilevel")`).
#'
#' @format A numeric matrix with 21 rows (one per individual) and 1508 columns
#'   (metabolic features).
#' @source Hanhineva K. et al., as used in the MUVR2 tutorial.
#' @seealso [MUVR2()] with `ML = TRUE`
#' @name crispEM
#' @docType data
#' @keywords data
#' @usage data(crisp)
NULL


#' Subject identifiers for the rye metabolomics regression tutorial
#'
#' Identifies which individual each of the 112 samples in [XRVIP] came from. Some
#' individuals contributed more than one sample, which is what makes this dataset
#' the one to use when demonstrating the `ID` argument.
#'
#' @format A numeric vector of length 112.
#' @seealso [XRVIP], [YR], [IDR2]
#' @name IDR
#' @docType data
#' @keywords data
#' @usage data(freelive)
NULL

#' Metabolomics data for the rye metabolomics regression tutorial
#'
#' Urine metabolomics features for 112 samples from 58 individuals, with repeated
#' samples per individual.
#'
#' @format A numeric matrix with 112 rows (samples) and 1147 columns (metabolomics
#'   features).
#' @source Hanhineva K. et al. (2015).
#' @seealso [YR], [IDR], and [XRVIP2] for the one-sample-per-individual version
#' @name XRVIP
#' @docType data
#' @keywords data
#' @usage data(freelive)
NULL

#' Rye consumption for the rye metabolomics regression tutorial
#'
#' Wholegrain rye intake for the 112 samples in [XRVIP].
#'
#' @format A numeric vector of length 112.
#' @seealso [XRVIP], [IDR]
#' @name YR
#' @docType data
#' @keywords data
#' @usage data(freelive)
NULL

#' Subject identifiers for the rye metabolomics regression tutorial, using unique individuals
#'
#' @format A numeric vector of length 58, one entry per individual.
#' @seealso [XRVIP2], [YR2]
#' @name IDR2
#' @docType data
#' @keywords data
#' @usage data(freelive2)
NULL

#' Metabolomics data for the rye metabolomics regression tutorial, using unique individuals
#'
#' Urine metabolomics features for 58 individuals, one sample each. This is the
#' predictor matrix used in most of the regression examples.
#'
#' @format A numeric matrix with 58 rows (individuals) and 1147 columns
#'   (metabolomics features).
#' @source Hanhineva K. et al. (2015).
#' @seealso [YR2], [IDR2], and [XRVIP] for the repeated-samples version
#' @name XRVIP2
#' @docType data
#' @keywords data
#' @usage data(freelive2)
NULL

#' Rye consumption for the rye metabolomics regression tutorial, using unique individuals
#'
#' Wholegrain rye intake for the 58 individuals in [XRVIP2]. This is the continuous
#' target variable of the regression examples.
#'
#' @format A numeric vector of length 58.
#' @seealso [XRVIP2], [IDR2]
#' @name YR2
#' @docType data
#' @keywords data
#' @usage data(freelive2)
NULL


#' Microbiota composition in mosquitos for the classification tutorial
#'
#' 16S rRNA operational taxonomic unit (OTU) counts for 29 *Anopheles gambiae*
#' mosquitoes caught in three villages in western Burkina Faso.
#'
#' @format A numeric matrix with 29 rows (mosquitoes) and 1678 columns (OTUs).
#' @source Buck M., Nilsson L.K., Brunius C., Dabiré R.K., Hopkins R., Terenius O.
#'   (2016). Bacterial associations reveal spatial population dynamics in Anopheles
#'   gambiae mosquitoes. *Scientific Reports*, 6(1), 1-9.
#' @seealso [Yotu]
#' @name Xotu
#' @docType data
#' @keywords data
#' @usage data(mosquito)
NULL

#' Village of capture of mosquitos for the classification tutorial
#'
#' The village each of the 29 mosquitoes in [Xotu] was caught in. This is the
#' categorical target variable of the classification examples. Class sizes are 10,
#' 11 and 8, which is why `nOuter` should not exceed 8 for this data.
#'
#' @format A factor of length 29 with three levels: VK3, VK5, VK7.
#' @source Buck M. et al. (2016). *Scientific Reports*, 6(1), 1-9.
#' @seealso [Xotu]
#' @name Yotu
#' @docType data
#' @keywords data
#' @usage data(mosquito)
NULL
