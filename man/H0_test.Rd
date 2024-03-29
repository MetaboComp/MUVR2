% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/H0_test.R
\name{H0_test}
\alias{H0_test}
\title{Make permutation or resampling with data and default settings from an actual MUVR object
This function will extract data and parameter settings from a MUVR object and run standard permutation or resampling test.
This will fit a standard case of multivariate predictive modelling in either a regression, classification or multilevel case.
However, if an analysis has a complex sample dependency which requires constrained permutation of your response vector
or if a variable pre-selection is performed for decreased computational burden, then permutaion/resampling loops should be constructed manually.
In those cases, View(H0_test) can be a first start from which to build custom solutions for permutation analysis.}
\usage{
H0_test(
  MUVRclassObject,
  n = 50,
  nRep,
  nOuter,
  varRatio,
  parallel,
  type = c("resampling", "permutation")
)
}
\arguments{
\item{MUVRclassObject}{A 'MUVR' class object}

\item{n}{number of permutations to run}

\item{nRep}{number of repetitions for each permutation (defaults to value of actual model)}

\item{nOuter}{number of outer validation segments for each permutation (defaults to value of actual model)}

\item{varRatio}{varRatio for each permutation (defaults to value of actual model)}

\item{parallel}{whether to run calculations using parallel processing - which requires registered backend (defaults to value of actual model)}

\item{type}{Either permutation or resampling, To decide the permutation sampling is performed on original Y values or the probability(If Y categorical)/distributions(If Y continuous) of Y values}
}
\value{
permutation_output: A permutation matrix with permuted fitness statistics (nrow=n and ncol=3 for min/mid/max)
}
\description{
Make permutation or resampling with data and default settings from an actual MUVR object
This function will extract data and parameter settings from a MUVR object and run standard permutation or resampling test.
This will fit a standard case of multivariate predictive modelling in either a regression, classification or multilevel case.
However, if an analysis has a complex sample dependency which requires constrained permutation of your response vector
or if a variable pre-selection is performed for decreased computational burden, then permutaion/resampling loops should be constructed manually.
In those cases, View(H0_test) can be a first start from which to build custom solutions for permutation analysis.
}
