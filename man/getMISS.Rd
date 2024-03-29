% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getMISS.R
\name{getMISS}
\alias{getMISS}
\title{get the number of miss classifications from classification analysis}
\usage{
getMISS(actual, predicted, weigh_added = FALSE, weighing_matrix)
}
\arguments{
\item{actual}{Vector of actual classifications of samples}

\item{predicted}{Vector of predicted classifications of samples}

\item{weigh_added}{To add a weighing matrix when it is classfication}

\item{weighing_matrix}{The matrix used for get a miss classfication score}
}
\value{
miss classification
}
\description{
get the number of miss classifications from classification analysis
}
