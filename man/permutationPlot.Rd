% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/permutationPlot.R
\name{permutationPlot}
\alias{permutationPlot}
\title{Plot permutation analysis}
\usage{
permutationPlot(
  MUVRclassObject,
  permutation_result,
  model = "Mid",
  type = "t",
  side = c("greater", "smaller"),
  pos,
  xlab = NULL,
  xlim,
  ylim = NULL,
  breaks = "Sturges",
  main = NULL
)
}
\arguments{
\item{MUVRclassObject}{A 'MUVR' class object}

\item{permutation_result}{A permutation result. It is a list of 1 items:  permutation_output}

\item{model}{'Min', 'Mid', or 'Max'}

\item{type}{'t' (default; for Student's t) or 'non' for "non-parametric" (i.e. rank) studen'ts}

\item{side}{'smaller' for actual lower than H0 or 'greater' for actual larger than H0 (automatically selected if not specified)}

\item{pos}{which side of actual to put p-value on}

\item{xlab}{optional xlabel}

\item{xlim}{optional x-range}

\item{ylim}{otional y-range}

\item{breaks}{optional custom histogram breaks (defaults to 'sturges')}

\item{main}{optional plot title (or TRUE for autoname)}
}
\value{
A permutation plot
}
\description{
Plot permutation analysis using actual model and permutation result. This is basically a wrapper for the MUVR2::plotPerm() function using model objects to make coding nicer and cleaner.
}
\examples{
\donttest{
data("freelive2")
nRep <- 2
nOuter <- 4
varRatio <-0.6
regrModel <- MUVR2(X = XRVIP2,
                   Y = YR2,
                   nRep = nRep,
                  nOuter = nOuter,
                   varRatio = varRatio,
                   method = "PLS",
                   modReturn = TRUE)
permutation_result<-H0_test(regrModel,n=10)
permutationPlot(regrModel,permutation_result)
}
}
