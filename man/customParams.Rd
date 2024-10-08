% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/customParams.R
\name{customParams}
\alias{customParams}
\title{Make custom parameters for internal modelling}
\usage{
customParams(
  method = c("RF", "PLS", "SVM", "ANN"),
  robust = 0.05,
  ntreeIn = 150,
  ntreeOut = 300,
  mtryMaxIn = 150,
  compMax = 5,
  nodes = 200,
  threshold = 0.1,
  stepmax = 1e+08,
  neuralMaxIn = 10,
  kernel = "notkernel",
  nu = 0.1,
  gamma = 1,
  degree = 1,
  oneHot,
  NZV,
  rfMethod = c("randomForest", "ranger"),
  svmMethod = c("svm", "ksvm", "svmlight"),
  annMethod = c("nnet", "neuralnet")
)
}
\arguments{
\item{method}{PLS or RF (default)}

\item{robust}{Robustness (slack) criterion for determining min and max knees (defaults to 0.05)}

\item{ntreeIn}{RF parameter: Number of trees in inner cross-validation loop models (defaults to 150)}

\item{ntreeOut}{RF parameter: Number of trees in outer (consensus) cross-validation loop models (defaults to 300)}

\item{mtryMaxIn}{RF parameter: Max number of variables to sample from at each node in the inner CV loop (defaults to 150). Will be further limited by standard RF rules (see randomForest documentation)}

\item{compMax}{PLS parameter: Maximum number of PLS components (defaults to 5)}

\item{nodes}{ann parameter:}

\item{threshold}{ann parameter:}

\item{stepmax}{ann parameter:}

\item{neuralMaxIn}{ann parameter: Maximum number of ANN (defaults to 20)}

\item{kernel}{svm parameter: kernal function to use, which includes sigmoid, radical, polynomial}

\item{nu}{svm parameter: ratios of errors allowed in the training set range from 0-1}

\item{gamma}{svm parameters: needed for "vanilladot","polydot","rbfdot" kernel in svm}

\item{degree}{svm parameter: needed for polynomial kernel in svm}

\item{oneHot}{TRUE or FALSE using onehot endcoding or not}

\item{NZV}{TRUE or FALSE using non-zero variance or not}

\item{rfMethod}{randomforest method, which includes randomForest and ranger}

\item{svmMethod}{support vector machine method, which includes svm, ksvm, s}

\item{annMethod}{artificial neural network method which includes 2 different ann methods}
}
\value{
a `methParam` object
}
\description{
Make custom parameters for MUVR internal modelling, not rdCV. Please note that, at present, there is no mtryMax for the outer (consensus) loop in effect.
}
\examples{
# Standard parameters for random forest
methParam <- customParams() # or
methParam <- customParams('RF')
# Custom ntreeOut parameters for random forest
methParam <- customParams('RF',ntreeOut=50) # or
methParam <- customParams('RF')
methParam$ntreeOut <- 50
methParam
}
