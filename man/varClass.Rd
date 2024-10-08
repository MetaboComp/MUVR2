% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/varClass.R
\name{varClass}
\alias{varClass}
\title{Report variables belonging to different classes}
\usage{
varClass(MUVRclassObject)
}
\arguments{
\item{MUVRclassObject}{A MUVR class object}
}
\value{
A list with names and numbers of variables: all as well as optimal (Corresponding to 'min' or minial-optimal model),
redundant (from 'min' up to 'max' or all-relevant ) and noisy (the rest)
}
\description{
Reports names and numbers of variables: all as well as optimal (min model), redundant (from min up to max) and noisy (the rest).
}
\examples{
\donttest{
data("mosquito")
nRep <- 2
nOuter <- 4
classModel <- MUVR2_EN(X = Xotu,
                       Y = Yotu,
                       nRep = nRep,
                       nOuter = nOuter,
                       DA = TRUE,
                       modReturn = TRUE)
classModel<-getVar(classModel,option="quantile")
varClass(classModel)
}

}
