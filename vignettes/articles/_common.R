## Shared setup for every article.
##
## The articles fit real models, but small ones: the tutorial's settings
## (nRep = 35, nOuter = 8) take minutes to hours, which is not something a
## website build can carry. Everything here is therefore run with a reduced
## nRep/nOuter under a fixed seed. The shape of the results is the same; the
## exact numbers are not, and where the tutorial quotes a number that depends on
## a full-scale model, the article says so.

knitr::opts_chunk$set(
  collapse = FALSE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  fig.align = "center",
  out.width = "100%",
  dpi = 96,
  warning = FALSE,
  message = FALSE
)

set.seed(42)

## Article-scale settings. Compare with the tutorial's final-model settings,
## given in the text of each article.
nRep_article <- 4
nOuter_article <- 6
varRatio_article <- 0.75
