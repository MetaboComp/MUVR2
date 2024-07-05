#' PLS-DA
#' Adapted and stripped down from mixOmics v 5.2.0 (https://cran.r-project.org/web/packages/mixOmics/)
#' @param x x
#' @param y y
#' @param ncomp ncomp
#' @param max.iter  max.iter
#' @param tol tol
#' @param near.zero.var near.zero.var
#' @param scale scale
#' @return a plsdaMUVR object
#' @examples
#' \dontrun{
#' data("mosquito")
#' MUVR2:::plsda(Xotu,
#'       Yotu)
#' }
#' @noRd

plsda <- function(x,
                  y,
                  ncomp = 2,
                  max.iter = 500,
                  tol = 1e-06,
                  near.zero.var = TRUE,
                  scale = TRUE) {
  y <- as.factor(y)

  n <- length(y)
  groups <- sort(unique(y))

  levels <- levels(y)### Add levels

  ###To make sure the sequence do not change here.
  cgroups <- as.character(groups)
  groups <-
    as.numeric(factor(cgroups, levels = unique(cgroups)))  ###I also used this in one hot encoding. Actually Plsda is really similar to one hot encoding
  classification <-
    as.numeric(factor(as.character(y), levels = unique(cgroups)))

  k <- length(groups)
  nam <-
    levels(factor(cgroups, levels = unique(cgroups)))####this is the group name sequencing with alphabet order
  ind.mat <- matrix(0,
                    n,
                    k,
                    dimnames = list(1:n, nam))   ###,row is obs, column is number of groups
  ####names for rows are number, names for colnum is group levels

  for (j in 1:k)
  {
    ind.mat[classification == groups[j], j] <- 1
  }
  attr(ind.mat, "levels") <-
    levels   ###Get or set specific attributes of an object.
  # create a 2 by 5 matrix
  #x <- 1:10
  #attr(x,"dim") <- c(2, 5)

  result = pls(
    x,
    ind.mat,
    ncomp = ncomp,
    max.iter = max.iter,
    tol = tol,
    near.zero.var = near.zero.var,
    scale = scale
  )

  result$ind.mat <- ind.mat
  result$names$Y <- levels(y)   ###names saved in alphabet order
  class(result) <- "plsdaMUVR"
  return(invisible(result))   ###Return a (temporarily) invisible copy of an object.
}
