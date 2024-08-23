#' Perform matrix pre-processing
#' @param X Data matrix with samples in rows and variables in columns
#' @param offset Add offset to all data points (defaults to 0)
#' @param zeroOffset Add offset to zero data (defaults to 0)
#' @param trans Either 'log', 'sqrt' or 'none' (default is 'none')
#' @param center Either 'mean', 'none' or a numeric vector of length equal to the number of columns of X (defaults to 'none').
#' @param scale Either 'UV', 'Pareto', 'none' or a numeric vector of length equal to the number of columns of X (defaults to 'none').
#' @return A pre-processed data matrix
#' @export
#' @examples
#' \donttest{
#' data("freelive2")
#' preProcess(XRVIP2)
#' }
preProcess <- function(X,
                      offset = 0,
                      zeroOffset = 0,
                      trans = 'none',
                      center = 'none',
                      scale = 'none') {
  nVar <- ncol(X)
  # Add offset
  X[X == 0] <-
    zeroOffset          ######Add offset(a value) to zero data (defaults to 0)
  message('Zero offset:', zeroOffset)
  X <-
    X + offset                  #########Add offset(a value) to all data points
  message('\nOffset:', offset)
  # Perform transformation
  trans <- match.arg(trans, c('log', 'sqrt', 'none'))
  #match.arg(c("gauss", "rect", "ep"),
  #          c("gaussian", "epanechnikov", "rectangular", "triangular"),
  #           several.ok = TRUE)
  #[1] "gaussian"     "rectangular"  "epanechnikov"
  message('\nTransformation:', trans)
  if (trans != 'none') {
    if (trans == 'log') {
      if (any(X <= 0)) {
        stop('no zero or negative values allowed when doing log transformation')
      }
      X <- apply(X, 2, log)
    }
    if (trans == 'sqrt') {
      if (any(X < 0)) {
        stop('no negative values allowed when doing sqrt transformation')
      }
      X <- apply(X, 2, sqrt)
    }
  }


  # Perform centering and scaling
  if (length(center) != nVar) {
    center <- match.arg(center, c('mean', 'none'))
    message('\nCenter:', center)
    if (center == 'mean') {
      center <- TRUE
    } else {
      center <- FALSE
    }   ###what does this mean
    ##This is used in the scale function scale(x, center  <-  TRUE, scale  <-  TRUE)
  } else {
    message('\nCenter: By vector')
  }

  ## center: either a logical value or numeric-alike vector of length equal to the number of columns of x,
  ##  where ‘numeric-alike’ means that as.numeric(.) will be applied successfully if is.numeric(.) is not true.
  ##

  if (length(scale) != nVar) {
    scale <-
      match.arg(scale, c('UV', 'Pareto', 'none'))  ###unit variance
    message('\nScale:', scale)
    if (scale == 'UV') {
      scale <- TRUE
    }
    else if (scale == 'none') {
      scale <- FALSE
    }
    else {
      scale <-
        apply(X, 2, function(x) {
          sqrt(sd(x))
        })
    }   ####When Pareto scaling

  } else {
    message('\nScale: By vector')
  }
  ## scale: either a logical value or a numeric-alike vector of length equal to the number of columns of x.
  if (!(is.logical(scale) |
        length(scale) == nVar)) {
    stop('Error with scaling')
  }

  X <- scale(X, center, scale)
}
