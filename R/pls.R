#' PLS regression
#' Adapted and stripped down from mixOmics v 5.2.0 (https://cran.r-project.org/web/packages/mixOmics/)
#' @param x Numeric matrix of predictors with the rows as individual observations. missing values (NAs) are allowed.
#' @param y Numeric matrix of response(s) with the rows as individual observations matching X. missing values (NAs) are allowed.
#' @param ncomp Positive Integer. The number of components to include in the model. Default to 2.
#' @param max.iter 	Integer, the maximum number of iterations. Default to 100.
#' @param tol Positive numeric used as convergence criteria/tolerance during the iterative process. Default to 1e-06.
#' @param near.zero.var near_zero_variance in MUVR
#' @param scale scale or not
#' @return a plsMUVR object
#' @examples
#' \dontrun{
#' data(freelive2)
#' MUVR2:::pls(XRVIP2,YR2)
#'
#' }
#' @noRd

pls <- function(x,
                y,
                ncomp = 2,
                max.iter = 500,
                tol = 1e-06,
                near.zero.var = TRUE,
                scale = TRUE)
{
  y <- as.matrix(y)
  x <- as.matrix(x)
  # Remove variables with near zero variance
  if (near.zero.var) {
    nzv <- MUVR2::nearZeroVar(x)
    if (length(nzv$Position > 0)) {
      warning(
        "Zero- or near-zero variance predictors.\nReset predictors matrix to not near-zero variance predictors.\nSee $nzv for problematic predictors."
      )
      x <- x[, -nzv$Position, drop = FALSE]
      ##Delete the dimensions of an array which have only one level.
      ##If drop==FALSE, x is still a column even it is one column
      if (ncol(x) == 0)
      {
        stop("No more predictors after Near Zero Var has been applied!")
      }
    }
  }
  n <- nrow(x)
  p <- ncol(x)
  q <- ncol(y)
  # Names:Give X and Y variable names
  x.names <- colnames(x)
  if (is.null(x.names)) {
    x.names <- paste("X", 1:p, sep = "")
    colnames(x) <- x.names
  }
  if (dim(y)[2] == 1) {
    y.names <- "Y"
  } else {
    y.names <- colnames(y)
    if (is.null(y.names)) {
      y.names <- paste("Y", 1:q, sep = "")
      colnames(y) <- y.names
    }
  }
  ##Give X and Y observation names
  ind.names <- rownames(x)      ###to name it as x rownames first
  if (is.null(ind.names)) {
    ###if x name is null, name it as y names
    ind.names <- rownames(y)
    rownames(x) <- ind.names
  }
  if (is.null(ind.names)) {
    ind.names <- 1:n
    rownames(x) <- rownames(y) <- ind.names
  }

  # Center and scale indata
  if (scale) {
    x.temp <- x <- scale(x, center = TRUE, scale = TRUE)
  } else {
    x.temp <- x
  }
  y.temp <- y <- scale(y, center = TRUE, scale = TRUE)
  # Allocate matrices n=nrow(X),p=nrow(Y),q=ncol(Y)
  mat.t <-
    mat.u <-
    matrix(nrow = n, ncol = ncomp)  ##row is observation,column is component
  mat.a <-
    mat.c <-
    matrix(nrow = p, ncol = ncomp)  ##row is x variable,column is component
  mat.b <-
    mat.d <-
    matrix(nrow = q, ncol = ncomp)  ##row is y variable,column is component
  # Iterate pls components h
  iter <- NULL
  for (h in 1:ncomp) {
    #-- initialisation --#
    M <-
      crossprod(x.temp, y.temp) ####x1 dim(4,5),y1 dim(4,2)  crossprod(x1,y1) dim(5,2)
    svd.M <- svd(M,
                 nu = 1, #the number of left singular vectors to be computed. This must between 0 and n = nrow(x).
                 nv = 1) #the number of right singular vectors to be computed. This must be between 0 and p = ncol(x).
    #X = U D V´
    #output d	a vector containing the singular values of x, of length min(n, p), sorted decreasingly.
    #output u a matrix whose columns contain the left singular vectors of x, present if nu > 0. Dimension c(n, nu).
    #output v a matrix whose columns contain the right singular vectors of x, present if nv > 0. Dimension c(p, nv).
    ##d dim(1,2)  when nu==nv=2 dim(1,2) This is decided by nu,y1 dimy
    ##u dim(5,1)  when nu=nv=2 dim(5,2) This is decided by x1 dimy, nu
    ##v dim(2,1)  when nu=nv=2 dim(2,2)  This is decided by y1 dimy nv
    a.old <- svd.M$u  ###initial loadings of x
    b.old <- svd.M$v  ### initial loadings of y
    #-- latent variables --#
    ##############################################################################################
    t <- x.temp %*% a.old / drop(crossprod(a.old))   ###scores
    u <- y.temp %*% b.old / drop(crossprod(b.old))
    iterh <- 1
    #-- convergence of a  --#   NIPALS algorithm
    repeat {
      a <- t(x.temp) %*% u                          ###loadings
      a <- a / drop(sqrt(crossprod(a)))
      t <- x.temp %*% a / drop(crossprod(a))
      b <- t(y.temp) %*% t
      b <- b / drop(sqrt(crossprod(b)))
      u <- y.temp %*% b / drop(crossprod(b))

      if (crossprod(a - a.old) < tol) {
        break
      }
      if (iterh == max.iter) {
        break
      }

      a.old <- a
      b.old <- b
      iterh <- iterh + 1
    }

    #-- deflation --# remove variablity already explained from Xa and Ya
    c <- crossprod(x.temp, t) / drop(crossprod(t))
    x.temp <- x.temp - t %*% t(c)

    #-- mode regression --#
    d <- crossprod(y.temp, t) / drop(crossprod(t))
    y.temp <- y.temp - t %*% t(d)

    mat.t[, h] <- t
    mat.u[, h] <- u
    mat.a[, h] <- a
    mat.b[, h] <- b
    mat.c[, h] <- c
    mat.d[, h] <- d
    iter <-
      c(iter, iterh) #save the number of iteration per component
  }
  #-- valeurs sortantes --#
  rownames(mat.a) <-
    rownames(mat.c) <- x.names    ##same as matrix u
  rownames(mat.b) <- y.names                      ##same as matrix v
  rownames(mat.t) <- rownames(mat.u) <- ind.names  ##scores
  comp <- paste("comp", 1:ncomp)
  colnames(mat.t) <- colnames(mat.u) <- comp
  colnames(mat.a) <- colnames(mat.b) <- colnames(mat.c) <- comp
  result <- list(
    X = x,
    Y = y,
    ncomp = ncomp,
    mat.c = mat.c,
    variates = list(X = mat.t, Y = mat.u),
    loadings = list(X = mat.a, Y = mat.b),
    tol = tol,
    max.iter = max.iter,
    iter = iter
  )

  if (near.zero.var == TRUE) {
    result$nzv <- nzv
  }
  class(result) <- "plsMUVR"

  return(invisible(result))   ###it is invisible if I just do pls(XRVIP,YR) there is no output

}
