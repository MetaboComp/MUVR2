#' Extract PLS VIP values
#' Adapted and stripped down from mixOmics v 5.2.0 (https://cran.r-project.org/web/packages/mixOmics/)
#' @param object pls(da)MUVR object
#' @return vip object
#' @export
#' @examples
#' \dontrun{
#'
#' }
vip <-
  function(object) {
    #-- initialisation des matrices --#
    W <-
      object$loadings$X  ###row is variables of X, column is components
    H <-  object$ncomp    ###number of component defined
    q <-  ncol(object$Y)  ##number of variables of Y
    p <-  ncol(object$X)  ##number of variables of X

    cor2 <-
      cor(object$Y,             ##row is observation,col is  Y variables
          object$variates$X,    ##row is observation, col is component
          use = "pairwise") ^ 2   # then the correlation or covariance between each pair of variables is computed using all complete pairs of observations on those variables.
    ##an optional character string giving a method for computing co variances in the presence of missing values.
    #This must be (an abbreviation of) one of the strings "everything", "all.obs", "complete.obs", "na.or.complete", or "pairwise.complete.obs".

    ##calculate the correlation between the columns of 2 matrix
    ##here is to calculate the correlation between each Y variable and component of X(same observations)
    ##the result is a matrix      with nrow of Y variables numbers and ncol of component

    cor2 <-
      as.matrix(cor2, nrow = q)   ### with nrow of Y variables numbers and ncol of component
    VIP <-
      matrix(0, nrow = p, ncol = H)   ###row is X variables, col is component
    VIP[, 1] <-
      W[, 1] ^ 2   ##the first component for each variable square,  length is the number of variables
    if (H > 1) {
      ##H is number of components of X
      for (h in 2:H) {
        if (q == 1) {
          ##q is number of variables of Y. When there is only one variable in Y
          Rd <-
            cor2[, 1:h]      ##with nrow of Y variables numbers and ncol of component
        } else {
          Rd <-
            colSums(cor2[, 1:h])   ##Add all row in each col together. There is one number for each component
        }
        VIP[, h] <-  Rd %*% t(W[, 1:h] ^ 2) / sum(Rd)
      }
    }
    VIP <-  sqrt(p * VIP)
    rownames(VIP) <-  rownames(W)         ###row is X variables
    colnames(VIP) <-  paste("comp", 1:H)   ###column is component
    return(invisible(VIP))
  }
