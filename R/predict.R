#' Predict pls
#' Adapted and stripped down from mixOmics v 5.2.0 (https://cran.r-project.org/web/packages/mixOmics/)
#' @param object a plsMUVR object
#' @param newdata new data
#' @param onlyPred Boolean for whether to report back predictions only (defaults to FALSE)
#' @param scale scale in MUVR
#' @param ... other argument
#' @return pls prediction
#' @export
#'
predict.plsMUVR <- function(object,
                            newdata,
                            onlyPred = FALSE,
                            scale = TRUE,
                            ...) {
  #-- validation des arguments --#
  if (missing(newdata)) {
    stop("No new data available.")
  }

  x <- object$X
  y <- object$Y
  q <- ncol(y)
  p <- ncol(x)

  if (ncol(x) != ncol(newdata) & length(object$nzv$Position) > 0) {
    newdata <- newdata[, -object$nzv$Position]
  }
  ###If variable number is not the same or there is near zero variance in the original x
  ###
  if (length(dim(newdata)) == 2) {
    if (ncol(newdata) != p)
      ###col numbers are not the same
    {
      stop("'newdata' must be a numeric matrix with ncol = ",
           p,
           " or a vector of length = ",
           p,
           ".")
    }
    if (!identical(colnames(x), colnames(newdata)))
      ###col names are not the same
    {
      stop("Mismatch between columns in model and newdata.")
    }
  }

  ###############################################################################################################################

  if (length(dim(newdata)) == 0) {
    ###when there
    if (length(newdata) != p)
    {
      stop("'newdata' must be a numeric matrix with ncol = ",
           p,
           " or a vector of length = ",
           p,
           ".")
    }
    dim(newdata) <-
      c(1, p)                       ##change the vector to matrix
  }
  ###########################################################################################################
  #-- initialisation des matrices --#
  ncomp <- object$ncomp
  a <-
    object$loadings$X                ## row is variables, column is component
  b <- object$loadings$Y
  c <-
    object$mat.c                                   ##########################

  newdata <- as.matrix(newdata)

  if (scale == TRUE) {
    means.x <- attr(x, "scaled:center")
    sigma.x <- attr(x, "scaled:scale")
    newdata <- scale(newdata,
                     center = means.x,
                     scale = sigma.x)
  }

  means.y <- attr(y, "scaled:center")
  sigma.y <- attr(y, "scaled:scale")

  ##- coeff de regression
  B.hat <- array(0, dim = c(p, q, ncomp))    ##ncoly,ncolx,ncomp
  ##- prediction
  y.hat <-
    y.hat2 <-
    array(0, dim = c(nrow(newdata), q, ncomp)) ###newdata obsevations,ncolx,ncomp

  #############################################################################################################
  ##- variates
  t.pred <- array(0, dim = c(nrow(newdata), ncomp))
  ##the dim attribute for the array to be created, that is an integer vector of length one or
  ##more giving the maximal indices in each dimension.

  ##############################################################################################################
  variates.x <-
    object$variates$X     ###row is observations, col is components

  betay <- list()    ###each component will be a list array

  #-- prediction --#
  for (h in 1:ncomp) {
    dd <- coefficients(lm(y ~ variates.x[, 1:h, drop = FALSE]))
    #regression of y on variates.global.x => =loadings.global.y at a scale factor

    if (q == 1) {
      betay[[h]] <- (dd[-1])
    } else {
      betay[[h]] <- (dd[-1, ])
    }    ###q is ncoly
    ####################################################################################################################################
    #############I don't quite understand this
    W <-
      a[, 1:h, drop = FALSE] %*% solve(t(c[, 1:h, drop = FALSE]) %*% a[, 1:h, drop =
                                                                         FALSE])  ###if AX=B,solve(A,B) is X
    ###drop=False do not drop dimensions
    ###a is loading
    B <- W %*% drop(betay[[h]])
    ##############################################################################################################################
    y.temp <-
      newdata %*% as.matrix(B) #so far: gives a prediction of y centered and scaled
    y.temp <- scale(y.temp,
                    center = FALSE,
                    scale = 1 / sigma.y) #so far: gives a prediction of y centered, with the right scaling
    y.temp <- scale(y.temp,
                    center = -means.y,
                    scale = FALSE) #so far: gives a prediction of y with the right centering and scaling

    y.hat[, , h] <-
      y.temp # we add the variance and the mean of y used in object to predict
    t.pred[, h] <- newdata %*% W[, h]
    B.hat[, , h] <- B
  }  #end h

  #-- valeurs sortantes --#
  rownames(t.pred) <- rownames(newdata)
  colnames(t.pred) <- paste("dim", c(1:ncomp), sep = " ")
  rownames(y.hat) <- rownames(newdata)
  colnames(y.hat) <- colnames(y)

  if (onlyPred) {
    return(invisible(list(predict = y.hat)))
  }
  else {
    return(invisible(
      list(
        predict = y.hat,
        ###row is new data observation times, column is y col
        variates = t.pred,
        ###row is new data observation times,column is y col
        B.hat = B.hat,
        betay = betay
      )
    ))
  }
}





#' Predict plsda
#' Adapted and stripped down from mixOmics v 5.2.0 (https://cran.r-project.org/web/packages/mixOmics/)
#' @param object a plsdaMUVR object
#' @param newdata new data
#' @param onlyPred Boolean for whether to report back predictions only (defaults to FALSE)
#' @param ... other arguments
#' @param scale scale in MUVR
#' @return plsda predictions
#' @export
predict.plsdaMUVR <- function(object,
                              newdata,
                              scale = TRUE,
                              ##scale in MUVR
                              onlyPred = FALSE,
                              ...)  {
  #-- validation des arguments --#
  if (missing(newdata)) {
    stop("No new data available.")
  }

  x <- object$X
  y <- object$Y
  yprim <- object$ind.mat   ###index matrix
  q <- ncol(yprim)
  p <- ncol(x)

  if (ncol(x) != ncol(newdata) & length(object$nzv$Position) > 0) {
    newdata <- newdata[, -object$nzv$Position]
  }
  if (length(dim(newdata)) == 2) {
    if (ncol(newdata) != p)
    {
      stop("'newdata' must be a numeric matrix with ncol = ",
           p,
           " or a vector of length = ",
           p,
           ".")
    }
    if (!identical(colnames(x), colnames(newdata)))
    {
      stop("Mismatch between columns in model and newdata.")
    }
  }

  if (length(dim(newdata)) == 0) {
    if (length(newdata) != p)
    {
      stop("'newdata' must be a numeric matrix with ncol = ",
           p,
           " or a vector of length = ",
           p,
           ".")
    }
    dim(newdata) = c(1, p)
  }

  #-- initialisation des matrices --#
  ncomp <- object$ncomp
  a <- object$loadings$X
  b <- object$loadings$Y
  c <- object$mat.c

  newdata <- as.matrix(newdata)

  if (scale == TRUE) {
    means.x <- attr(x, "scaled:center")
    sigma.x <- attr(x, "scaled:scale")
    newdata <- scale(newdata, center = means.x, scale = sigma.x)
  }

  means.y <- attr(y, "scaled:center")
  sigma.y <- attr(y, "scaled:scale")

  ##- coeff de regression
  B.hat <- array(0, dim = c(p, q, ncomp))
  ##- prediction
  y.hat <- array(0, dim = c(nrow(newdata), q, ncomp))
  ##- variates
  t.pred <- array(0, dim = c(nrow(newdata), ncomp))
  variates.x <- object$variates$X
  betay <- list()

  #-- prediction --#
  for (h in 1:ncomp) {
    dd <- coefficients(lm(y ~ variates.x[, 1:h, drop = FALSE]))
    #regression of y on variates.global.x => =loadings.global.y at a scale factor
    if (q == 1) {
      betay[[h]] <- (dd[-1])
    } else {
      betay[[h]] = (dd[-1, ])
    }

    W <-
      a[, 1:h, drop = FALSE] %*% solve(t(c[, 1:h, drop = FALSE]) %*% a[, 1:h, drop =
                                                                         FALSE])
    B <- W %*% drop(betay[[h]])

    y.temp <-
      newdata %*% as.matrix(B) #so far: gives a prediction of y centered and scaled
    y.temp <-
      scale(y.temp, center = FALSE, scale = 1 / sigma.y) #so far: gives a prediction of y centered, with the right scaling
    y.temp <-
      scale(y.temp, center = -means.y, scale = FALSE) #so far: gives a prediction of y with the right centering and scaling

    y.hat[, , h] <-
      y.temp # we add the variance and the mean of y used in object to predict
    t.pred[, h] <- newdata %*% W[, h]
    B.hat[, , h] <- B
  }  #end h

  #-- valeurs sortantes --#
  rownames(t.pred) <- rownames(newdata)
  colnames(t.pred) <- paste("dim", c(1:ncomp), sep = " ")
  rownames(y.hat) <- rownames(newdata)
  colnames(y.hat) <- colnames(y)

  if (onlyPred == TRUE) {
    return(invisible(list(predict = y.hat)))
  }
  else {
    return(invisible(
      list(
        predict = y.hat,
        variates = t.pred,
        B.hat = B.hat,
        betay = betay
      )
    ))
  }
}
