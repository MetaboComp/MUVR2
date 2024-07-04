#' qMUVR2: Wrapper for speedy access to MUVR (autosetup of parallelization)
#' @param X X-data
#' @param Y Y-data
#' @param ML Boolean for multilevel
#' @param method 'RF' (default) or 'PLS'
#' @param varRatio proportion of variables to keep in each loop of the recursive feature elimination
#' @param nCore Number of threads to use for calculation (defaults to detectCores()-1)
#' @param repMult Multiplier of cores -> nRep = repMult * nCore
#' @param nOuter Number of outer segments
#' @param ... Additional arguments(see MUVR)
#' @return MUVR object
#' @export
#' @examples
#' \dontrun{
#' data("freelive2")
#' regrModel <- qMUVR2(X = XRVIP2,
#' Y = YR2)
#' }
qMUVR2 <- function(X,
                  Y,
                  ML = FALSE,
                  method = 'RF',
                  varRatio = 0.65,
                  nCore,
                  repMult = 1,
                  nOuter = 5,
                  ...) {
  #library(doParallel)
  if (missing(nCore)) {
    nCore <- detectCores() - 1
  }
  nRep <- repMult * nCore
  cl <- makeCluster(nCore)
  registerDoParallel(cl)
  if (ML) {
    mod <-
      MUVR2(
        X = X,
        ML = TRUE,
        method = method,
        nRep = nRep,
        nOuter = nOuter,
        varRatio = varRatio
      )
  } else {
    ###This is when ML we don,t use true Y, we use man-made Y
    mod <-
      MUVR2(
        X = X,
        Y = Y,
        method = method,
        nRep = nRep,
        nOuter = nOuter,
        varRatio = varRatio,
        ...
      )
  }
  stopCluster(cl)
  return(mod)
}
