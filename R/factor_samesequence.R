#'factor_samesequence: This function is to change the order of items in the levels of a factor variable
#' @param X a factor variable
#' @param level levels if want to manually set
#' @param sequence default as FALSE, if TRUE normal factor
#' @return X_factor
#' @examples
#' data("mosquito")
#' Yotu2<-factor(Yotu,levels=c("VK5","VK7","VK3"))
#' MUVR2:::factor_samesequence(Yotu)
#' @noRd
factor_samesequence <- function(X,
                                level,
                                sequence) {
  if(!is.factor(X)){
    warning("The input needs to be a factor variable")
  }
  if (missing(level)) {
    if (missing(sequence)) {
      sequence <- FALSE
    }
    if (sequence == FALSE) {
      XX <- X
      X <- as.character(X)
      X_levels <- vector()
      if (length(X_levels) == 0) {
        X_factor <- c(X_levels, X[1])
      }
      for (i in 1:length(X)) {
        if (X[i] %in% X_levels) {
          X_levels <- X_levels
        }
        else{
          X_levels <- c(X_levels, X[i])
        }
      }
      X_factor <- factor(XX, levels = X_levels)
    }
    if (sequence == TRUE) {
      X <- factor(X, levels = unique(X))
    }
  }
  if (!missing(level)) {
    X <- factor(X, levels = level)
  }
  return(X_factor)
}
