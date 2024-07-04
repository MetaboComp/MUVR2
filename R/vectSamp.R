#' Sampling of a vector
#' Random sampling of a vector into n groups
#' Bug: Sampling is faulty when length(vect) < n
#' @param vect A vector to be sampled into groups
#' @param n Number of groups
## @param sampLen A vector with custom number of samples per group. Is calculated if missing (best choice).
#' @return a list with n groups containing sub sampled `vect`
#' @export
#' @examples
#' \dontrun{
#' data("mosquito")
#' vectSamp(Yotu)
#' }
vectSamp <- function(vect,
                    n = 4) {
  # Pick 'n' random samples within vector 'vect'
  # sampLen is a vector of number of observations within sample
  # If sampLen is not specified it is automatically calculated

  #library(caret)
  if (length(vect) < n) {
    fold_pre <- caret::createFolds(y = vect, k = length(vect))
    fold <- list()
    len <- n - length(vect)
    for (s in 1:len) {
      fold[[s]] <- as.factor(vector())
      fold[[s]] <-
        factor(fold[[s]], levels = levels(vect))
    }
    for (s in ((len + 1):n))  {
      fold[[s]] <- fold_pre[[s - len]]
    }
  } else{
    fold <- caret::createFolds(y = vect, k = n)  ###stratified spliting
  }
  data <- list()
  for (i in 1:n) {
    data[[i]] <- vect[fold[[i]]]
  }
  sorteddata <- data[order(sapply(data, length))]
  return(sorteddata)


  ### A issue needs to be fixed when number of fold
  ## is bigger than the length of variables
}
