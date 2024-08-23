#' get the number of miss classifications from classification analysis
#' @param actual Vector of actual classifications of samples
#' @param predicted Vector of predicted classifications of samples
#' @param weigh_added To add a weighing matrix when it is classfication
#' @param weighing_matrix The matrix used for get a miss classfication score
#' @return miss classification
#' @export
#' @examples
#' \donttest{
#' data("mosquito")
#' actual <- Yotu
#' predicted <- sampling_from_distribution(actual)
#' getMISS(actual, predicted)
#' }

getMISS <- function (actual,
                     predicted,
                     weigh_added = FALSE,
                     weighing_matrix)
{
  if (length(actual) != length(predicted)) {
    stop ("Mismatch in length of arguments")
  }
  if (!is.factor(actual)) {
    actual <- factor(actual)
  }
  predicted <- factor(predicted,
                      levels = levels(actual))

  levs <- levels(actual)
  nlevs <- length(levs)
  confMat <- table(actual, predicted)
  rownames(confMat) <- levs
  colnames(confMat) <- paste0("pred.", c(levs))

  if (weigh_added == TRUE) {
    if (missing(weighing_matrix)) {
      #  warning("Missing weighing_matrix,weighing_matrix will be diagnoal")
      weighing_matrix <- diag(1, length(levels(actual)),
                              length(levels(actual)))
    }
    if (dim(weighing_matrix)[1] != length(levels(actual))) {
      stop("The dimension of weighing_matrix is not correct")
    }
    if (dim(weighing_matrix)[2] != length(levels(actual))) {
      stop("The dimension of weighing_matrix is not correct")
    }
    for (i in 1:nrow(weighing_matrix)) {
      if (weighing_matrix[i, i] != 1) {
        stop("diagonal values must be 1")
      }
      for (j in 1:ncol(weighing_matrix)) {
        if (weighing_matrix[i, j] < 0 | weighing_matrix[i, j] > 1) {
          stop("Values in the weighing matrix must between 0 and 1")
        }
      }
    }
    confMat <- confMat * weighing_matrix
  }
  MISS <- length(actual) - (sum(confMat) - sum(diag(confMat)))
  ##balance error rate
  return(MISS)
}
