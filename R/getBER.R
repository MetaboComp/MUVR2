#' get Balanced Error Rate from classification analysis
#' @param actual Vector of actual classifications of samples
#' @param predicted Vector of predicted classifications of samples
#' @param weigh_added To add a weighing matrix when it is classfication
#' @param weighing_matrix The matrix used for get a miss classfication score
#' @return Balanced Error Rate (BER)
#' @export
#' @examples
#' \dontrun{
#' data("mosquito")
#' actual <- Yotu
#'
#' predicted <- sampling_from_distribution(actual)
#' getBER(actual, predicted)
#' }
getBER <- function (actual,
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

  predicted <- factor(predicted, levels = levels(actual))
  levs <- levels(actual)
  nlevs <- length(levs)
  confMat <- matrix(0, nrow = nlevs, ncol = nlevs + 1)
  rownames(confMat) <- levs
  colnames(confMat) <- paste0("pred.", c(levs, "NA"))

  for (i in 1:nlevs) {
    whLev.i <- which(actual == levs[i])
    for (j in 1:nlevs) {
      confMat[i, j] <- sum(predicted[whLev.i] == levs[j], na.rm = TRUE)
    }
    #if i=1,j=2 confMat is the number of the obs actual in group 1, but predicted in group 2
    confMat[i, nlevs + 1] <- sum(is.na(predicted[whLev.i]))
  }   ##the last column is to see how many predicted are NA when actual is level i
  if (sum(is.na(predicted)) == 0) {
    confMat <- confMat[, -(nlevs + 1)]
  }

  ## When there is no NA in predicted, there is no value for the whole column,then remove the column
  confMat.wrong <- confMat
  diag(confMat.wrong) <- 0
  if (weigh_added == TRUE) {
    if (missing(weighing_matrix)) {
      #  warning("Missing weighing_matrix,weighing_matrix will be diagnoal")
      weighing_matrix <-
        diag(1, length(levels(actual)),
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
    confMat.correct <- confMat * weighing_matrix
    confMat.wrong <- confMat - confMat.correct

  }
  BER <-
    sum(apply(confMat.wrong, 1, sum, na.rm = TRUE) / apply(confMat, 1, sum, na.rm = TRUE),
        na.rm = TRUE) / nlevs
  ##balance error rate
  return(BER)
}
