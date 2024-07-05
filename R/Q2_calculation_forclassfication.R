#' Q2 calculation for classification
#'@param classification_prediction_matrix prediction values
#'@param y real values
#'@param minmidmax minmidmax
#'@return Q2
#' @examples
#' \dontrun{
#' data("mosquito")
#' nRep <- 2
#' nOuter <- 4
#' classModel<-MUVR2_EN(Xotu,Yotu,
#'                      method="PLS",
#'                      nRep=nRep,
#'                      nOuter=nOuter,
#'                      DA=T,
#'                      modReturn = T)
#' classification_prediction_matrix <- classModel$yPred
#' y <- Yotu
#' Q2_calculation_forclassification(classification_prediction_matrix,
#' y)
#' }
#' @noRd


Q2_calculation_forclassification <-
  function(classification_prediction_matrix,
           y,
           minmidmax = "min") {
    if (!identical(colnames(classification_prediction_matrix), levels(y))) {
      stop("colnames of the matrix should be the same as levels of Y")
    }
    if (length(y) != nrow(classification_prediction_matrix)) {
      stop("Observation number is different")
    }
    y_frame <- as.data.frame(y)
    y_frame <- onehotencoding(y_frame)
    Q2_vector <- vector()
    if (length(levels(y)) > 2) {
      if (ncol(classification_prediction_matrix) != ncol(y_frame)) {
        stop("number of levels does not match")
      }
    } else{
      y_frame <- cbind(y_frame, y_frame)
      y_frame[, 2] <- abs(y_frame[, 1] - 1)

    }
    colnames(y_frame) <- levels(y)
    for (i in 1:length(levels(y))) {
      Q2_vector <- c(
        Q2_vector,
        Q2_calculation(yhat = classification_prediction_matrix[, i],
                       y = y_frame[, i])
      )
    }

    names(Q2_vector) <- colnames(classification_prediction_matrix)
    return(Q2_vector)

  }
