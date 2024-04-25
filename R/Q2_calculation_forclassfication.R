#' Q2 calculation for classification
#'@param classification_prediction_matrix prediction values
#'@param y real values
#'@param minmidmax minmidmax
#'@export
#'@return Q2
# a<-MUVR2(Xotu[1:13,],factor(Yotu[1:13]),method="PLS",DA=T,modReturn = T)
# classification_prediction_matrix<-a$yPred$min
# y<-as.factor(as.character(Yotu[1:13]))
# b<-MUVR2(Xotu,factor(Yotu),method="PLS",DA=T,modReturn = T)
# classification_prediction_matrix<-b$yPred$min
# y<-as.factor(as.character(Yotu))

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
