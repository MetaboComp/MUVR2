#' Q2 calculation
#'@param yhat prediction values
#'@param y real values
#'@export
#'@return Q2

Q2_calculation <- function(yhat, y) {
  PRESS <- sum((y - yhat) ^ 2)
  TSS <- sum((y - mean(y)) ^ 2)
  Q2 <- 1 - PRESS / TSS
  return(Q2)

}
