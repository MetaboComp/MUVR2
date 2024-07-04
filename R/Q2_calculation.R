#' Q2 calculation
#'@param yhat prediction values
#'@param y real values
#'@export
#'@return Q2
#' @examples
#' \dontrun{
#' data("freelive2")
#' actual <- YR2
#' predicted <- MUVR2::sampling_from_distribution(actual)
#' Q2_calculation(actual, predicted)
#' }
Q2_calculation <- function(yhat, y) {
  PRESS <- sum((y - yhat) ^ 2)
  TSS <- sum((y - mean(y)) ^ 2)
  Q2 <- 1 - PRESS / TSS
  return(Q2)

}
