#' Get RMSEP
#' 
#' Get Root Mean Square Error of Prediction (RMSEP) in classification.
#' @param actual Vector of actual classifications of samples
#' @param predicted Vector of predicted classifications of samples
#' @return RMSEP
#' @export
#' @examples
#' data("mosquito")
#' actual <- YR2
#' predicted <- sampling_from_distribution(actual)
#' get_rmsep(actual, predicted)

get_rmsep <- function(actual,
                      predicted) {
  value <-
    sqrt(sum((actual - predicted) ^ 2, na.rm = TRUE) / (length(actual) - sum(is.na(predicted))))
  return(value)
}
