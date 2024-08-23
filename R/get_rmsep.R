#' get rmsep for regression
#' @param actual Vector of actual classifications of samples
#' @param predicted Vector of predicted classifications of samples
#'
#' @return rmsep
#' @export
#' @examples
#' \donttest{
#' data("mosquito")
#' actual <- YR2
#' predicted <- sampling_from_distribution(actual)
#' get_rmsep(actual, predicted)
#' }
get_rmsep <- function(actual,
                      predicted) {
  value <-
    sqrt(sum((actual - predicted) ^ 2, na.rm = TRUE) / (length(actual) - sum(is.na(predicted))))
  return(value)
}
