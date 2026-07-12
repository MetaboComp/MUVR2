#' Plot predictions for PLS regression (ggplot2)
#'
#' `ggplot2` version of [plotPred()]. Grey points are the individual predictions
#' (one series per column of `Ypreds`); black points are their consensus (row
#' means).
#'
#' @param Ytrue True value of Y, should be a vector
#' @param Ypreds Predicted value of Y; a vector, or a matrix/data frame with one
#'   row per observation
#' @return A `ggplot` object
#' @seealso [plotPred()] for the base graphics version
#' @export
#' @examples
#' \donttest{
#' data("freelive2")
#' Ypreds <- sampling_from_distribution(YR2)
#' ggplotPred(YR2, Ypreds)
#' }
ggplotPred <- function(Ytrue, Ypreds) {
  d <- predData(Ytrue, Ypreds)

  ggplot(d$consensus, aes(x = .data$Ytrue, y = .data$Ypred)) +
    geom_point(data = d$perPred, colour = "grey", size = 1) +
    geom_point(colour = "black", size = 1.5) +
    labs(x = "Original Y", y = "Predicted Y") +
    theme_bw()
}
