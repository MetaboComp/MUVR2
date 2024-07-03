#' Plot predictions
#' At present, this function only supports predictions for PLS regression type problems
#' @param Ytrue True value of Y
#' @param Ypreds Predicted value of Y
#' @export
#' @examples
#' \dontrun{
#' data("freelive2")
#' Ytrue<-YR2
#' Ypreds<-sampling_from_distribution(YR2)
#' plotPred(Ytrue,Ypreds)
#' }
plotPred <- function(Ytrue,
                     Ypreds) {
  ###when Y true is a row, Ypreds is a row
  par(mar = c(4, 4, 0, 0) + .5)
  matplot(Ytrue, t(Ypreds), pch = '.', col = 'grey')    #Plot the columns of one matrix against the columns of another (which often is just a vector treated as 1-column matrix).
  points(
    Ytrue,
    colMeans(Ypreds),
    pch = '.',
    col = 'black',
    cex = 2
  )
}
