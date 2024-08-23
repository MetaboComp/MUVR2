#' Plot predictions
#' At present, this function only supports predictions for PLS regression type problems
#' @param Ytrue True value of Y, should be a vector
#' @param Ypreds Predicted value of Y can be a vector or data frame with the same number of rows
#' @export
#' @return A plot, plot the prediction
#' @examples
#' \donttest{
#' data("freelive2")
#' Ytrue<-YR2
#' Ypreds<-sampling_from_distribution(YR2)
#' plotPred(Ytrue,Ypreds)
#' Ytrue<-YR2
#' nRep <- 2
#' nOuter <- 4
#' varRatio <-0.6
#' regrModel <- MUVR2(X = XRVIP2,
#'                    Y = YR2,
#'                    nRep = nRep,
#'                    nOuter = nOuter,
#'                    varRatio = varRatio,
#'                    method = "PLS",
#'                    modReturn = TRUE)
#' Ypreds<-regrModel$yPred
#' plotPred(Ytrue,Ypreds)
#' }
plotPred <- function(Ytrue,
                     Ypreds) {
  if(is.list(Ypreds)){
    stop("Ypreds should be either vector or dataframe or matrix")
  }

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  if(!is.data.frame(Ypreds)&!is.matrix(Ypreds)){
  if(length(Ytrue)!=length(Ypreds)){
    stop("The YTrue and YPreds should have same number of observations.")
  }

  ###when Y true is a row, Ypreds is a row

  par(mar = c(4, 4, 0, 0) + .5)
  matplot(Ytrue,
          Ypreds,
          pch = '.', col = 'grey')    #Plot the columns of one matrix against the columns of another (which often is just a vector treated as 1-column matrix).
  points(
    Ytrue,
    Ypreds,
    pch = '.',
    col = 'black',
    cex = 2
  )
  }else{
    if(length(Ytrue)!=nrow(Ypreds)){
      stop("The YTrue and YPreds should have same number of obsevations.")
    }
    ###when Y true is a row, Ypreds is a row

    par(mar = c(4, 4, 0, 0) + .5)
    matplot(Ytrue,
            Ypreds,
            pch = '.', col = 'grey')    #Plot the columns of one matrix against the columns of another (which often is just a vector treated as 1-column matrix).
    points(
      Ytrue,
      rowMeans(Ypreds),
      pch = '.',
      col = 'black',
      cex = 2
    )

  }
}
