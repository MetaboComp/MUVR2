#' Plot stability 
#' 
#' Plot stability of selected variables and prediction fitness as a function of number of repetitions.
#' @param MUVRrdCVclassObject MUVR class object or rdCV object
#' @param model 'min' (default), 'mid' or 'max'
#' @param VAll Option of specifying which variables (i.e. names) to consider as reference set.
#' Defaults to variables selected from the `model` of the `MUVRrdCVclassObject`
#' @param nVarLim Option of specifying upper limit for number of variables
#' @param missLim Option of specifying upper limit for number of misclassifications
#' @return Plot of number of variables, proportion of variables overlapping with reference and prediction accuracy (Q2 for regression; MISS otherwise) as a function of number of repetitions.
#' @seealso [ggplotStability()] for the ggplot2 version
#' @export
#' @examples
#' \donttest{
#' data("freelive2")
#' nRep <- 2
#' nOuter <- 4
#' varRatio <-0.6
#' regrModel <- MUVR2(X = XRVIP2,
#'                    Y = YR2,
#'                    nRep = nRep,
#'                   nOuter = nOuter,
#'                    varRatio = varRatio,
#'                    method = "PLS",
#'                    modReturn = TRUE)
#' plotStability(regrModel, model = "min")
#' }
plotStability <- function(MUVRrdCVclassObject,
                          model = 'min',
                          VAll,
                          nVarLim,
                          missLim) {
  d <- stabilityData(MUVRrdCVclassObject,
                     model = model,
                     VAll = VAll,
                     nVarLim = nVarLim,
                     missLim = missLim)

  regr <- d$regr
  DA <- d$DA
  ML <- d$ML
  nRep <- d$nRep
  nVRep <- d$nVRep
  nV <- d$nV
  VARep <- d$VARep
  VA <- d$VA
  missRep <- d$missRep
  miss <- d$miss
  berRep <- d$berRep
  ber <- d$ber
  q2Rep <- d$q2Rep
  q2 <- d$q2
  nVarLim <- d$nVarLim
  missLim <- d$missLim

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  # One panel per metric, stacked
  nPlot <- ifelse(ML, 5, ifelse(regr, 3, 4))
  par(mfrow = c(nPlot, 1))
  par(mar = c(3, 4, 0, 0) + .5)

  ######################
  #Plot 1 Number of selected variables vs number of repetions
  plot(
    nVRep,
    ###each repetition
    ylim = c(0, nVarLim),
    type = 'l',
    xlab = '',
    ylab = 'Number of selected variables',
    col = 'grey',
    bty = 'l'
  )     #### the type of box
  lines(nV)
  legend(
    'bottomright',
    c('Per repetition', 'Cumulative'),
    col = c('grey', 'black'),
    lty = 1,
    bty = 'n'
  )

  ########################
  ##Plot 2 proportion of selected variables (variables numbers that included in each repetition\variable numbers that includes in final model)
  ##vs  number of repetitions
  plot(
    VARep,
    ######each repetition
    type = 'l',
    ylim = c(0, 1),
    col = 'pink',
    xlab = '',
    ylab = 'Proportion of selected variables',
    bty = 'l'
  )
  lines(VA,                       ####cumulative
        col = 'red')
  legend(
    'bottomright',
    c('Per repetition', 'Cumulative'),
    col = c('pink', 'red'),
    lty = 1,
    ###linetype
    bty = 'n'
  )
  #  }
  #######################
  ####Plot 3 Number of Missclassification vs number of repetitions

  if (DA | ML) {
    plot(
      missRep,
      ######each repetition
      ylim = c(0, missLim),
      type = 'l',
      col = 'lightblue',
      xlab = '',
      ylab = 'Number of misclassifications',
      bty = 'l'
    )
    lines(miss,                ####cumulative
          col = 'blue')
    legend(
      'bottomright',
      c('Per repetition', 'Cumulative'),
      col = c('lightblue', 'blue'),
      lty = 1,
      bty = 'n'
    )
    plot(
      berRep,
      ######each repetition
      ylim = c(0, 1),
      type = 'l',
      col = 'lightblue',
      xlab = '',
      ylab = 'Balance ErrorRate',
      bty = 'l'
    )
    lines(ber,                ####cumulative
          col = 'blue')
    legend(
      'bottomright',
      c('Per repetition', 'Cumulative'),
      col = c('lightblue', 'blue'),
      lty = 1,
      bty = 'n'
    )

  }
  ########################
  ##Plot 4 Q2 vs number of repetitions
  if (regr | ML) {
    ###ML is a regression
    plot(
      q2Rep,
      ######each repetition
      ylim = c(min(q2Rep, q2), 1),
      type = 'l',
      col = 'lightgreen',
      xlab = '',
      ylab = 'Q2',
      bty = 'l'
    )
    lines(q2,                       ####cumulative
          col = 'darkgreen')
    legend(
      'bottomright',
      c('Per repetition', 'Cumulative'),
      col = c('lightgreen', 'darkgreen'),
      lty = 1,
      bty = 'n'
    )
  }

  #########################
  ####For all 4 plots
  mtext(
    text = 'Number of repetitions',
    side = 1,
    ####on which side of the plot (1=bottom, 2=left, 3=top, 4=right).
    line = 2.3,
    ####on which MARgin line, starting at 0 counting outwards.
    cex = par()$cex
  )    ###custom text size, if this is not added, the text size is bigger
  ###character expansion factor. NULL and NA are equivalent to 1.0.
  ###This is an absolute measure, not scaled by par("cex") or by setting par("mfrow") or par("mfcol").
  ###Can be a vector
  par(mfrow = c(1, 1))



}
