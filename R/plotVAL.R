#' Plot validation metric
#'
#' Produces a plot of validation metric vs number of variables in model (inner segment).
#' @param MUVRclassObject An object of class `MUVR`
#' @param show_outlier Boolean, show outliers
#' @return A plot
#' @seealso [ggplotVAL()] for the ggplot2 version
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
#' plotVAL(regrModel)
#' }
plotVAL <- function(MUVRclassObject,
                    show_outlier = TRUE) {
  d <- valData(MUVRclassObject)
  metric <- d$metric
  nVar <- d$nVar

  if (d$type == "quantile") {
    ###### Elastic net, variables selected by quantile: distribution of the
    ###### number of variables selected across the nOuter*nRep calibration models
    hist(
      d$nonZeroRep,
      xlab = "Number of variables selected across nOuter*nRep loops",
      main = NULL,
      xlim = d$xlim,
      breaks = d$breaks
    )
    for (i in 1:3) {
      ####add vertical line
      abline(
        v = nVar[i],
        lty = i,
        col = i + 1,
        lwd = 1.5
      )
    }
    legend(
      'topright',
      legend = c("Min", "Median", "Max"),
      lty = 1:3,
      cex = 0.5,
      trace = FALSE,
      col = 2:4,
      bty = 'n'
    )

  } else if (d$type == "fitness") {
    ###### Elastic net, variables selected from the fitness curve: one point per
    ###### calibration model, plus the curve getVar() fitted through them
    points_df <- d$points
    if (!show_outlier) {
      points_df <- points_df[!points_df$outlier, ]
    }
    plot(
      points_df$nVar,
      points_df$fitness,
      col = points_df$colour,
      xlab = "Number of variables selected across nOuter*nRep loops",
      ylab = metric
    )
    lines(d$curve$nVar,
          d$curve$fitness)
    for (i in 1:3) {
      ####add vertical line
      abline(
        v = nVar[i],
        lty = i,
        col = i + 1,
        lwd = 1.5
      )
    }
    legend(
      'topright',
      legend = c("'Min (Minimal-optimal)",
                 "'Mid'",
                 "'Max' (All-relevant)"),
      lty = 1:3,
      cex = 0.5,
      trace = FALSE,
      col = 2:4,
      bty = 'n'
    )

  } else {
    ###### PLS / RF: validation curves per segment, per repetition, and overall
    VAL <- d$VAL
    count <- d$count
    nRep <- d$nRep

    plot(
      count,
      count,
      ylim = range(VAL),
      xlim = range(count),
      log = 'x',
      ###########log scale x axis
      type = 'n',
      bty = 'l',
      ylab = metric,
      xlab = 'Number of variables (log scale)'
    )
    for (r in 1:nRep) {
      matlines(count,
               t(VAL[, , r]),
               type = 'l',
               lty = 1,
               col = 'lightgrey')
    }
    for (r in 1:nRep) {
      lines(count,
            colMeans(VAL[, , r]), col = 'darkgrey')     ####mean per repetition
    }
    lines(count,
          apply(VAL, 2, mean),   ###mean over all segments and repetitions
          col = 'black')

    for (i in 1:3) {
      ####add vertical line
      abline(
        v = nVar[i],
        lty = i,
        col = i + 1,
        lwd = 1.5
      )
    }
    legend(
      'topleft',
      legend = c('Validation segments',
                 'Repetitions',
                 'Overall'),
      cex = 0.5,
      trace = FALSE,
      lty = 1,
      col = c('lightgrey', 'darkgrey', 'black'),
      bty = 'n'
    )
    legend(
      'topright',
      legend = c("'Min (Minimal-optimal)", "'Mid'", "'Max' (All-relevant)"),
      lty = 1:3,
      cex = 0.5,
      trace = FALSE,
      col = 2:4,
      bty = 'n'
    )
  }
}
