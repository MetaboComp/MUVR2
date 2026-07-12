#' Plot predictions
#' 
#' Plot predicted and actual target variables, with different plots depending on modelling approach.
#' @param MUVRclassObject An MUVR class object
#' @param model What type of model to plot ('min', 'mid' or 'max'). Defaults to 'mid'.
#' @param factCols An optional vector with colors for the factor levels (in the same order as the levels)
#' @param sampLabels Sample labels (optional; implemented for classification)
#' @param ylim Optional for imposing y-limits for regression and classification analysis
#' @return A plot of results from multivariate predictions
#' @seealso [ggplotMV()] for the ggplot2 version
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
#' plotMV(regrModel, model="min")
#' }
#######################################################################################################################3
#Change  the variables as MUVRclassObject
#
####################################################################################################
plotMV <- function(MUVRclassObject,
                   model = 'min',
                   factCols,
                   sampLabels,
                   ylim = NULL) {
  d <- mvData(MUVRclassObject,
              model = model,
              sampLabels = sampLabels,
              ylim = ylim)

  modNum <- d$modNum
  Y <- d$Y
  nSamp <- d$nSamp
  sampLabels <- d$sampLabels
  YP <- d$YP
  YPR <- d$YPR
  ylim <- d$ylim

  if (d$type == "regression") {
    ###########################
    # REGRESSION PLOT
    ###########################
    # Plot Y-predicted per repetition in grey
    matplot(
      Y,
      #####X axis 112 observations
      YPR,
      ####Y axis 112 obsevations each of them have 7 repetitions
      pch = 20,
      xlab = 'Original Y',
      ylab = 'Predicted Y',
      col = 'grey',
      bty = 'l',
      cex = 0.5,
      ylim = ylim
    )
    # Add in overall Y-predictions of repitions in black
    points(Y,
           YP,
           pch = 20)
    # Add simple regression line
    reg <- lm(YP ~ Y)
    #clip(x1=min(Y),x2=max(Y),y1=min(YP),y2=max(YP))
    abline(reg)
    # Add legend
    legend('topleft',
           legend = c(paste('Model R2 =', signif(d$R2, 3)),
                      paste('Model Q2 =', signif(d$Q2, 3))),
           bty = 'n')  ##	the type of box to be drawn around the legend. The allowed values are "o" (the default) and "n".
  } else if (d$type == "classification") {
    ################################
    # CLASSIFICATION SWIMLANE PLOT
    ################################
    # Unique levels in Y
    classes <- d$classes
    # Colors per level
    if (missing(factCols)) {
      factCols <- classes + 1
    }
    if (length(factCols) != length(classes)) {
      stop('Length of factCols not equal to number of levels in Y.')
    }
    # "jitter"/nudge between levels in the swimlane plot
    classNudge <- d$classNudge
    # Allocate plot surface
    plot(
      1:nSamp,
      Y,
      type = 'n',
      ###"n" for no plotting.
      ylim = ylim,
      xlab = '',
      ylab = 'Class prediction probability',
      xaxt = 'n'
    )
    # Custom axis
    axis(1,
         ###1=below, 2=left, 3=above and 4=right.
         at = 1:length(Y),
         ###the points at which tick-marks are to be drawn.
         labels = sampLabels,
         las = 3)               ###label direction
    ##0: always parallel to the axis
    ##1: always horizontal
    ##2: always perpendicular to the axis
    ##3: always vertical.
    # Plot each Y level separately
    for (cl in classes) {
      # Y-pred per rep
      matpoints((1:nSamp) + classNudge[cl],
                YPR[, cl, ],
                ##For all the observations for all repetition in each class
                pch = 20,
                col = factCols[cl],
                cex = 0.5
      )
      # Y-pred overall
      points((1:nSamp) + classNudge[cl],
             YP[, cl],
             pch = 20,
             col = factCols[cl])
    }
    # Add swimlane lines
    for (li in 1:(nSamp + 1)) {
      abline(v = li - .5,
             lty = 3,
             ####type of line
             col = 'grey')
    }
    # Ring the misclassified samples at the probability of their true class
    if (nrow(d$wrong) > 0) {
      points(d$wrong$x,
             d$wrong$probability,
             cex = 2)
    }
    ##################################################################################################################################################
    ##I didn't figure out how to add legend here for classification problem
    # Add legend
    xpdOld <- par()$xpd            #######

    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))

    par(xpd = TRUE) ###A logical value or NA. If FALSE, all plotting is clipped to the plot region,
    ###if TRUE, all plotting is clipped to the figure region, and if NA, all plotting is clipped to the device region.

    legend(
      x = 0,
      y = ylim[2] + diff(ylim) / 5,
      ##the difference between ylim max an ylim min
      horiz = TRUE,
      legend = c(levels(Y), 'misclassified'),
      pch = c(rep(16, length(classes)), 1),
      ###the plotting symbols appearing in the legend, a
      col = c(factCols, 1),
      cex = 0.8,
      pt.cex = c(rep(0.5, length(classes)), 2),
      #legend.direction="vertical",
      bty = 'n'
    )                       ##the type of box to be drawn around the legend
    par(xpd = xpdOld)


    ##################################################################################################################################################
  } else if (d$type == "multilevel") {
    ###########################
    # MULTILEVEL PLOT
    ###########################
    matplot(
      YPR,
      1:nSamp,
      pch = 20,
      col = 'grey',
      cex = 0.5,
      ylim = c(nSamp, 1),
      ylab = 'Sample number',
      xlab = 'Predicted Y'
    )
    # Plot Y-predicted overall in black
    points(YP,
           1:nSamp,
           pch = 20,
           col = 'black')
    # Draw support lines
    abline(h = nSamp / 2 + 0.5, lty = 2)
    abline(v = 0, lty = 2)
  }
}
