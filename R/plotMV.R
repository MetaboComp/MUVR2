#' Plot MV object Swim lane plot for classification, for regression
#' @param MUVRclassObject An MUVR class object
#' @param model What type of model to plot ('min', 'mid' or 'max'). Defaults to 'mid'.
#' @param factCols An optional vector with colors for the factor levels (in the same order as the levels)
#' @param sampLabels Sample labels (optional; implemented for classification)
#' @param ylim Optional for imposing y-limits for regression and classification analysis
#' @return A plot of results from multivariate predictions
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
  # Basic sanity
  if (!any(class(MUVRclassObject) == 'MUVR')) {
    stop('Wrong object class')
  }

  # Model number min 1, mid 2, max 3
  modNum <- ifelse(model == 'min',
                   1,
                   ifelse(model == 'mid', 2, 3))
  # Extract actual data of Y
  Y <- MUVRclassObject$inData$Y
  nSamp <- length(Y)
  #################################
  ##When it is classification it is useful, when it is regression, although it has a values it is not used
  if (missing(sampLabels)) {
    sampLabels <- Y
  }
  #####################
  # Sanity check sample labels
  if (length(sampLabels) != nSamp) {
    stop('Length of sampLabels not equal to number of samples in Y.')
  }

  if (any(class(MUVRclassObject) == 'Regression')) {
    ###########################
    # REGRESSION PLOT
    ###########################


    if (class(MUVRclassObject$yPredPerRep)[1] == "list") {
      # Y-predicted overall
      YP <- MUVRclassObject$yPred[, modNum]
      # Y-predicted per repetition
      YPR <- MUVRclassObject$yPredPerRep[[modNum]]
    } else{
      YP <- MUVRclassObject$yPred
      YPR <- MUVRclassObject$yPredPerRep

    }
    # Y-limits
    if (is.null(ylim)) {
      ylim <-
        range(YPR)
    }  ###returns a vector containing the minimum and maximum of all the given arguments
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
           legend = c(paste(
             'Model R2 =', signif(MUVRclassObject$fitMetric$R2[modNum], 3)
           ),
           paste(
             'Model Q2 =', signif(MUVRclassObject$fitMetric$Q2[modNum], 3)
           )),
           ###x2<-c(3.141593e-02,3.141593e+00,3.141593e+02,3.141593e+04,3.141593e+06)
           ###signif(x2,3)
           bty = 'n')  ##	the type of box to be drawn around the legend. The allowed values are "o" (the default) and "n".
  } else if (any(class(MUVRclassObject) == 'Classification')) {
    ################################
    # CLASSIFICATION SWIMLANE PLOT
    ################################
    if (class(MUVRclassObject$yPredPerRep)[1] == "list") {
      # Y-predicted overall
      YP <-
        MUVRclassObject$yPred[[modNum]]     ####The probability that belongs to each class
      # Y-predicted per repetition
      YPR <- MUVRclassObject$yPredPerRep[[modNum]]
    } else{
      YP <-
        MUVRclassObject$yPred     ####The probability that belongs to each class
      # Y-predicted per repetition
      YPR <- MUVRclassObject$yPredPerRep

    }
    # Y-limits
    if (is.null(ylim)) {
      ylim <- range(YPR)
    }
    # Unique levels in Y
    classes <- 1:length(levels(Y))
    # Colors per level
    if (missing(factCols)) {
      factCols <- classes + 1
    }
    if (length(factCols) != length(classes)) {
      stop('Length of factCols not equal to number of levels in Y.')
    }
    # Sort out "jitter"/nudge between levels in the swimlane plot
    ##
    classNudge <-
      0.2 * ((classes - mean(classes)) / (mean(classes) - 1))  ###create distance between different classification on x axis
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
    # Identify erroneous classifications
    if (class(MUVRclassObject$yPredPerRep)[1] == "list") {
      yClass <-
        MUVRclassObject$yClass[, modNum]    ###The class of the most probabilityM
    } else {
      yClass <- MUVRclassObject$yClass
    }
    whichWrong <-
      which(yClass != Y)               ###whichWrong is the sequence number of observations
    wrongClass <-
      as.numeric(Y[whichWrong])      ##Transform to the corresponding real class number of the miss classification ones
    # Mark them out in the plot
    for (w in 1:length(wrongClass)) {
      points(whichWrong[w] + classNudge[wrongClass[w]],
             YP[whichWrong[w], wrongClass[w]],
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
  } else if (any(class(MUVRclassObject) == 'Multilevel')) {
    ###########################
    # MULTILEVEL PLOT
    ###########################



    if (class(MUVRclassObject$yPredPerRep)[1] == "list") {
      # Y-predicted overall
      YP <-
        MUVRclassObject$yPred[, modNum]     ####The probability that belongs to each class
      # Y-predicted per repetition
      YPR <- MUVRclassObject$yPredPerRep[[modNum]]
    } else{
      YP <-
        MUVRclassObject$yPred     ####The probability that belongs to each class
      # Y-predicted per repetition
      YPR <- MUVRclassObject$yPredPerRep

    }

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
