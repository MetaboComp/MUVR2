#' Plot stability of selected variables and prediction fitness as a function of number of repetitions
#' @param MUVRrdCVclassObject MUVR class object or rdCV object
#' @param model 'min' (default), 'mid' or 'max'
#' @param VAll Option of specifying which variables (i.e. names) to consider as reference set.
#'             Defaults to variables selected from the `model` of the `MUVRrdCVclassObject`
#' @param nVarLim Option of specifying upper limit for number of variables
#' @param missLim Option of specifying upper limit for number of misclassifications
#' @return Plot of number of variables, proportion of variables overlapping with reference and prediction accuracy (Q2 for regression; MISS otherwise) as a function of number of repetitions.
#' @export
plotStability <- function(MUVRrdCVclassObject,
                          model = 'min',
                          VAll,
                          nVarLim,
                          missLim) {
  regr <- any(class(MUVRrdCVclassObject) == 'Regression')
  classification <-
    any(class(MUVRrdCVclassObject) == 'Classification')
  DA <- MUVRrdCVclassObject$inData$DA
  ML <- MUVRrdCVclassObject$inData$ML
  Y <- MUVRrdCVclassObject$inData$Y

  nModel <- ifelse(model == 'min',
                   1,
                   ifelse(model == 'mid',
                          2,
                          3))
  ######### if rdCV
  if (!any(class(MUVRrdCVclassObject) == 'rdCVnet')) {
    nVar <- round(MUVRrdCVclassObject$nVar[nModel])
    if (missing(VAll)) {
      VAll <- names(sort(MUVRrdCVclassObject$VIRank[, nModel])[1:nVar])
    }

  } else {
    nVar <- round(MUVRrdCVclassObject$nVar[nModel])
    ################?????????????????
    if (missing(VAll)) {
      VAll <- MUVRrdCVclassObject$Var[[nModel]]
    }
  }
  ##sort the column of of the selected model(min, mid, max).choose the first nVar variables' names

  # Final selection of variables

  nRep <- MUVRrdCVclassObject$inData$nRep
  nVRep <-
    VARep <-
    missRep <-
    berRep <-
    r2Rep <-
    q2Rep <- nV <- VA <- miss <- ber <- r2 <- q2 <- numeric(nRep)
  ### anumeric vector of length nRep

  for (i in 1:nRep) {
    if (!any(class(MUVRrdCVclassObject) == 'rdCVnet')) {
      #nModel=1
      nVRep[i] <- MUVRrdCVclassObject$nVarPerRep[[nModel]][i]
      ##number of nVar for each repetition in min model.It is the same length as nRep

      ######### if rdCV
      nV[i] <-
        round(mean(MUVRrdCVclassObject$nVarPerRep[[nModel]][1:i]))
      ## the mean of nVarPerRep of first i repetitions in min model. It is the same length as nRep

      VARep[i] <-
        sum(names(sort(
          MUVRrdCVclassObject$VIRankPerRep[[nModel]][, i]
        )[1:nVRep[i]]) %in% VAll)
      ###in repetition i,if the variables selected as included in that repetition is in the VAll, save there names
      ##before sum() Each VARep[i] is a vector of variable names. Each repetition has a vector
      ###sum() add 1 if name is in it
      ##output is a number of variables in VALL for  repetition i

      VA[i] <- sum(names(sort(
        rowMeans(MUVRrdCVclassObject$VIRankPerRep[[nModel]][, 1:i, drop = F])
      )[1:nV[i]]) %in% VAll)
      ##calculate  the mean of  first i repetitions of VIRankPerRep first and then rank them
      ##choose the first nV[i] and keep the ones that are in VALL
      ###sum() add 1 if name is in it
      ##output is a number of variables in VALL for first i repetition
    } else{
      nVRep[i] <-
        MUVRrdCVclassObject$nVarPerRep[i]  ### median of each nOuter's number's of variables
      nV[i] <-
        round(mean(MUVRrdCVclassObject$nVarPerRep[1:i]))   ## cumulative average
      VARep_temp <- c()
      for (z in 1:MUVRrdCVclassObject$inData$nOuter) {
        VARep_temp <-
          c(VARep_temp, MUVRrdCVclassObject$varRep[[(i - 1) * MUVRrdCVclassObject$inData$nOuter +
                                                      z]])
      }
      VARep_temp <- unique(VARep_temp)

      VARep[i] <- sum(VARep_temp %in% VAll)
      # VARep[i]<-sum(rownames(MUVRrdCVclassObject$coefRep)[apply(abs(MUVRrdCVclassObject$coefRep[,,i]),1,sum)!=0]%in%VAll)
      #VARep[i] <- sum(names(sort(MUVRrdCVclassObject$VIRankPerRep[[nModel]][,i])[1:nVRep[i]])%in%VAll)
      VA[i] <- mean(VARep[1:i])
      #    VA[i] <- sum(names(sort(
      #      rowMeans(MUVRrdCVclassObject$VIRankPerRep[[nModel]][,1:i,drop=F]))[1:nV[i]])%in%VAll)
    }


    if (DA == TRUE) {
      ############discuss the scenario of DA
      if (!any(class(MUVRrdCVclassObject) == 'rdCVnet')) {
        predsRep <- MUVRrdCVclassObject$yPredPerRep[[nModel]][, , i, drop = F]
      } else{
        predsRep <- MUVRrdCVclassObject$yPredPerRep[, , i, drop = F]
      }
      ##row is observations, column is groups, The first value is for the first repetition,
      ##it will be substituted for each i

      ###balance error rate
      berRep[i] <-
        getBER(predicted = levels(Y)[apply(predsRep, 1, which.max)],
               actual = Y)

      missRep[i] <- sum(levels(Y)[apply(predsRep, 1, which.max)] != Y)
      ##a number for each repetition, which is miss classification numbers
      if (!any(class(MUVRrdCVclassObject) == 'rdCVnet')) {
        preds <- MUVRrdCVclassObject$yPredPerRep[[nModel]][, , 1:i]
      } else{
        preds <- MUVRrdCVclassObject$yPredPerRep[, , 1:i]
      }
      ###for the first i repetitions, it is a list

      preds <- apply(preds, c(1, 2), mean)
      ###mean for first i repetitions,output is a matrix (row is observarion, column is group)
      ber[i] <-
        getBER(predicted = levels(Y)[apply(preds, 1, which.max)],
               actual = Y)
      miss[i] <- sum(levels(Y)[apply(preds, 1, which.max)] != Y)
      ##  ##a number for first i repetition, which is miss classification numbers

    } else {
      if (!any(class(MUVRrdCVclassObject) == 'rdCVnet')) {
        predsRep <- MUVRrdCVclassObject$yPredPerRep[[nModel]][, i, drop = F]
      } else{
        predsRep <- MUVRrdCVclassObject$yPredPerRep[, i, drop = F]

      }

      ###each repetition,The first value is for the first repetition, it will be substituted
      ##row is all observations, column is one column, which is repetition i
      PRESS <- sum((Y - predsRep) ^ 2)
      TSS <- sum((Y - mean(Y)) ^ 2)
      q2Rep[i] <- 1 - (PRESS / TSS)  ###one q2 for each repetition

      if (!any(class(MUVRrdCVclassObject) == 'rdCVnet')) {
        preds <- MUVRrdCVclassObject$yPredPerRep[[nModel]][, 1:i, drop = F]
      } else{
        preds <- MUVRrdCVclassObject$yPredPerRep[, 1:i, drop = F]
      }

      ####row is all observations, column is first i repetitions
      preds <-
        rowMeans(preds)   ###row means,  row is all observations, column is one column,
      PRESS <- sum((Y - preds) ^ 2)
      TSS <- sum((Y - mean(Y)) ^ 2)
      q2[i] <- 1 - (PRESS / TSS)    ##one q2 for first i repetitions
    }

    if (ML == TRUE) {
      class <- ifelse(preds < 0, -1, 1)
      miss[i] <- sum(class != Y)
      ber[i] <- getBER(predicted = class,
                       actual = Y)
    }
  }


  if (!any(class(MUVRrdCVclassObject) == 'rdCVnet')) {
    VARep <-
      VARep / length(VAll)    ####Originally, Each VARep[i] is a vector of variable names that is in VALL. Each repetition has a vector
    ##output is a percentage
    VA <- VA / length(VAll)
    ##########################################################################################################

    if (missing(nVarLim)) {
      pot <- 10 ^ floor(log10(max(nV))) ###number become smaller
      nVarLim <-
        ceiling(max(c(nV, nVRep)) / pot) * pot ###nV and nVRep both has the same length as nREP
      ###number becomes bigger
    }

  } else{
    VARep <-
      VARep / length(VAll)    ####Originally, Each VARep[i] is a vector of variable names that is in VALL. Each repetition has a vector
    ##output is a percentage
    VA <- VA / length(VAll)
    if (missing(nVarLim)) {
      pot <- 10 ^ floor(log10(max(nV))) ###number become smaller
      nVarLim <-
        ceiling(max(c(nV, nVRep)) / pot) * pot ###nV and nVRep both has the same length as nREP
      ###number becomes bigger
    }
  }


  ######################################################################################################
  ##takes a single numeric argument x and returns a numeric vector containing the smallest integers
  ##not less than the corresponding elements of x.
  if (any(class(MUVRrdCVclassObject) == 'rdCVnet')) {
    nPlot <- ifelse(ML, 5, ifelse(regr, 3, 4))
    par(mfrow = c(nPlot, 1))
    par(mar = c(3, 4, 0, 0) + .5)
  } else{
    nPlot <- ifelse(ML, 5, ifelse(regr, 3, 4))
    par(mfrow = c(nPlot, 1))
    par(mar = c(3, 4, 0, 0) + .5)
  }

  ######################
  #Plot 1 Number of selected variables vs number of repetions
  #if(!any(class(MUVRrdCVclassObject)=='rdCVnet')){
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
    if (missing(missLim)) {
      missLim = length(Y)
    }
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
