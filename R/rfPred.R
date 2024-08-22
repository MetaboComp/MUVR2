#' RF predictions for outer segments and consensus models
#' @param xTrain xTrain
#' @param yTrain yTrain
#' @param xTest xTest
#' @param yTest yTest
#' @param ntree ntree
#' @param keep.forest keep.forest
#' @param method method
#' @param DA DA
#' @return  The predicted value of yTest
#' @examples
#' \dontrun{
#' data("freelive2")
#'xTrain<-XRVIP2[1:40,]
#'yTrain<-YR2[1:40]
#'xTest<-XRVIP2[41:nrow(XRVIP2),]
#'yTest<-YR2[41:length(YR2)]
#'rfPred_object<-
#'  rfPred(xTrain=xTrain,
#'         yTrain=yTrain,
#'         xTest=xTest,
#'         yTest=yTest,
#'         DA= FALSE)
#' }
#' @noRd
rfPred <- function(xTrain,
                   yTrain,
                   xTest,
                   yTest,
                   ntree = 500,
                   DA,
                   keep.forest = FALSE,
                   #If set to FALSE, the forest will not be retained in the output object. If xtest is given, defaults to FALSE.
                   method) {
  # Allocate return object
  #library(randomForest)
  return <- list()
  if (missing(method)) {
    method = "randomForest"
  }
  # Use "Train" for "Testing" if lacking (for fit-predict)
  if (missing(xTest)) {
    xTest <- xTrain
    yTest <- yTrain
  }

  if (method == 'randomForest') {
    ## safeguard measure in case some class is not appear after separating segment
    if (is.factor(yTrain)) {
      if (length(levels(yTrain)) != length(levels(droplevels(yTrain)))) {
        #xTrain<-Xotu[1:15,]
        #yTrain<-Yotu[1:15]
        #xTest<-Xotu[16:29,]
        #yTest<-Yotu[16:29]
        #original_levels_yTrain<-levels(yTrain)
        #original_levels_yTest<-levels(yTest)
        #real_levels_yTrain<-levels(droplevels(yTrain))
        #real_levels_yTest<-levels(droplevels(yTest))
        yTrain <- droplevels(yTrain)
        xTest <- xTest[!is.na(factor(yTest, levels = levels(yTrain))), ]
        yTest <-
          factor(yTest[!is.na(factor(yTest, levels = levels(yTrain)))], levels = levels(yTrain))

      }
    }
    return$model <- randomForest(
      x = xTrain,
      y = yTrain,
      xtest = xTest,
      ytest = yTest,
      ntree = ntree,
      keep.forest = keep.forest
    )

    ########################################################################################################
    # What is votes value? If this is for classification, Why votes is not used in rfInner

    #########################################################################################################
    if (DA == TRUE) {
      return$fit <-
        return$model$votes    ###(classification only) a matrix with one row for each input data point and one column for each class,
      ###giving the fraction or number of (OOB) `votes' from the random forest.
      return$predicted <-
        return$model$test$votes  ###what is test$vote
    } else {
      return$fit <- return$model$predicted   ###this is to predict y Train
      return$predicted <-
        return$model$test$predicted  ##This is to predict ytest
    }
  } else if (method == 'ranger') {
    probability <- ifelse(DA, TRUE, FALSE)
    return$model <- ranger(
      x = xTrain,
      y = yTrain,
      num.trees = ntree,
      importance = 'impurity',
      probability = probability,
      #Grow a probability forest as in Malley et al. (2012).
      # respect.unordered.factors = 'order', # Sort out criterion for "if there are any"
      write.forest = keep.forest
    )
    #Save ranger.forest object, required for prediction. Set to FALSE to reduce memory usage if no prediction intended.
    # Extract predictions
    return$fit <- return$model$predictions
    if (keep.forest) {
      return$predicted <- predict(return$model, data = xTest)$predictions
    }
  } else {
    stop('other RF methods not yet implemented')
  }
  return(return)
}
