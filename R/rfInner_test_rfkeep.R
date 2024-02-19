#' RF model in inner CV loop  test version, the original version is saved in rfInner_orignal
#' @param xTrain Training data (samples as rows; variables as columns)
#' @param yTrain Training response
#' @param xVal Validation data
#' @param yVal Validation response (for tuning)
#' @param DA Logical for discriminant analysis (classification)
#' @param fitness Fitness function ('MISS', 'AUROC' or 'RMSEP')
#' @param ntree See original function (`randomForest`). Passed from wrapper.
#' @param method Choice of Random Forest implementation (randomForest, ranger or Rborist). Passed from wrapper.
#' @param mtry See original function (`randomForest`). Passed from wrapper.
#' @param weigh_added To add a weighing matrix when it is classfication
#' @param weighing_matrix The matrix used for get a miss classfication score
#' @param keep_variables The variables to be kept
#' @return An object containing:
#' @return (`miss`, `auc` or `rmsep`) A fitness metric
#' @return `virank` variable importance rankings
#' @export
#'
rfInner_test_rfkeep <- function(xTrain,
                    yTrain,
                    xVal,
                    yVal,
                    DA,
                    fitness,
                    ntree,
                    mtry,
                    method,
                    weigh_added,
                    weighing_matrix,
                    keep_variables=NULL) {
  # Allocate return object
  returnIn <- list()
  if (missing(method)) {
    method <- "randomForest"
  }
  # Different functions for different RF implementations
  if (method == 'randomForest') {
    # safeguard measure in case some class is not appear after separating segment
    if (is.factor(yTrain)) {
      if (length(levels(yTrain)) != length(levels(droplevels(yTrain)))) {
        #xTrain<-Xotu[1:15,]
        #yTrain<-Yotu[1:15]
        #xVal<-Xotu[16:29,]
        #yVal<-Yotu[16:29]
        #original_levels_yTrain<-levels(yTrain)
        #original_levels_yVal<-levels(yVal)
        #real_levels_yTrain<-levels(droplevels(yTrain))
        #real_levels_yVal<-levels(droplevels(yVal))
        yTrain <- droplevels(yTrain)
        xVal <-
          xVal[!is.na(factor(yVal, levels = levels(yTrain))), ]
        yVal <-
          factor(yVal[!is.na(factor(yVal, levels = levels(yTrain)))], levels = levels(yTrain))

      }
    }
    rfModIn <- randomForest(xTrain,
                            yTrain,
                            xVal,
                            yVal,
                            ntree = ntree,
                            mtry = mtry)
    #ntree: Number of trees to grow. This should not be set to too small a number,
    #to ensure that every input row gets predicted at least a few times.
    #mtry: Number of variables randomly sampled as candidates at each split. Note that the default values
    #are different for classification (sqrt(p) where p is number of variables in x) and regression (p/3)

    # Extract predictions
    yValInner <- rfModIn$test$predicted

    # Variable importance rank
    returnIn$virank <- rank(-rfModIn$importance)
    names(returnIn$virank) <- rownames(rfModIn$importance)
    #Importance:a matrix with nclass + 2 (for classification) or two (for regression) columns.
    #For classification, the first nclass columns are the class-specific measures computed as mean descrease in accuracy.
    #The nclass + 1st column is the mean descrease in accuracy over all classes.
    #The last column is the mean decrease in Gini index.
    #For Regression, the first column is the mean decrease in accuracy and the second the mean decrease in MSE.
    #If importance=FALSE, the last measure is still returned as a vector

  } else if (method == 'ranger') {
    rfModIn <- ranger(
      x = xTrain,
      y = yTrain,
      num.trees = ntree,
      importance = 'impurity',
      # respect.unordered.factors = 'order', # Sort this out as "ifany"
      #The 'impurity' measure is the Gini index for classification, the variance of the
      #responses for regression and the sum of test statistics (see splitrule) for survival.
      mtry = mtry,
      always.split.variables = keep_variables
    )
    # Extract predictions
    yValInner <- predict(rfModIn, data = xVal)$predictions
    # Variable importance rank
    returnIn$virank <- rank(-rfModIn$variable.importance)
  } else {
    stop('other RF methods not yet implemented')
  }


  #####################################################################################################
  #In the main MUVR model, for the relationship with Y data type and DA value:
  #When DA=TRUE, y is factor, run
  #When DA=TRUE, y is numeric, y is transformed to factor. Is there a step in the main MUVR that check for the maximum number of classes?
  #When Y is factor, DA must be TRUE
  #When Y is numeric, DA=FALSE, run
  #No combination of DA=FALSE and Y is factor
  #
  #Here I have some questions
  # From what I understand, MISS, BER, AUROC are for classifications, RSMEP is for regression
  #Problem1: When fitness is MISS,
  #       yValInner could be more than 2 category and the value could vary, Why set 0 as the cut off for predicted values?
  #
  #Problem 2: When fitness is BER, what if Y is numeric (continuous data)?
  #Problem 3: When fitness is AUROC,what if Y is numeric
  #Problem 4: When fitness is RMSEP, what is Y is factor?
  #
  #Problem 5: when method=randomForest, yValInner <- rfModIn$test$predicted, what about $voted for classification
  #Problem 6:What is the Difference between rfInner and rfPredict? For me the only differences seems to be that rfInner calculated the fitness
  #######################################################################################################################################


  if (fitness == 'MISS' | fitness == 'wMISS') {
    # cat(' miss',count)
    if (DA == TRUE) {
      returnIn$miss <- sum(yValInner != yVal)
      if (fitness == 'wMISS') {
        returnIn$wmiss <- getMISS(
          actual = yVal,
          predicted = yValInner,
          weigh_added = weigh_added,
          weighing_matrix = weighing_matrix
        )

      }
    }

    else {
      ###ML
      yClassInner <-
        ifelse(yValInner > 0, 1, -1)   #####classification binary??????Why?????
      returnIn$miss <- sum(yClassInner != yVal)
      if (fitness == 'wMISS') {
        returnIn$wmiss <- getMISS(
          actual = yVal,
          predicted = yValInner,
          weigh_added = weigh_added,
          weighing_matrix = weighing_matrix
        )

      }
    }
  }

  if (fitness == 'BER' | fitness == 'wBER') {
    returnIn$ber <- getBER(actual = yVal,
                           predicted = yValInner)  ###getBER from MUVR

    if (weigh_added == FALSE) {
      weighing_matrix <-
        diag(1, length(levels(yVal)), length(levels(yVal)))
    } else{
      if (is.null(weighing_matrix)) {
        #   warning("Missing weighing_matrix,weighing_matrix will be diagnoal")
        weighing_matrix <-
          diag(1, length(levels(yVal)), length(levels(yVal)))
      }
      if (dim(weighing_matrix)[1] != length(levels(yVal))) {
        stop("The dimension of weighing_matrix is not correct")
      }
      if (dim(weighing_matrix)[2] != length(levels(yVal))) {
        stop("The dimension of weighing_matrix is not correct")
      }
      for (i in 1:nrow(weighing_matrix)) {
        if (weighing_matrix[i, i] != 1) {
          stop("diagonal values must be 1")
        }
        for (j in 1:ncol(weighing_matrix)) {
          if (weighing_matrix[i, j] < 0 | weighing_matrix[i, j] > 1) {
            stop("Values in the weighing matrix must between 0 and 1")
          }
        }
      }
    }

  }
  if (fitness == 'wBER') {
    returnIn$wber <- getBER(
      actual = yVal,
      predicted = yValInner,
      weigh_added = weigh_added,
      weighing_matrix = weighing_matrix
    )  ###getBER from MUVR
  }
  ##Balance error rate


  if (fitness == 'AUROC') {
    returnIn$auc <- roc(yVal,
                        rfModIn$test$votes[, 1])$auc  ####what is votes
  }
  # if test set is given (through the xtest or additionally ytest arguments), this component is a list which contains the
  # corresponding predicted, err.rate, confusion, votes (for classification) or predicted, mse and rsq (for regression) for
  # the test set. If proximity=TRUE, there is also a component, proximity, which contains the proximity among the test set
  # as well as proximity between test and training data.
  if (fitness == 'RMSEP') {
    returnIn$rmsep <-
      sqrt(sum((yVal - yValInner) ^ 2, na.rm = TRUE) / (length(yVal) - sum(is.na(yValInner))))
  }
  ##RMSEP root mean squared error of prediction

  return(returnIn)
}
