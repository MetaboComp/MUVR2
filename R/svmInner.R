#' Support vector machine model in inner CV loop
#' @param xTrain Training data (samples as rows; variables as columns)
#' @param yTrain Training response
#' @param xVal Validation data
#' @param yVal Validation response (f´´or tuning)
#' @param DA Logical for discriminant analysis (classification)
#' @param fitness Fitness function ('MISS', 'AUROC' or 'RMSEP')
#' @param kernel What kernel to choose: polynomial, radical, linear, sigmoid
#' @param method Choice of Support vector machine implementation ("svm", "ksvm", "svmlight"). Passed from wrapper.
## @param nu An upper bound on the fraction of training errors and a lower bound of the fraction of support vectors". (0,1)
## @param gamma
## @param degree
#' @return An object containing:(`miss`,`ber`,`auc` or `rmsep`) A fitness metric and `virank` variable importance rankings
#' @export
svmInner <- function(xTrain,
                     yTrain,
                     xVal,
                     yVal,
                     DA,
                     fitness,
                     kernel,
                     #nu,
                     #gamma,
                     #degree,
                     method) {
  library(rminer)
  library(kernlab)
  library(e1071)
  returnIn <- list()
  ###Put it in the main MUVR
  xTrain <- as.data.frame(xTrain)
  xVal <- as.data.frame(xVal)
  data <- cbind(xTrain, yTrain)
  if (DA == FALSE) {
    if (method == "svm") {
      svmModIn <- train(
        x = xTrain,
        y = yTrain,
        method = "svmLinear2",
        preProcess = "scale",
        #trControl=trainControl( method = "repeatedcv"),
        #kernel="radial",
        scale = FALSE,
        nu = 0.1,
        ### not tuning it
        #tuneGrid=tuneGrid,
        #cost=2,
        type = "nu-regression"   ## this needs to be specificed with type at the same time
      )
      importance <- varImp(svmModIn, scale = FALSE)
      yValInner <- predict(svmModIn, xVal)
      returnIn$virank <- importance[[1]][colnames(xTrain), , drop = FALSE]
      returnIn$virank <- returnIn$virank[, 1]
      names(returnIn$virank) <- colnames(xTrain)
      returnIn$virank <- rank(-returnIn$virank)
    }

    if (method == "ksvm") {
      #####Test for stability
      ###doing nu regression
      ##
      svmModIn <- rminer::fit(
        yTrain ~ .,
        data = data,
        model = "ksvm",
        ##ranking could different in different kernel
        task = "reg",
        #search=list(search=list(nu=seq(0.001,0.9,0.001))),   ### parameter tuning
        nu = 0.1,
        ## without parameter tuning
        type = "nu-svr",
        #kpar=list(sigma=gamma),
        kernel = kernel
      )
      b <- ksvm(yTrain ~ .,
                data = data,
                type = "nu-svr",
                nu = svmModIn@mpar$nu)
      yValInner <- predict(b, xVal)
      svm_imp <- Importance(svmModIn,
                            data = xTrain)
      names(svm_imp$imp) <- colnames(xTrain)
      returnIn$virank <- rank(-svm_imp$imp)


      #svmModIn <- svm(
      # yTrain ~ .,
      # kernel=kernel,
      # scale=FALSE,
      # type="nu-regression",
      # data = data,
      # nu = nu,
      # gamma = gamma,
      # kernel = kernel,
      #
      # )


    }

  }

  if (DA == TRUE) {
    if (method == "svm") {
      library(caret)
      ################################################################################################33
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
          xVal <- xVal[!is.na(factor(yVal, levels = levels(yTrain))), ]
          yVal <-
            factor(yVal[!is.na(factor(yVal, levels = levels(yTrain)))], levels = levels(yTrain))

        }
      }
      #############################################################################################################
      svmModIn <- train(
        x = xTrain,
        y = yTrain,
        method = "svmLinear2",
        preProcess = "scale",
        #trControl=trainControl(method = "repeatedcv"),  ### parameter tuning
        #kernel="radial",
        scale = FALSE,

        #tuneGrid=expand.grid(cost=c(0.001,0.01,0.1,1,10,100,1000)),
        #tuneGrid=expand.grid(cost=c(1,10)),
        # C=1,
        # type="C-svc"
        nu = 0.5,
        ## without parameter tuning
        type = "nu-classification"   ## this needs to be specificed with type at the same time
      )
      importance <- varImp(svmModIn, scale = FALSE)
      yValInner <- predict(svmModIn, xVal)
      returnIn$virank <- importance[[1]][colnames(xTrain), , drop = FALSE]

      for (i in 1:ncol(returnIn$virank)) {
        returnIn$virank[, i] <- rank(-returnIn$virank[, i])

      }
      returnIn$virank <- rowSums(returnIn$virank)
      returnIn$virank <- rank(returnIn$virank)


    }
    if (method == "ksvm") {
      #####Test for stability
      svmModIn <- rminer::fit(
        yTrain ~ .,
        data = data,
        model = "ksvm",
        task = "class",
        #search=list(search=list(nu=seq(0.001,0.9,0.001))),  ### parameter tuning
        #nu=0.5,                                ### without parameter tuning
        #type="nu-svc",
        C = 1,
        type = "C-svc",
        #kpar=list(sigma=gamma),
        kernel = kernel
      )
      #b<-ksvm(yTrain~.,data=data,type="nu-svc",nu=svmModIn@mpar$nu)
      b <- ksvm(yTrain ~ .,
                data = data,
                type = "C-svc",
                C = svmModIn@mpar$C)
      yValInner <- predict(b, xVal)
      svm_imp <-
        Importance(svmModIn,   ### it takes sometime to calculate it
                   data = xTrain)

      #####somehow it gives NA

      names(svm_imp$imp) <- colnames(xTrain)   ##should I add here
      svm_imp$imp <- svm_imp$imp[!is.na(names(svm_imp$imp))]
      returnIn$virank <- rank(-svm_imp$imp)


      #svmModIn <- svm(
      # yTrain ~ .,
      # kernel=kernel,
      # scale=FALSE,
      # type="nu-regression",
      # data = data,
      # nu = nu,
      # gamma = gamma,
      # kernel = kernel,
      #
      # )

    }

  }

  ####tune for gamma and cost


  if (fitness == 'MISS') {
    # cat(' miss',count)
    if (DA) {
      returnIn$miss <- sum(yValInner != yVal)
    }
    else {
      ###ML
      yClassInner <-
        ifelse(yValInner > 0, 1, -1)   #####classification binary??????Why?????
      returnIn$miss <- sum(yClassInner != yVal)
    }
  }

  if (fitness == 'BER') {
    returnIn$ber <- getBER(actual = yVal,
                           predicted = yValInner)  ###getBER from MUVR
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
