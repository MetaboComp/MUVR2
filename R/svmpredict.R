#' RF predictions for outer segments and consensus models
#' @param xTrain xTrain
#' @param yTrain yTrain
#' @param xTest xTest
#' @param yTest yTest
#' @param method method
#' @param kernel The build in method of svm "linear","radical","polynomial","sigmoid"
## @param nu
## @param degree
## @param gamma
#' @param DA DA or not, coming form MUVR
#' @return  The predicted value of yTest
#' @export
svmpredict <- function(xTrain,
                       yTrain,
                       xTest,
                       yTest,
                       method = c("svm", "ksvm", "svmlight"),
                       kernel = c("vanilladot", "polydot", "rbfdot"),
                       #nu,
                       #degree,
                       #gamma,
                       DA) {
  library(rminer)
  library(kernlab)
  library(e1071)
  # Allocate return object
  return <- list()
  if (missing(xTrain) |
      missing(yTrain)) {
    stop("There must be x and y training data")
  }
  if (missing(xTest)) {
    xTest <- xTrain
    yTest <- yTrain
  }
  #dat<-data.frame(x=xTrain,
  #                y=yTrain)                           #####one thing to notice is that when using svm, y must be a factor
  # Use "Train" for "Testing" if lacking (for fit-predict)

  if (class(yTest) != class(yTrain)) {
    stop("yTrain must be the same class as yTest")
  }
  #tesdat<-data.frame(x=xTest,
  #                   y=yTest)

  if (missing(method))
  {
    method <- "ksvm"
  }
  if (!method %in% c("svm", "ksvm", "svmlight"))
  {
    stop("This method is not applied")
  }
  ##some notes
  ##for svm, when y is factor, it automatically use C-classification, when y is numeric, it use eps regression

  ##C-classification: the range  is for range 0 ,nu-classification is for range(0,1)
  ##svm has cross validation function build inside,defalut is cross=0, To use that one need to specified cross= in svm()


  #####method 1
  if (method == "svm") {
    library(e1071)
    if (missing(kernel)) {
      kernel <- "radial"
    }
    if (!kernel %in% c("linear", "radial", "polynomial", "sigmoid"))
    {
      stop("This method is not included in svm")
    }
    xTrain <- as.data.frame(xTrain)
    xTest <- as.data.frame(xTest)
    data <- cbind(xTrain, yTrain)
    if (DA == FALSE) {
      return$model <- train(
        x = xTrain,
        y = yTrain,
        method = "svmLinear2",
        preProcess = "scale",
        trControl = trainControl(method = "none"),
        #kernel="radial",
        scale = FALSE,
        nu = 0.1,
        #tuneGrid=tuneGrid,
        #cost=2,
        type = "nu-regression"   ## this needs to be specificed with type at the same time
      )


      return$fit <- predict(return$model , xTrain)
      return$predicted <- predict(return$model , xTest)
    }

    if (DA == TRUE) {
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

      return$model <- train(
        x = xTrain,
        y = yTrain,
        method = "svmLinear2",
        preProcess = "scale",
        #trControl=trainControl(method = "repeatedcv"),  ### parameter tuning
        #kernel="radial",
        scale = FALSE,
        #cost=1,
        #type="C-classification"
        #tuneGrid=expand.grid(cost=c(0.001,0.01,0.1,1,10,100,1000)),

        nu = 0.5,
        ## without parameter tuning
        type = "nu-classification"   ## this needs to be specificed with type at the same time
      )

      return$fit <- predict(return$model , xTrain)
      return$predicted <- predict(return$model , xTest)
    }
    #if (missing(type)) {
    #  if (class(Y) == "numeric"){type = "nu-Classification"}
    #  if (class(Y) == "numeric" | class(Y) == "integer"){type = "nu-regression"}
    #}

    #if (!type %in% c("nu-classification","nu-regression" ))
    #{ stop("This type is not included in svm")}

    #if (type %in% c("nu-classification")) {
    #  if (class(Y) != "factor") { stop("dependent variable has to be of factor type for classification mode.")}
    #}
    #if (type %in% c( "nu-regression")) {
    #  if (class(Y) != "numeric" | class(Y) != "integer") { stop("Need numeric dependent variable for regression")}
    #}
    ###########################################
  }
  ####for ksvm there are 9 types in total. For simplicity, I will just include the method that is included in svm
  ####method 2

  if (method == "ksvm")
  {
    library(kernlab)
    if (missing(kernel)) {
      kernel <- "vanilladot"
    }
    if (!kernel %in% c("vanilladot", "polydot", "rbfdot"))
    {
      stop("This method is not included in svm")
    }
    xTrain <- as.data.frame(xTrain)
    xTest <- as.data.frame(xTest)
    data <- cbind(xTrain, yTrain)
    if (DA == FALSE) {
      M <- rminer::fit(
        yTrain ~ .,
        data = data,
        model = "ksvm",
        ##ranking could different in different kernel
        task = "reg",
        #search=list(search=list(nu=seq(0.001,0.9,0.001))),
        nu = 0.1,
        type = "nu-svr",
        #kpar=list(sigma=gamma),
        kernel = kernel
      )
      return$model <- M
      a <- ksvm(yTrain ~ .,
                data = data,
                type = "nu-svr",
                nu = M@mpar$nu)
      return$fit <- predict(a, xTrain)
      return$predicted <- predict(a , xTest)
    }

    if (DA == TRUE) {
      M <- rminer::fit(
        yTrain ~ .,
        data = data,
        model = "ksvm",
        ##ranking could different in different kernel
        task = "class",
        #search=list(search=list(nu=seq(0.001,0.9,0.001))),
        #nu=0.5,
        #type="nu-svc",
        C = 1,
        type = "C-svc",

        #kpar=list(sigma=gamma),
        kernel = kernel
      )
      return$model <- M
      # a<-ksvm(yTrain~.,data=data,type="nu-svc",nu=M@mpar$nu)
      a <- ksvm(yTrain ~ .,
                data = data,
                type = "C-svc",
                C = M@mpar$C)
      return$fit <- predict(a, xTrain)
      return$predicted <- predict(a , xTest)
    }

  }



  return(return)
}
