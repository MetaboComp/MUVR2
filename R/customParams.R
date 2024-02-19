#' Make custom parameters for MUVR internal modelling,not rdCV
#' Please note that, at present, there is no mtryMax for the outer (consensus) loop in effect
#' @param method PLS or RF (default)
#' @param robust Robustness (slack) criterion for determining min and max knees (defaults to 0.05)
#' @param ntreeIn RF parameter: Number of trees in inner cross-validation loop models (defaults to 150)
#' @param ntreeOut RF parameter: Number of trees in outer (consensus) cross-validation loop models (defaults to 300)
#' @param mtryMaxIn RF parameter: Max number of variables to sample from at each node in the inner CV loop (defaults to 150). Will be further limited by standard RF rules (see randomForest documentation)
#' @param compMax PLS parameter: Maximum number of PLS components (defaults to 5)
#' @param neuralMaxIn  ann parameter: Maximum number of ANN (defaults to 20)
#' @param nodes ann parameter:
#' @param threshold ann parameter:
#' @param stepmax ann parameter:
#' @param oneHot  TRUE or FALSE using onehot endcoding or not
#' @param scoring_matrix  A matrix that its score can be adjusted
#' @param NZV TRUE or FALSE using NZV or not
#' @param rfMethod randomforest method, which includes randomForest and ranger
#' @param svmMethod support vector machine method, which includes svm, ksvm, s
#' @param kernel svm parameter: kernal function to use, which includes sigmoid, radical, polynomial
#' @param nu svm parameter: ratios of errors allowed in the training set range from 0-1
#' @param degree svm parameter: needed for polynomial kernel in svm
#' @param gamma svm parameters: needed for "vanilladot","polydot","rbfdot" kernel in svm
#' @param annMethod artificail neural network method which include 2 different ann methods
#' @return a `methParam` object
#' @export
#'
#' @examples
#' # Standard parameters for random forest
#' methParam <- customParams() # or
#' methParam <- customParams('RF')
#' # Custom `ntreeOut` parameters for random forest
#' methParam <- customParams('RF',ntreeOut=50) # or
#' methParam <- customParams('RF')
#' methParam$ntreeOut <- 50
#' methParam
customParams <- function(method = c('RF', 'PLS', "SVM", "ANN"),
                         robust = 0.05,
                         ntreeIn = 150,
                         ntreeOut = 300,
                         mtryMaxIn = 150,
                         compMax = 5,
                         nodes = 200,
                         threshold = 0.1,
                         ###neuralnet default
                         stepmax = 1e+08,
                         ###neuralnet default
                         neuralMaxIn = 10,
                         kernel = "notkernel",
                         nu = 0.1,
                         gamma = 1,
                         degree = 1,
                         scoring_matrix = FALSE,
                         oneHot,
                         NZV,
                         rfMethod = c('randomForest', 'ranger'),
                         svmMethod = c("svm", "ksvm", "svmlight"),
                         annMethod = c("nnet", "neuralnet")) {
  # Allocate methParam object
  methParam <- list(robust = robust)

  # Random Forest as default method
  ###########################################################################################################################
  ##when method is missing
  if (missing(method)) {
    if (!missing(rfMethod) &
        !missing(svmMethod)) {
      stop("There could only be one method")
    }
    if (!missing(annMethod) &
        !missing(svmMethod)) {
      stop("There could only be one method")
    }
    if (!missing(rfMethod) &
        !missing(annMethod)) {
      stop("There could only be one method")
    }
    if (missing(rfMethod) & missing(svmMethod) &
        missing(annMethod)) {
      rfMethod <- "randomForest"
      methParam$rfMethod <-
        "randomForest"  ## This is the output not the input argument
      methParam$method <- "RF"
    }

    if (!missing(rfMethod))
    {
      if (rfMethod != "randomForest" & rfMethod != "ranger")
      {
        stop('other rfMethods not implemented')
      }
      methParam$method <- "RF"
      methParam$ntreeIn <- ntreeIn
      methParam$ntreeOut <- ntreeOut
      methParam$mtryMaxIn <- mtryMaxIn
      methParam$rfMethod <- rfMethod

    } else if (!missing(svmMethod)) {
      if (svmMethod != "svm" &
          svmMethod != "ksvm" & svmMethod != "svmlight")
      {
        stop('other svmMethods not implemented')
      }
      methParam$method <- "SVM"
      methParam$svmMethod <- svmMethod

      methParam$nu <- nu
      methParam$gamma <- gamma
      methParam$degree <- degree
      if (kernel == "notkernel") {
        if (methParam$svmMethod == "svm") {
          kernel <- "radial"
        } else if (methParam$svmMethod == "ksvm") {
          kernel <- "vanilladot"
        }
      }
      methParam$kernel <- kernel
      ####Here new method could be added
    } else if (!missing(annMethod)) {
      if (annMethod != "nnet" & annMethod != "neuralnet")
      {
        stop('other annMethods not implemented')
      }
      methParam$method <- "ANN"
      methParam$annMethod <- annMethod
      methParam$nodes <- nodes
      methParam$threshold <- threshold
      methParam$stepmax <- stepmax
    }
    else{
      methParam$method <- "PLS"
      methParam$compMax <- compMax
    }

  }

  ##when method is not missing but is not the method included in the function
  if (!missing(method))
  {
    if (method != "RF" &
        method != "PLS" &
        method != "SVM" &
        method != "ANN")
    {
      stop('other methods not implemented')
    }

    methParam$method <- method

    if (method == "RF")
    {
      if (!missing(svmMethod)) {
        stop('Method is RF. There should not be svmMethod')
      }
      if (!missing(annMethod)) {
        stop('Method is RF. There should not be annMethod')
      }
      if (missing(rfMethod))
      {
        rfMethod <- "randomForest"
        methParam$rfMethod <- "randomForest"

      } else{
        if (rfMethod != "randomForest" & rfMethod != "ranger")
        {
          stop('other rfMethods not implemented')
        }
        methParam$rfMethod <- rfMethod
      }
      methParam$ntreeIn <- ntreeIn
      methParam$ntreeOut <- ntreeOut
      methParam$mtryMaxIn <- mtryMaxIn
    }

    if (method == "PLS") {
      if (!missing(svmMethod)) {
        stop('Method is PLS. There should not be svmMethod')
      }
      if (!missing(rfMethod)) {
        stop('Method is PLS. There should not be rfMethod')
      }
      if (!missing(annMethod)) {
        stop('Method is PLS. There should not be annMethod')
      }
      #########################
      # Default PLS parameters
      #########################

      methParam$compMax <- compMax
    }

    if (method == "ANN")
    {
      if (!missing(rfMethod)) {
        stop('Method is ANN. There should not be rfMethod')
      }
      if (!missing(svmMethod)) {
        stop('Method is ANN. There should not be svmMethod')
      }
      if (missing(annMethod)) {
        methParam$annMethod <- "neuralnet"

      } else{
        if (annMethod != "nnet" & annMethod != "neuralnet") {
          stop("This annMethod not implemented")
        }
        methParam$annMethod <- annMethod

      }

      methParam$nodes <- nodes
      methParam$threshold <- threshold
      methParam$stepmax <- stepmax
      methParam$neuralMaxIn <- neuralMaxIn
    }

    if (method == "SVM")
    {
      if (!missing(rfMethod)) {
        stop('Method is SVM. There should not be rfMethod')
      }
      if (!missing(annMethod)) {
        stop('Method is SVM. There should not be annMethod')
      }
      if (missing(svmMethod)) {
        methParam$svmMethod <- "ksvm"

      } else{
        if (svmMethod != "svm" &
            svmMethod != "ksvm" & svmMethod != "svmlight") {
          stop("This svmMethod not implemented")
        }
        methParam$svmMethod <- svmMethod

      }
      methParam$kernel <- kernel
      methParam$nu <- nu
      methParam$gamma <- gamma
      methParam$degree <- degree

      if (kernel == "notkernel") {
        if (methParam$svmMethod == "svm") {
          kernel <- "radial"
        } else if (methParam$svmMethod == "ksvm") {
          kernel <- "vanilladot"
        }
        methParam$kernel <- kernel
      }
    }


  }

  ##when oneHot i snot missing but not TRUE& FALSE
  if (!missing(oneHot))
  {
    if (oneHot != TRUE & oneHot != FALSE)
    {
      stop('oneHot can only be defined as TRUE or FAlSE')
    }
  }
  ##when NZV i snot missing but not TRUE& FALSE
  if (!missing(NZV))
  {
    if (NZV != TRUE & NZV != FALSE)
    {
      stop('NZV can only be defined as TRUE or FAlSE')
    }
  }

  # Default oneHot values per method
  if (missing(oneHot)) {
    if (methParam$method == 'PLS') {
      oneHot <- TRUE
    } else if (methParam$method == 'RF') {
      oneHot <- FALSE
    } else if (methParam$method == "SVM")
    {
      oneHot <- FALSE
    } else  {
      oneHot <- FALSE
    }
  }

  methParam$oneHot <- oneHot
  # Default oneHot values per method
  if (missing(NZV)) {
    if (methParam$method == 'PLS') {
      NZV <- TRUE
    } else if (methParam$method == 'RF') {
      NZV <- FALSE
    } else if (methParam$method == "SVM")
    {
      NZV <- TRUE
    } else{
      NZV <- TRUE
    }
  }
  methParam$NZV <- NZV

  if (methParam$method == "PLS" &
      oneHot == FALSE) {
    stop("PLS method must use oneHot encoding. ")
  }
  if (methParam$method == "PLS" &
      NZV == FALSE) {
    stop("PLS method must use near zero variance. ")
  }

  #########################
  # Default RF parameters
  # For PLS oneHot and NZV can only be true, for RF, the default value is set as TRUE but can be changed to FALSE
  #########################


  return(methParam)
}
