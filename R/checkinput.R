#'This can be run to test if the command input of parameters contradict to each other
#'And also check the structure of the data
#'If something goes wrong warning messages are given
#'@param X   The original data of X, not the result after onehotencoding
#'@param Y   The original data of Y
#'@param ML  ML in MUVR2
#'@param DA  DA in MUVR2
#'@param method  RF or PLS so far in MUVR2
#'@param fitness fitness in MUVR2
#'@param nInner nInnerin MUVR2
#'@param nOuter nOuter in MUVR2
#'@param varRatio varRatio in MUVR2
#'@param scale scale
#'@param modReturn modReturn in MUVR2
#'@param logg logg in MUVR2
#'@param parallel parallel in MUVR2
#'@return correct_input: the original input(call) and the real input used in MUVR2 when you enter your input
#'@export
#'@examples
#'checkinput(X = XRVIP2,
#'           Y = YR2,  ## YR2 a numeric variable
#'           DA = FALSE,
#'           fitness="RMSEP")
checkinput <- function(X,
                       Y,
                       ML,
                       DA,
                       method,
                       fitness,
                       nInner,
                       nOuter,
                       varRatio,
                       scale,
                       modReturn,
                       logg,
                       parallel) {
  ###Step 0: save the model in put
  call <- match.call()

  ####Step 1: check if X exists and what is the structure of X
  #check for  existence
  if (missing(X)) {
    stop("X data is missing.")
  }

  #analyze X datatype
  if (class(X)[1] == "data.frame") {
    X <- MUVR2::onehotencoding(X)
    cat("X is transformed to a matrix by onehotencoding.", "\n")
  }
  factor_number <- 0
  numeric_integer_number <- 0
  character_number <- 0
  logical_number <- 0
  for (i in 1:ncol(X))
  {
    if (is.logical(X[, i]) == TRUE) {
      logical_number <- logical_number + 1
    }
    if (is.numeric(X[, i]) == TRUE |
        is.integer(X[, i]) == TRUE) {
      numeric_integer_number <- numeric_integer_number + 1
    }
    if (is.character(X[, i]) == TRUE) {
      character_number <- character_number + 1
    }
    if (is.factor(X[, i]) == TRUE) {
      factor_number <- factor_number + 1
    }
  }
  cat(
    "\n",
    "In X",
    "\n",
    "There are",
    "\n",
    numeric_integer_number,
    "numeric variables,",
    "\n",
    factor_number,
    "factor variables",
    "\n",
    character_number,
    "character varaibles",
    "\n",
    logical_number,
    "logical variables.",
    "\n"
  )




  ######Step 2.check if the datatype of input parameters are correct when they are not missing. For example, when DA=1, it should give an error
  #In short: The scenario when parameters are not missing but the input is wrong ################################

  #, ML, DA, method, fitness, nInner, nOuter,varRatio
  ##Parameters Group 1: These parameters are correlated with each other and with Y.
  # when missing() and when the input of the is correct are discusse later

  if (!missing(ML)) {
    if (is.logical(ML) == FALSE) {
      stop("\n", "ML must be TRUE or FALSE.")
    }
  }
  if (!missing(DA)) {
    if (is.logical(DA) == FALSE) {
      stop("\n", "DA must be TRUE or FALSE.")
    }
  }
  if (!missing(method)) {
    {
      if (!method %in% c("PLS", "RF")) {
        stop("\n", "method type is not correct.")
      }
    }
  }
  if (!missing(fitness)) {
    if (!fitness %in% c("MISS", "RMSEP", "AUROC", "BER")) {
      stop("\n", "fitness type is not correct.")
    }
  }

  ##Parameters Group 2: nInner and nOuter can be correlated with each other and liminted by lengh(Y)
  if (!missing(nInner)) {
    if (is.numeric(nInner) == FALSE &
        is.integer(nInner) == FALSE) {
      stop("\n", "nInner is a number.")
    }
    if (nInner < 2) {
      stop("\n", "nInner is too small.")
    }
  }
  if (!missing(nOuter)) {
    if (is.numeric(nOuter) == FALSE &
        is.integer(nOuter) == FALSE) {
      stop("\n", "nOuter is a number.")
    }
    if (nOuter < 3) {
      stop("\n", "nOuter is too small.")
    }
  }


  # Parameters Group 3: These parameters are not correlated with each other
  if (!missing(scale)) {
    if (is.logical(scale) == FALSE) {
      stop("\n", "scale must be TRUE or FALSE.")
    }
  }
  if (!missing(modReturn)) {
    if (is.logical(modReturn) == FALSE) {
      stop("\n", "modReturn must be TRUE or FALSE.")
    }
  }
  if (!missing(logg)) {
    if (is.logical(logg) == FALSE) {
      stop("\n", "logg must be TRUE or FALSE.")
    }
  }
  if (!missing(parallel)) {
    if (is.logical(parallel) == FALSE) {
      stop("\n", "parallel must be TRUE or FALSE.")
    }
  }
  if (!missing(varRatio)) {
    if (is.numeric(varRatio) == FALSE &
        is.integer(varRatio) == FALSE) {
      stop("\n", "varRatio is a number.")
    }
    if (varRatio <= 0 |
        varRatio > 1) {
      stop("\n", "varRation should be in (0,1].")
    }
    if (varRatio < 0.5 |
        varRatio > 0.95) {
      warning("varRatio is recommened to be within [0.5,0.95]")
    }
  }

  ########## Step 3: For parameters group 3 that are not correlated with each other, give them a value when they are missing
  ##when they are not missing, leave them be
  if (missing(scale)) {
    scale <- TRUE
  }
  if (missing(modReturn)) {
    modReturn <- FALSE
  }
  if (missing(logg)) {
    logg <- FALSE
  }
  if (missing(parallel)) {
    parallel <- TRUE
  }
  if (missing(varRatio)) {
    varRatio <- 0.75
  }

  ######Step 4: #Specify the scenario when ML is missing and ML=TRUE. The scenario of ML=FALSE will be discussed later
  if (missing(ML)) {
    ML <- FALSE
  }
  if (ML == TRUE) {
    if (missing(DA)) {
      DA <- FALSE
    }
    if (missing(fitness)) {
      fitness <- "MISS"
    }
    if (fitness != "MISS" |
        DA == TRUE) {
      warning("\n",
              "Multilevel model must have DA=FALSE and fitness=MISS.")
      fitness <- "MISS"
      DA <- FALSE
    }
    if (missing(method)) {
      method <- "RF"
    }
  }

  #####Step 5:Y is allowed missing when ML=TRUE, so here discuss when Y is missing, and when it is not missing, analyze its structure
  if (missing(Y)) {
    if (ML != TRUE) {
      stop("\n", "Y data is missing")
    }   ###when ML==TRUE, a Y will be built.
  } else{
    if (nrow(X) != length(Y)) {
      stop("The number of observations are not the same.")
    }

    ##analyze Y data type
    if (is.logical(Y) == TRUE) {
      stop("\n", "Y must be a numeric or factor variable.")
    } else if (is.character(Y) == TRUE) {
      Y <- factor(Y)
      warning("\n",
              "Original Y is a character variable and transformed to a factor variable now.")
      if (length(levels(Y)) > 10) {
        warning("\n", "There are more than 10 levels in Y.")
      }
      cat(
        "Y is transformed to a factor variable with",
        length(levels(Y)),
        "levels and",
        length(Y),
        "observations."
      )
    } else if (is.factor(Y) == TRUE) {
      if (length(levels(Y)) > 10) {
        warning("\n", "There are more than 10 levels in Y.")
      }
      cat(
        "\n",
        "Y is a factor variable with",
        length(levels(Y)),
        "levels and",
        length(Y),
        "observations."
      )
    } else{
      cat("\n",
          "Y is a numeric variable with",
          length(Y),
          "obsevations",
          "\n")
    }
  }

  ####Step 6 For parameters group 2: when nOuter and nInner is missing, they are given a value
  ##They are also limited by Y
  #nInner and nOuter
  if (missing(nOuter)) {
    nOuter <- 6
  }
  if (missing(nInner)) {
    nInner <- nOuter - 1
  }
  #Y
  if (length(Y) <= nInner)
    stop("\n", "Y has too few observations.")


  ##now there are som scenarios needs to be discussed for when ML=FALSE
  ##When DA,fitness and method is missing and when DA=TRUE/FALSE, fitness, and method



  #####step 7.about datatype and contracdictory of command when ML=FALSE
  ##now original X is a data frame and Y is a factor or numeric variable
  ##ML=FALSE


  if (ML == FALSE)
  {
    if (is.factor(Y) == TRUE) {
      if (missing(DA))
      {
        DA <- TRUE
      }
      if (DA == FALSE) {
        warning("\n", "When Y is a factor, must use DA.")
        DA <- TRUE
      }
      if (missing(fitness)) {
        fitness <- "MISS"
      }
      if (fitness == "RMSEP") {
        warning("\n", "When is a factor, fitness must be MISS, AUROC or BER.")
      }

      if (missing(method)) {
        method <- "RF"
      }
    }

    if (is.numeric(Y) == TRUE | is.integer(Y) == TRUE)
    {
      if (missing(DA))
      {
        DA <- FALSE
      }
      if (DA == TRUE) {
        if (length(levels(factor(Y))) <= 10) {
          Y <- factor(Y)
          warning(
            "\n",
            "Y is a numeric variable. It will be transformed to a factor variable when DA==TRUE."
          )
        } else{
          Y <- factor(Y)
          stop(
            "\n",
            "Y is a numeric variable. It will be transformed to a factor variable when DA==TRUE.",
            "\n",
            "There are >10 levels when transformed to factor."
          )
        }
        if (missing(fitness)) {
          fitness <- "MISS"
        }
        if (fitness == "RMSEP") {
          warning("\n",
                  "When is a factor, fitness must be MISS, AUROC or BER.")
        }

        if (missing(method)) {
          method <- "RF"
        }
      }

      if (missing(fitness)) {
        fitness <- "RMSEP"
      }
      if (fitness != "RMSEP") {
        warning("\n", "When is a factor, fitness must be RMSEP.")
      }

      if (missing(method)) {
        method <- "RF"
      }

    }


  }
  correct_input <- list(
    call,
    ML,
    DA,
    method,
    fitness,
    nInner,
    nOuter,
    varRatio,
    scale,
    modReturn,
    logg,
    parallel
  )
  names(correct_input) <-
    c(
      "call",
      "ML",
      "DA",
      "method",
      "fitness",
      "nInner",
      "nOuter",
      "varRatio",
      "scale",
      "modReturn",
      "logg",
      "parallel"
    )


  cat("\n",
      "\n",
      "Original input(call) and the input that will used in MUVR2:\n")
  return(correct_input)

}
