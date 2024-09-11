#' MUVR2 with EN
#'
#' "Multivariate modelling with Unbiased Variable selection" using Elastic Net (EN). Repeated double cross validation with tuning of variables using Elastic Net.
#' @param X Predictor variables. NB: Variables (columns) must have names/unique identifiers. NAs not allowed in data. For multilevel, only the positive half of the difference matrix is specified.
#' @param Y Response vector (Dependent variable). For classification, a factor (or character) variable should be used. For multilevel, Y is calculated automatically.
#' @param ID Subject identifier (for sampling by subject; Assumption of independence if not specified)
#' @param NZV Boolean for whether to filter out near zero variance variables (defaults to TRUE)
#' @param nRep Number of repetitions of double CV. (Defaults to 5)
#' @param nOuter Number of outer CV loop segments. (Defaults to 6)
#' @param nInner Number of inner CV loop segments. (Defaults to nOuter-1)
#' @param DA Boolean for Classification (discriminant analysis) (By default, if Y is numeric -> DA=FALSE. If Y is factor (or character) -> DA=TRUE)
#' @param fitness Fitness function for model tuning (choose either 'AUROC' or 'MISS' (default) for classification; or 'RMSEP' (default) for regression.)
#' @param methParam List with parameter settings for specified MV method (see function code for details)
#' @param ML Boolean for multilevel analysis (defaults to FALSE)
#' @param modReturn Boolean for returning outer segment models (defaults to FALSE). Setting modReturn=TRUE is required for making MUVR predictions using predMV().
#' @param parallel Boolean for whether to perform `foreach` parallel processing (Requires a registered parallel backend; Defaults to `TRUE`)
#' @param alow alpha tuning: lowest value of alpha
#' @param ahigh alpha tuning: highest value of alpha
#' @param astep alpha tuning: number of alphas to try from low to high
#' @param alog alpha tuning: Whether to space tuning of alpha in logarithmic scale (TRUE; default) or normal/arithmetic scale (FALSE)
#' @param keep A group of confounders that you want to manually set as non-zero
#' @param weigh_added weigh_added
#' @param weighing_matrix weighing_matrix
## @param percent_quantile range from 0 to 0.5. When select_variables_by quantile, this value represent the first quantile.
## @param percent_smoothcurve If select_variables_by smoothcurve, then it is robust
## @param Var_option quantile or smoothcurve
#' @param ... Pass additional arguments
#' @import splines glmnet pROC magrittr foreach doParallel graphics parallel psych
#' @importFrom randomForest randomForest
#' @importFrom ranger ranger
#' @importFrom grDevices colorRampPalette dev.off png
#' @importFrom mgcv predict.gam
#' @importFrom magrittr %>%
#' @importFrom dplyr filter mutate select
#' @importFrom stats as.dist coef coefficients cor density dt ecdf hclust heatmap lm loess median predict pt quantile resid sd
#' @return A MUVR object
#' @export
#' @examples
#' \donttest{
#' data("freelive2")
#' nRep <- 2 # Number of MUVR2 repetitions
#' nOuter <- 4 # Number of outer cross-validation segments
#' regrModel <- MUVR2_EN(X = XRVIP2,
#'                       Y = YR2,
#'                       nRep = nRep,
#'                       nOuter = nOuter,
#'                       modReturn = TRUE)
#' }
MUVR2_EN <- function(X,
                    ## X should be a dataframe
                    Y,
                    ID,
                    alow = 1e-5,
                    ## lowest value of alpha
                    ahigh = 1,
                    ##
                    astep = 11,
                    ##  number of alphas to try from low to high
                    alog = TRUE,
                    nRep = 5,
                    nOuter = 6,
                    nInner,
                    NZV = TRUE,
                    DA = FALSE,
                    fitness = c('AUROC', 'MISS', 'BER', 'RMSEP', 'wBER', 'wMISS'),
                    methParam,
                    ML = FALSE,
                    modReturn = FALSE,
                    parallel = TRUE,
                    keep = NULL,
                    weigh_added = FALSE,
                    weighing_matrix = NULL,
                    #   percent_quantile=0.25,
                    #    percent_smoothcurve=0.05,
                    #    Var_option=c("quantile","smoothcurve"),
                    ...) {



  #library(glmnet)

  X_original <- X


  # methParams
  if (missing(methParam)) {
    methParam <- rdcvNetParams()
  }

  ## If DA, multinomial, if not gaussian


  if (methParam$oneHot == TRUE) {
    if (any(class(X) %in% c("data.frame"))) {
      X <- onehotencoding(X)
      message("X is transformed to a matrix by onehotencoding.", "\n")
    }
  } else {
    X <- as.matrix(X)
  }
  # Remove nearZeroVariance variables

  modelReturn <- list(call = match.call())

  if (methParam$NZV == TRUE) {
    nzv <- MUVR2::nearZeroVar(X)
    if (length(nzv$Position) > 0) {
      modelReturn$nzv <- colnames(X)[nzv$Position]
      X <- X[, -nzv$Position]
      message(
        '\n',
        length(nzv$Position),
        'variables with near zero variance detected -> removed from X and stored under $nzv'
      )
    }
  }

  # Number of samples and variables
  nSamp <- nrow(X)
  nVar_smoothcurve <- nVar_quantile <- nVar0 <- ncol(X)

  # Sample identifiers
  if (missing(ID)) {
    message('\nMissing ID -> Assume all unique (i.e. sample independence)')
    ID <- 1:nSamp
  }
  if (ML) {
    X <- rbind(X, -X)
    if (missing(Y)) {
      Y <- rep(-1, nSamp)
    }
    Y <- c(Y, -Y)
    nSamp <- 2 * nSamp
    ID <- c(ID, ID)
    DA <- FALSE
    fitness <- 'MISS'
    message('\nMultilevel -> Regression on (-1,1) & fitness=MISS')
  }
  if(dim(X)[1]!=length(Y)){
    stop("The X and Y should have same number of observations")
  }
  if (is.null(weighing_matrix) & DA == TRUE) {
    weighing_matrix <- diag(1, length(levels(Y)), length(levels(Y)))
  }

  # Call in packages
  #library(pROC) # for roc and auroc
  #library(magrittr) # for pipe operator
  #library(foreach) # Parallel processing

  # Set up for parallel/serial
  if (parallel) {
    "%doVersion%" <- get("%dopar%")
  } else{
    "%doVersion%" <- get("%do%")
  }

  # Initialise modelReturn with function call


  # Start timer
  start.time <- proc.time()[3]

  # Rough check indata
  if (length(dim(X)) != 2) {
    stop('\nWrong format of X matrix.\n')
  }
  if (is.null(colnames(X))) {
    stop('\nNo column names in X matrix.\n')
  }







  # Sort out internal modelling parameters
  if (missing(nInner)) {
    nInner <- nOuter - 1
  }

  # DA / Classification
  if (is.character(Y)) {
    Y <- factor(Y)
  }
  if (is.factor(Y)) {
    message('\nY is factor -> Classification (',
        length(unique(Y)),
        ' classes)',
        sep = '')
    DA <- TRUE
  }
  if (is.numeric(Y) & DA) {
    Y <- as.factor(Y)
    message('\nDA=TRUE -> Y as factor -> Classification (',
        length(unique(Y)),
        ' classes)',
        sep = '')
  }
  if (DA) {
    methParam$family <- 'multinomial'
  }
  # Check fitness criterion
  # This may not be needed!
  if (missing(fitness)) {
    if (DA) {
      fitness <- 'BER'
      message('\nMissing fitness -> BER')
    } else {
      fitness <- 'RMSEP'
      message('\nMissing fitness -> RMSEP')
    }
  }

  # Set up for multilevel analysis

  # No Missingness allowed
  if (any(is.na(X)) | any(is.na(Y))) {
    stop('\nNo missing values allowed in X or Y data.\n')
  }
  if (!is.null(dim(Y))) {
    warning('\nY is not a vector: Return NULL')
    return(NULL)
  }

  # Sanity check
  if (nrow(X) != length(Y)) {
    warning('\nMust have same nSamp in X and Y: Return NULL')
    return(NULL)
  }


  ## Store indata in list for later model return
  InData <- list(
    X = X,
    Y = Y,
    ID = ID,
    alow = alow,
    ahigh = ahigh,
    astep = astep,
    nRep = nRep,
    nOuter = nOuter,
    nInner = nInner,
    DA = DA,
    fitness = fitness,
    methParam = methParam,
    ML = ML,
    parallel = parallel,
    method = "rdCVnet"
  )

  ## Sort sampling based on subjects and not index
  unik <- !duplicated(ID)  # boolean of unique IDs
  unikID <- ID[unik]
  if (DA) {
    if (nOuter > min(table(Y))) {
      warning(
        '\nnOuter is larger than your smallest group size. Consider lowering your nOuter to min(table(Y)).',
        call. = TRUE
      )
    }
    unikY <-
      Y[unik]  # Counterintuitive, but needed for groupings by Ynames
    Ynames <- sort(unique(Y))  # Find groups
    groups <- length(Ynames) # Number of groups
    groupID <- list()  # Allocate list for indices of groups
    for (g in 1:groups) {
      groupID[[g]] <- unikID[unikY == Ynames[g]]  # Find indices per group
    }
  }

  # Allocate prediction variables
  if (DA) {
    # Predictions per repetition
    yPredSeg <- matrix(
      nrow = length(Y),
      ncol = length(levels(Y)),
      dimnames = list(ID, levels(Y))
    )
  } else {
    # Predictions per repetition
    yPredSeg <- numeric(length(Y))
  }
  ##########################################0############################################################################################
  #########################################################################################3
  # tuning values for alpha
  if (alog) {
    ## Whether to space tuning of alpha in logarithmic scale (TRUE; default) or normal/arithmetic scale (FALSE)
    avect <- seq(from = log10(alow),
                 to = log10(ahigh),
                 length.out = astep) # logarithmic
    avect <- 10 ^ avect
  } else {
    avect <- seq(from = alow,
                 to = ahigh,
                 length.out = astep)
  }# Arithmetic



  ## Choose package/core algorithm according to chosen method
  packs <- c('pROC', 'glmnet', 'magrittr')
  exports <- c('vectSamp', 'uniqDASamp', 'foldVector')

  ############################
  ## Start repetitions
  # reps <- list() # 2+2 lines for pseudomanual troubleshooting
  # for (r in 1:nRep){


  #z<-0
  # if(percent_quantile<=0|percent_quantile>=0.5){
  #    stop("\npercent_quantile must be between 0 and 0.5")
  #  }
  #  if(percent_smoothcurve<=0|percent_smoothcurve>=0.5){
  #    stop("\npercent_smoothcurve must be between 0 and 0.5")
  #  }
  reps <- foreach(r = 1:nRep,
                  .packages = packs,
                  .export = exports) %doVersion% {
                    yPredSeg_list <- list()
                    # r <- 1
                    # r <- r + 1
                    if (modReturn) {
                      outMod <- list()
                    }
                    message('\n', '   Repetition ', r, ' of ', nRep, ':', sep =
                          '')
                    # Sampling into holdout segments
                    if (DA & identical(unikID, ID)) {
                      allTest <- uniqDASamp(Y, ID, nOuter)

                      ##a problem is that in some groups there are only 2 levels
                    } else {
                      allTest <- vectSamp(unikID, n = nOuter)
                    }
                    # Allocate result variables per repetition
                    if (DA) {
                      coefSeg <- array(
                        0,
                        dim = c(nVar0,   ## number of variables
                                length(levels(Y)),  ## number of groups
                                nOuter),
                        dimnames = list(colnames(X),
                                        levels(Y),
                                        paste0('Segment', 1:nOuter))
                      )
                    } else {
                      coefSeg <- matrix(0,
                                        nrow = nVar0,
                                        ncol = nOuter)
                      rownames(coefSeg) <- colnames(X)
                      colnames(coefSeg) <-
                        paste0('Segment', 1:nOuter)
                    }
                    alphaSeg <-
                      lambdaSeg <- nonZeroSeg <- numeric(nOuter)

                    if (DA) {
                      nonZeroSegClass <- matrix(nrow = nOuter,
                                                ncol = length(levels(Y)))
                    }
                    VALSeg <- matrix(nrow = nOuter,
                                     ncol = astep)   ## number of alphas from low or high
                    colnames(VALSeg) <- avect
                    rownames(VALSeg) <- paste0('Segment', 1:nOuter)
                    varSeg <- list()

                    if (DA) {
                      varSegClass <- list()
                    }

                    a <- 1


                    ## Perform outer loop segments -> one "majority vote" MV model per segment
                    for (i in 1:nOuter) {
                      # i <- 1
                      # i <- i+1

                      i <- a


                      message('\n Segment ', i, ' (Inner repeat): ', sep = '') # Counter
                      ## Draw out test set
                      testID <-
                        allTest[[i]] # Draw out segment = holdout set BASED ON UNIQUE ID
                      testIndex <-
                        ID %in% testID # Boolean for samples included
                      xTest <- X[testIndex, ]
                      yTest <-
                        Y[testIndex]  ## some of the ytest here may only have 1 level
                      inID <-
                        unikID[!unikID %in% testID]  # IDs not in test set
                      inIndex <- ID %in% inID
                      xIn <- X[inIndex, ]
                      yIn <- Y[inIndex]
                      idIn <- ID[inIndex]
                      # Allocate variables for inner repetitions
                      VALInner <- matrix(nrow = methParam$nRepInner,
                                         ncol = astep)
                      # Repeat inner CV several times for improved stability



                      if (is.factor(yIn)) {
                        if (length(levels(yIn)) != length(levels(droplevels(yIn))) |
                            any(table(yIn) == 1) | any(table(yIn) == 2)) {
                          a <- i
                        } else{
                          a <- i + 1
                        }
                      } else{
                        a <- i + 1
                      }




                      ##########################################################################
                      ########safeguard code
                      if (is.factor(yIn)) {
                        if (length(levels(yIn)) != length(levels(droplevels(yIn))) |
                            any(table(yIn) == 1) | any(table(yIn) == 2)) {
                          next

                        }

                      }


                      for (j in 1:methParam$nRepInner) {
                        ##Is the same as nInners
                        message(j, '... ')



                        if (DA & identical(unikID, ID)) {
                          inY <-
                            unikY[!unikID %in% testID]  # Counterintuitive, but needed for grouping by Ynames

                          allVal <-
                            uniqDASamp(inY, inID, nInner)    ####### Where randomness is introduced
                        } else {
                          allVal <- vectSamp(inID, n = nInner)
                          ###
                        }
                        foldID <- foldVector(foldList = allVal,
                                             ID = idIn)  ## identify each ID is in which group


                        ## Perform steps with different a
                        for (count in 1:astep) {
                          # Build models with successively fewer variables. Quality metric = number of missclassifications for Validation set
                          # count <- 1 # for testing
                          # count <- count + 1
                          alpha <-
                            avect[count]  ## avect is the alpha vector
                          penaltyfactor <- rep(1, ncol(xIn))
                          if (!is.null(keep)) {
                            if (any(!keep %in% c(colnames(X_original),
                                                 colnames(X)))) {
                              stop("Could not find that variable(s) in X")

                            }
                          #################################### This is aborted
                          #### The kept but of zero variance variables will be automatically removed
                          # if (any(!keep %in% colnames(X))) {
                          #    stop("Some variables you want to keep are of near zero varance")
                          #  }
                            keep_frame<-X_original[,keep,drop=FALSE]
                            suppressMessages(

                            keep_frame_onehot<-onehotencoding(keep_frame)
                            )
                            filter <- which(colnames(xIn) %in% c(keep,
                                                                 colnames(keep_frame_onehot)))

                            penaltyfactor[filter] <- 0

                          }
                          suppressWarnings(
                            inMod <- cv.glmnet(
                              x = xIn,
                              y = yIn,
                              alpha = alpha,
                              penalty.factor =
                                penaltyfactor,

                              # exclude=filter, positions, this is which the beta coefficient is forced to Zero
                              family = methParam$family,
                              foldid = foldID
                            )
                          )
                          whichMin <-
                            which.min(inMod$cvm)  ## 	The mean cross-validated error - a vector of length length(lambda).
                          VALInner[j, count] <- inMod$cvm[whichMin]
                        }  ##each astep loop
                        # matplot(avect, t(VALInner), log = 'x', ylab='MSE', main=paste0('Rep ',r,' - Segment ',i), type='b')
                        VALSeg[i, ] <- colMeans(VALInner)
                      } ## where each nRepInner end

                      alphaSeg[i] <- avect[which.min(VALSeg[i, ])]




                      penaltyfactor <- rep(1, ncol(xIn))
                      if (!is.null(keep)) {
                        keep_frame<-X_original[,keep,drop=FALSE]
                        suppressMessages(

                          keep_frame_onehot<-onehotencoding(keep_frame)
                        )
                        filter <- which(colnames(xIn) %in% c(keep,
                                                             colnames(keep_frame_onehot)))
                        penaltyfactor[filter] <- 0
                      }


                      suppressWarnings(
                        inMod <- cv.glmnet(
                          x = xIn,
                          y = yIn,
                          alpha = alphaSeg[i],
                          penalty.factor =
                            penaltyfactor,
                          family = methParam$family,
                          foldid = foldID
                        )
                      )

                      if (modReturn) {
                        outMod[[i]] <- inMod
                        outMod[[i]]$X <- xIn
                        outMod[[i]]$Y <- yIn
                      }

                      whichMin <- which.min(inMod$cvm)
                      lambdaSeg[i] <- inMod$lambda.min

                      if (DA) {
                        coefs <- coef(inMod,
                                      s = lambdaSeg[i]) %>% sapply(., as.matrix)

                        coefs <-
                          coefs[-1, ]   #### remove the intercept
                        ###############################################################
                        ################if one column missing add a new column
                        coefs_correct <- matrix(0,
                                                nrow(coefs),
                                                length(levels(Y)))
                        colnames(coefs_correct) <- levels(Y)
                        for (m in 1:ncol(coefs_correct)) {
                          if (colnames(coefs_correct)[m] %in% colnames(coefs)) {
                            for (n in 1:nrow(coefs_correct))
                            {
                              coefs_correct[n, m] <- coefs[n, colnames(coefs_correct)[m]]
                            }
                          }
                        }

                        coefs <- coefs_correct
                        #########################################################################
                        rownames(coefs) <- colnames(X)

                        coefSeg[, , i] <- coefs
                        nonZeroSeg[i] <- apply(coefs,
                                               1,
                                               function(x) {
                                                 any(x != 0)
                                               }) %>% sum
                        nonZeroSegClass[i, ] <- apply(coefs,
                                                      2,
                                                      function(x)
                                                        sum(x != 0))
                        varSegClass[[i]] <-
                          lapply(as.data.frame(coefs),
                                 function(x, nm = rownames(coefs)) {
                                   whichNZ <- which(x != 0)
                                   return(nm[whichNZ])
                                 })

                        ######
                        varSeg[[i]] <-
                          unlist(varSegClass[[i]]) %>% unique
                        varSeg[[i]] <-
                          varSeg[[i]][order(match(varSeg[[i]],
                                                  colnames(X)))] # Resort according to order in X
                        yPredSeg[testIndex, ] <- predict(inMod,
                                                         newx = xTest,
                                                         s = lambdaSeg[i],
                                                         type = 'response')
                        ######################################################################################################
                        ### In classification,the name of each observation is recorded so I only do the onesthat is not NA

                        yPredSeg_matrix <- matrix(NA,
                                                  nrow = nrow(yPredSeg),
                                                  ncol = ncol(yPredSeg))
                        for (zz in 1:nrow(yPredSeg)) {
                          if (testIndex[zz]) {
                            for (zzz in 1:ncol(yPredSeg)) {
                              yPredSeg_matrix[zz, zzz] <- yPredSeg[zz, zzz]
                            }
                          }
                        }
                        rownames(yPredSeg_matrix) <- rownames(X)
                        colnames(yPredSeg_matrix) <-
                          colnames(yPredSeg)
                        yPredSeg_list <-
                          c(yPredSeg_list, list(yPredSeg_matrix))

                        #################################################################################################

                      } else {
                        nonZeroSeg[i] <- inMod$nzero[whichMin]
                        coefSeg[, i] <- as.matrix(coef(inMod,
                                                       s = lambdaSeg[i]))[-1, , drop =
                                                                            FALSE]
                        ## remove the intercept

                        varSeg[[i]] <-
                          names(coefSeg[, i][coefSeg[, i] != 0])
                        yPredSeg[testIndex] <- predict(inMod,
                                                       newx = xTest,
                                                       s = lambdaSeg[i])
                        ######################################################################################################
                        ### in regression, there is no name for each observation, So I will record the NA value

                        yPredSeg_vector <- rep(NA, length(yPredSeg))
                        for (zz in 1:length(yPredSeg)) {
                          if (testIndex[zz]) {
                            yPredSeg_vector[zz] <- yPredSeg[zz]
                          }
                        }
                        yPredSeg_list <-
                          c(yPredSeg_list, list(yPredSeg_vector))
                        #################################################################################################


                      }
                      # plot(yTest, yPred)

                    }  ## where the outer loop ends

                    # matplot(avect, t(VALSeg), log = 'x', ylab='MSE', main=paste0('Rep ',r,' - All segments'), type='b')
                    # Per repetition: Average outer loop predictions, VIP ranks and nComp for PLS
                    varSegClassReorder <- list()
                    if (DA) {
                      for (i in 1:nOuter) {
                        for (j in 1:length(levels(Y))) {
                          varSegClassReorder[[levels(Y)[j]]][[i]] <-
                            varSegClass[[i]][levels(Y)[j]]
                        }
                      }
                    }




                    parReturn <- list(
                      yPred = yPredSeg,
                      alpha = alphaSeg,
                      lambda = lambdaSeg,
                      nonZero = nonZeroSeg,
                      coef = coefSeg,
                      VAL = VALSeg,
                      vars = varSeg,
                      yPredSeg_list = yPredSeg_list
                    )
                    if (DA) {
                      parReturn$varsClass <- varSegClassReorder
                      parReturn$nonZeroClass <- nonZeroSegClass
                    }
                    # Return underlying models
                    if (modReturn) {
                      parReturn$outModel <- outMod
                    }
                    return(parReturn)
                    # reps[[r]] <- parReturn
                  }   ## Repetition loop end

  ####################################
  # Allocate prediction variables
  if (DA) {
    # Predictions for all repetitions
    yPredRep <- array(
      dim = c(length(Y),
              length(levels(Y)),
              nRep),
      dimnames = list(ID,
                      levels(Y),
                      paste('Rep', 1:nRep, sep = ''))
    )
  } else {
    # Predictions for all repetitions
    yPredRep <- matrix(
      nrow = length(Y),
      ncol = nRep,
      dimnames = list(ID, paste('Rep', 1:nRep, sep = ''))
    )
  }

  # Allocate validation and parameter variables
  alphaRep <- lambdaRep <- nonZeroRep <- fitnessRep <-
    matrix(nrow = nRep,
           ncol = nOuter,
           dimnames = list(
             paste('repetition', 1:nRep, sep = ''),
             paste('segment', 1:nOuter, sep = '')
           ))
  if (DA) {
    coefRep <- list()
  } else {
    coefRep <- array(
      0,
      dim = c(nVar0, nOuter, nRep),
      dimnames = list(
        colnames(X),
        paste0('Segment', 1:nOuter),
        paste0('Rep', 1:nRep)
      )
    )
  }



  VALRep <- array(dim = c(nOuter, astep, nRep),
                  dimnames = list(
                    paste('outSeg',
                          1:nOuter, paste = ''),
                    avect,
                    paste(rep('rep', nRep),
                          1:nRep, sep = '')
                  ))
  varRep <- list()
  if (DA) {
    varsClassRep <- list()
    nonZeroClassRep <- array(
      dim = c(nOuter,
              length(levels(Y)),
              nRep),
      dimnames = list(
        paste('outSeg', 1:nOuter, paste = ''),
        levels(Y),
        paste(rep('rep', nRep),
              1:nRep, sep = '')
      )
    )
  }

  # Allocate variable for models (for later predictions)
  if (modReturn) {
    outMods <- list()
  }

  ####################################
  # Aggregate results over predictions
  yPredSeg_listlist <- list()

  for (r in 1:nRep) {
    if (DA) {
      yPredRep[, , r] <- reps[[r]]$yPred
    } else {
      yPredRep[, r] <- reps[[r]]$yPred
    }

    yPredSeg_listlist <- c(yPredSeg_listlist, reps[[r]]$yPredSeg_list)

    alphaRep[r, ] <- reps[[r]]$alpha
    lambdaRep[r, ] <- reps[[r]]$lambda
    nonZeroRep[r, ] <- reps[[r]]$nonZero

    if (DA) {
      for (i in 1:length(levels(Y))) {
        varsClassRep[[levels(Y)[i]]][[r]] <-
          reps[[r]]$varsClass[[levels(Y)[i]]]
      }
      nonZeroClassRep[, , r] <- reps[[r]]$nonZeroClass
    }

    if (DA) {
      coefRep[[r]] <- reps[[r]]$coef
    } else {
      coefRep[, , r] <- reps[[r]]$coef
    }

    VALRep[, , r] <- reps[[r]]$VAL
    varRep <- c(varRep, reps[[r]]$vars)

    if (modReturn) {
      outMods <- c(outMods, reps[[r]]$outModel)
    }
  }
  #############################################################
  if (DA) {
    miss_vector <- vector()
    wmiss_vector <- vector()
    BER_vector <- vector()
    wBER_vector <- vector()

  } else{
    RMSEP_vector <- vector()
  }
  for (z in 1:length(yPredSeg_listlist)) {
    if (DA) {
      noNA <- rep(TRUE, nrow(yPredSeg_listlist[[z]]))
      for (zz in 1:nrow(yPredSeg_listlist[[z]])) {
        if (is.na(yPredSeg_listlist[[z]][zz, 1])) {
          noNA[zz] <- FALSE
        }

      }

      actual_noNA_temp <- vector()
      predicted_noNA_temp <- vector()
      for (zz in 1:length(noNA)) {
        if (noNA[zz] == TRUE) {
          actual_noNA_temp <- c(actual_noNA_temp,
                                as.character(Y)[zz])

          predicted_noNA_temp <- c(predicted_noNA_temp,
                                   colnames(yPredSeg_listlist[[z]])[which.max(yPredSeg_listlist[[z]][zz, ])])

        }

      }
      actual_noNA_temp <- factor(actual_noNA_temp, levels = levels(Y))
      BER_vector <- c(BER_vector,
                      getBER(actual_noNA_temp,
                             predicted_noNA_temp))
      miss_vector <- c(miss_vector,
                       sum(actual_noNA_temp == predicted_noNA_temp))
      if (fitness == 'wBER' | fitness == 'wMISS') {
        wBER_vector <- c(
          wBER_vector,
          getBER(
            actual_noNA_temp,
            predicted_noNA_temp,
            weigh_added = weigh_added,
            weighing_matrix = weighing_matrix
          )
        )
        wmiss_vector <- c(
          wmiss_vector,
          getBER(
            actual_noNA_temp,
            predicted_noNA_temp,
            weigh_added = weigh_added,
            weighing_matrix = weighing_matrix
          )
        )
      }

    } else {
      actual_temp <- Y
      predicted_temp <- yPredSeg_listlist[[z]]
      RMSEP_vector <- c(RMSEP_vector,
                        get_rmsep(actual_temp, predicted_temp))

    }

  }

  if (DA) {
    if (fitness == "MISS") {
      fitnessRep <- matrix(miss_vector,
                           nrow = nRep,
                           ncol = nOuter,
                           byrow = TRUE)
    } else if (fitness == "wMISS") {
      ### in the scenario of BER and AUROC
      fitnessRep <- matrix(wmiss_vector,
                           nrow = nRep,
                           ncol = nOuter,
                           byrow = TRUE)
    } else if (fitness == "BER") {
      ### in the scenario of BER and AUROC
      fitnessRep <- matrix(BER_vector,
                           nrow = nRep,
                           ncol = nOuter,
                           byrow = TRUE)
    } else if (fitness == "wBER") {
      ### in the scenario of BER and AUROC
      fitnessRep <- matrix(wBER_vector,
                           nrow = nRep,
                           ncol = nOuter,
                           byrow = TRUE)
    }
    colnames(fitnessRep) <- colnames(nonZeroRep)
    rownames(fitnessRep) <- rownames(nonZeroRep)
  } else{
    fitnessRep <- matrix(RMSEP_vector,
                         nrow = nRep,
                         ncol = nOuter,
                         byrow = TRUE)
    colnames(fitnessRep) <- colnames(nonZeroRep)
    rownames(fitnessRep) <- rownames(nonZeroRep)
  }
  #############################################################
  # Average predictions
  if (DA) {
    yPred <- apply(yPredRep,
                   c(1, 2),  ## what dimension is not changed by the function
                   mean)
  } else {
    yPred <- apply(yPredRep,
                   1,
                   mean)[1:nrow(X)]
  }
  # matplot(Y,yPredRep, pch=16, col='lightgrey', cex=0.7)
  # points(Y, yPred, pch=16, col='grey20',cex=.9)
  modelReturn$yPred <- yPred

  if (DA) {
    # Classify predictions
    yClass <- apply(yPred,
                    1,
                    function(x) {
                      levels(Y)[which.max(x)]
                    })

    miss <- sum(yClass != Y)
    auc <- numeric(length(levels(Y)))
    # if(fitness=='BER'|fitness=='wBER'){
    ber <- getBER(actual = Y,
                  predicted = yClass)
    modelReturn$ber <- ber
    #  }
    if (fitness == 'wBER' | fitness == 'wMISS') {
      wber <- getBER(
        actual = Y,
        predicted = yClass,
        weigh_added = weigh_added,
        weighing_matrix = weighing_matrix
      )
      modelReturn$wber <- wber
      wmiss <- getMISS(
        actual = Y,
        predicted = yClass,
        weigh_added = weigh_added,
        weighing_matrix = weighing_matrix
      )
      modelReturn$wmiss <- wmiss
    }
    names(auc) <- levels(Y)
    for (cl in 1:length(levels(Y))) {
      auc[cl] <- roc(Y == (levels(Y)[cl]),
                     yPred[, cl],
                     quiet = TRUE)$auc
    }
    # # Report
    #names(yClass)<-rownames(X)
    modelReturn$yClass <- yClass
    modelReturn$miss <- miss

    modelReturn$auc <- auc
  } else if (ML) {
    yClass <- ifelse(yPred > 0, 1, -1)
    modelReturn$yClass <- yClass
    modelReturn$miss <- sum(yClass != Y)
    modelReturn$auc <- roc(Y, yPred, quiet = TRUE)$auc
    # if(fitness=='BER'|fitness=='wBER'){
    ber <- getBER(actual = Y,
                  predicted = yClass)
    modelReturn$ber <- ber
    #}
    if (fitness == 'wBER' | fitness == 'wMISS') {
      modelReturn$wber <- getBER(
        actual = Y,
        predicted = yClass,
        weigh_added = weigh_added,
        weighing_matrix = weighing_matrix
      )
      modelReturn$wmiss <- getMISS(
        actual = Y,
        predicted = yClass,
        weigh_added = weigh_added,
        weighing_matrix = weighing_matrix
      )
    }
    names(modelReturn$yClass) = paste(1:nSamp,
                                      ID,
                                      sep = '_ID')
  }

  VAL <- apply(VALRep,
               2,
               mean)  ### averaged all Repetition and Outer loop to get the overall error
  modelReturn$varTable <-
    varRep %>% unlist %>% table %>% sort(., decreasing = TRUE)
  modelReturn$alpha <- avect[which.min(VAL)]
  modelReturn$alphaRep <- alphaRep
  modelReturn$lambdaRep <- lambdaRep
  modelReturn$coefRep <- coefRep
  modelReturn$nonZeroRep <- nonZeroRep
  modelReturn$fitnessRep <- fitnessRep
  modelReturn$varRep <- varRep
  modelReturn$VAL$metric <- fitness
  modelReturn$VAL$VALRep <- VALRep
  modelReturn$VAL$VAL <- VAL
  modelReturn$VAL$avect <- avect
  modelReturn$X <- X

  #################################################################################
  ####### adding nVar part for variable selection. Min mid max is built baseing on 2.5%, 50% and 97.5%
  ####### Calculate overall nVar
  #### caluate how many times each feature show up accross nRep and nOuter round, this is already done in varTable
  varTable <- modelReturn$varTable
  varTable_included <-
    colnames(modelReturn$X)[colnames(modelReturn$X) %in% names(varTable)]
  varTable_notincluded <-
    colnames(modelReturn$X)[!colnames(modelReturn$X) %in% names(varTable)]
  varTable_all <- c(varTable, rep(0, length(varTable_notincluded)))
  names(varTable_all) <- c(names(varTable), varTable_notincluded)
  varTable <- varTable_all
  modelReturn$varTable <- varTable_all

  cum_varTable <- numeric(length(table(varTable)))
  cum_varTable[1] <- table(varTable)[length(table(varTable))]
  for (s in 2:length(table(varTable))) {
    cum_varTable[s] <- cum_varTable[s - 1] + rev(table(varTable))[s]
  }
  names(cum_varTable) <- names(rev(table(varTable)))

  ################################################################################
  ##########################################################################################
  ### non zero repetition

  #message("please use the getVIRank function to see which one to use ")

  ##
  ##################################################################################################
  ###################################################################################
  ### by smooth curve
  #library(splines)
  nonZeroRep_vector <- c(nonZeroRep)
  fitnessRep_vector <- c(fitnessRep)
  nonZeroRep_vector_grid <-
    seq(min(nonZeroRep_vector), max(nonZeroRep_vector), 1)

  fit_temp <-
    loess(fitnessRep_vector ~ nonZeroRep_vector,
          span = 1,
          degree = 2)
  predict_temp <- predict(fit_temp,
                          newdata = data.frame(nonZeroRep_vector = nonZeroRep_vector_grid))
  #fit_temp<-lm(fitnessRep_vector ~ bs(nonZeroRep_vector,
  #                          df=3,  ### when intercept is false degree of freedom = df-degree  df must >=3,  df = length(knots) + degree
  #                          degree=3))
  #predict_temp<-predict(fit_temp,
  #        newdata = list(nonZeroRep_vector=seq(min(nonZeroRep_vector),
  #                                           max(nonZeroRep_vector),
  #                                           1)))

  percent_smoothcurve <- 0.05
  scaled_predict_temp <-
    (predict_temp - min(predict_temp)) / abs(diff(range(predict_temp)))
  maxIndex_smoothcurve <-
    max(which(scaled_predict_temp <= percent_smoothcurve))
  minIndex_smoothcurve <-
    min(which(scaled_predict_temp <= percent_smoothcurve))
  varMin_smoothcurve <- nonZeroRep_vector_grid[minIndex_smoothcurve]
  varMax_smoothcurve <- nonZeroRep_vector_grid[maxIndex_smoothcurve]
  varMid_smoothcurve <-
    round(exp(mean(log(
      c(nonZeroRep_vector_grid[minIndex_smoothcurve], nonZeroRep_vector_grid[maxIndex_smoothcurve])
    )))) # Geometric mean of min and max. This one has decimals



  ##min limit: take less
  for (s in 1:length(cum_varTable)) {
    ## set safeguard argument in case there are 0 values
    if (s != 1) {
      if (varMin_smoothcurve < cum_varTable[s]) {
        varMin_num_smoothcurve <- as.numeric(names(cum_varTable)[1:s - 1])
        varMin_num_smoothcurve <-
          varMin_num_smoothcurve[!is.na(varMin_num_smoothcurve)]
        break
      } else if (varMin_smoothcurve == cum_varTable[s] &
                 s == length(cum_varTable)) {
        varMin_num_smoothcurve <- as.numeric(names(cum_varTable)[1:s])
        varMin_num_smoothcurve <-
          varMin_num_smoothcurve[!is.na(varMin_num_smoothcurve)]
        break
      }
    } else{
      if (varMin_smoothcurve < cum_varTable[s]) {
        varMin_num_smoothcurve <- as.numeric(names(cum_varTable)[1])
        varMin_num_smoothcurve <-
          varMin_num_smoothcurve[!is.na(varMin_num_smoothcurve)]
        break
      }
    }
  }

  ##mid limit: take less
  for (s in 1:length(cum_varTable)) {
    ## set safeguard argument in case there are 0 values
    if (s != 1) {
      if (varMid_smoothcurve < cum_varTable[s]) {
        varMid_num_smoothcurve <- as.numeric(names(cum_varTable)[1:s - 1])
        varMid_num_smoothcurve <-
          varMid_num_smoothcurve[!is.na(varMid_num_smoothcurve)]
        break
      } else if (varMid_smoothcurve == cum_varTable[s] &
                 s == length(cum_varTable)) {
        varMid_num_smoothcurve <- as.numeric(names(cum_varTable)[1:s])
        varMid_num_smoothcurve <-
          varMid_num_smoothcurve[!is.na(varMid_num_smoothcurve)]
        break
      }
    } else{
      if (varMid_smoothcurve < cum_varTable[s]) {
        varMid_num_smoothcurve <- as.numeric(names(cum_varTable)[1])
        varMid_num_smoothcurve <-
          varMid_num_smoothcurve[!is.na(varMid_num_smoothcurve)]
        break
      }
    }
  }

  ##max limit: take less
  for (s in 1:length(cum_varTable)) {
    ## set safeguard argument in case there are 0 values
    if (s != 1) {
      if (varMax_smoothcurve < cum_varTable[s]) {
        varMax_num_smoothcurve <- as.numeric(names(cum_varTable)[1:s - 1])
        varMax_num_smoothcurve <-
          varMax_num_smoothcurve[!is.na(varMax_num_smoothcurve)]
        break
      } else if (varMax_smoothcurve == cum_varTable[s] &
                 s == length(cum_varTable)) {
        varMax_num_smoothcurve <- as.numeric(names(cum_varTable)[1:s])
        varMax_num_smoothcurve <-
          varMax_num_smoothcurve[!is.na(varMax_num_smoothcurve)]
        break
      }
    } else{
      if (varMax_smoothcurve < cum_varTable[s]) {
        varMax_num_smoothcurve <- as.numeric(names(cum_varTable)[1])
        varMax_num_smoothcurve <-
          varMax_num_smoothcurve[!is.na(varMax_num_smoothcurve)]
        break
      }
    }
  }

  minnames_smoothcurve <- vector()
  midnames_smoothcurve <- vector()
  maxnames_smoothcurve <- vector()

  for (s in 1:length(varTable)) {
    if (varTable[s] %in% varMin_num_smoothcurve) {
      minnames_smoothcurve <- c(minnames_smoothcurve, names(varTable)[s])
    }
    if (varTable[s] %in% varMid_num_smoothcurve) {
      midnames_smoothcurve <- c(midnames_smoothcurve, names(varTable)[s])
    }
    if (varTable[s] %in% varMax_num_smoothcurve) {
      maxnames_smoothcurve <- c(maxnames_smoothcurve, names(varTable)[s])
    }
  }

  nVar_smoothcurve <- c(varMin_smoothcurve,
                        varMid_smoothcurve,
                        varMax_smoothcurve)
  Var_smoothcurve <- list(min = minnames_smoothcurve,
                          mid = midnames_smoothcurve,
                          max = maxnames_smoothcurve)

  names(nVar_smoothcurve) <- c("min", "mid", "max")

  #  modelReturn$Var_smoothcurve<- Var_smoothcurve
  #  modelReturn$nVar_smoothcurve<- nVar_smoothcurve


  #####################################################################
  #### by quantile
  percent_quantile <- 0.125
  minmidmax_quantile <- quantile(nonZeroRep,
                                 c(percent_quantile, 0.5, 1 - percent_quantile))
  minlimit_quantile <-
    floor(minmidmax_quantile[1])  ### take the floor value in case no value is selected
  midlimit_quantile <- floor(minmidmax_quantile[2])  ###
  maxlimit_quantile <- floor(minmidmax_quantile[3])
  ##min limit: take less
  for (s in 1:length(cum_varTable)) {
    ## set safeguard argument in case there are 0 values
    if (s != 1) {
      if (minlimit_quantile < cum_varTable[s]) {
        minlimit_num_quantile <- as.numeric(names(cum_varTable)[1:s - 1])
        minlimit_num_quantile <-
          minlimit_num_quantile[!is.na(minlimit_num_quantile)]
        break
      } else if (minlimit_quantile == cum_varTable[s] &
                 s == length(cum_varTable)) {
        minlimit_num_quantile <- as.numeric(names(cum_varTable)[1:s])
        minlimit_num_quantile <-
          minlimit_num_quantile[!is.na(minlimit_num_quantile)]
        break
      }
    } else{
      if (minlimit_quantile < cum_varTable[s]) {
        minlimit_num_quantile <- as.numeric(names(cum_varTable)[1])
        minlimit_num_quantile <-
          minlimit_num_quantile[!is.na(minlimit_num_quantile)]
        break
      }
    }
  }

  ##min limit: take less
  for (s in 1:length(cum_varTable)) {
    ## set safeguard argument in case there are 0 values
    if (s != 1) {
      if (midlimit_quantile < cum_varTable[s]) {
        midlimit_num_quantile <- as.numeric(names(cum_varTable)[1:s - 1])
        midlimit_num_quantile <-
          midlimit_num_quantile[!is.na(midlimit_num_quantile)]
        break
      } else if (midlimit_quantile == cum_varTable[s] &
                 s == length(cum_varTable)) {
        midlimit_num_quantile <- as.numeric(names(cum_varTable)[1:s])
        midlimit_num_quantile <-
          midlimit_num_quantile[!is.na(midlimit_num_quantile)]
        break
      }
    } else{
      if (midlimit_quantile < cum_varTable[s]) {
        midlimit_num_quantile <- as.numeric(names(cum_varTable)[1])
        midlimit_num_quantile <-
          midlimit_num_quantile[!is.na(midlimit_num_quantile)]
        break
      }
    }
  }

  ##min limit: take less
  for (s in 1:length(cum_varTable)) {
    ## set safeguard argument in case there are 0 values
    if (s != 1) {
      if (maxlimit_quantile < cum_varTable[s]) {
        maxlimit_num_quantile <- as.numeric(names(cum_varTable)[1:s - 1])
        maxlimit_num_quantile <-
          maxlimit_num_quantile[!is.na(maxlimit_num_quantile)]
        break
      } else if (maxlimit_quantile == cum_varTable[s] &
                 s == length(cum_varTable)) {
        maxlimit_num_quantile <- as.numeric(names(cum_varTable)[1:s])
        maxlimit_num_quantile <-
          maxlimit_num_quantile[!is.na(maxlimit_num_quantile)]
        break
      }
    } else {
      if (maxlimit_quantile < cum_varTable[s]) {
        maxlimit_num_quantile <- as.numeric(names(cum_varTable)[1])
        maxlimit_num_quantile <-
          maxlimit_num_quantile[!is.na(maxlimit_num_quantile)]
        break
      }
    }

  }

  minnames_quantile <- vector()
  midnames_quantile <- vector()
  maxnames_quantile <- vector()

  for (s in 1:length(varTable)) {
    if (varTable[s] %in% minlimit_num_quantile) {
      minnames_quantile <- c(minnames_quantile, names(varTable)[s])
    }
    if (varTable[s] %in% midlimit_num_quantile) {
      midnames_quantile <- c(midnames_quantile, names(varTable)[s])
    }
    if (varTable[s] %in% maxlimit_num_quantile) {
      maxnames_quantile <- c(maxnames_quantile, names(varTable)[s])
    }
  }

  nVar_quantile <-
    c(minlimit_quantile, midlimit_quantile, maxlimit_quantile)
  Var_quantile <- list(min = minnames_quantile,
                       mid = midnames_quantile,
                       max = maxnames_quantile)

  names(nVar_quantile) <- c("min", "mid", "max")

  #  modelReturn$Var_quantile<- Var_quantile
  #  modelReturn$nVar_quantile<- nVar_quantile

  #  if(missing(Var_option)){
  #    Var_option="quantile"
  #  }
  # if(Var_option=="quantile"){
  #    modelReturn$Var<- Var_quantile
  #    modelReturn$nVar<- nVar_quantile
  #  }else{
  #    modelReturn$Var<- Var_smoothcurve
  #    modelReturn$nVar<- nVar_smoothcurve
  #  }
  #############################################################################
  ####################################################################################

  if (DA) {
    modelReturn$varClassRep <- sapply(varsClassRep,
                                      function(x) {
                                        unlist(x) %>% table %>% sort(., decreasing = TRUE)
                                      })
    modelReturn$nonZeroClassRep <- nonZeroClassRep
  }
  if (modReturn) {
    modelReturn$outModels <- outMods
  }

  modelReturn$yPredPerRep <- yPredRep
  modelReturn$inData <- InData

  modelReturn$cum_varTable <- cum_varTable
  #########################################################
  # Fit-predict model
  penaltyfactor <- rep(1, ncol(X))
  if (!is.null(keep)) {
    keep_frame<-X_original[,keep,drop=FALSE]
    suppressMessages(

      keep_frame_onehot<-onehotencoding(keep_frame)
    )
    filter <- which(colnames(X) %in% c(keep,
                                         colnames(keep_frame_onehot)))
    penaltyfactor[filter] <- 0
  }

  ####################################### drop levels if the class not appear
  ################# safe guard measure  first time
  if (is.factor(Y)) {
    if (length(levels(Y)) != length(levels(droplevels(Y)))) {
      Y <- droplevels(Y)
    }
    if (any(table(Y) == 1)) {
      remove_samp <- Y == names(table(Y))[which(table(Y) == 1)]
      X <- X[-which(remove_samp), ]
      Y <- Y[-which(remove_samp)]
    }
    if (any(table(Y) == 2)) {
      remove_samp <- yIn == names(table(Y))[which(table(Y) == 2)]
      X <- X[-which(remove_samp), ]
      Y <- Y[-which(remove_samp)]
    }
    if (length(levels(Y)) != length(levels(droplevels(Y)))) {
      Y <- droplevels(Y)
    }
  }

  ### Use the overall alpha to get the best lambda
  suppressWarnings(
    cvFit <- cv.glmnet(
      X,
      Y,
      penalty.factor = penaltyfactor,
      alpha = modelReturn$alpha,
      family = methParam$family
    )
  )
  ### do prediction models
  suppressWarnings(
    fitPredict <- glmnet(
      X,
      Y,
      penalty.factor = penaltyfactor,
      alpha = modelReturn$alpha,
      lambda = cvFit$lambda.min,
      family = methParam$family
    )
  )
  yFit <- predict(fitPredict,
                  newx = X,
                  type = 'response')
  # Calculate fit statistics

  if (!DA) {
    if (!missing(weighing_matrix)) {
      warning("This is not classification. Weighing matrix is ignored")
    }
    TSS <- sum((Y - mean(Y)) ^ 2)
    RSS <- sum((Y - yFit) ^ 2)
    PRESS <- sum((Y - yPred) ^ 2)
    R2 <- 1 - (RSS / TSS)
    Q2 <- 1 - (PRESS / TSS)
    modelReturn$fitMetric <- data.frame(R2, Q2)

    modelReturn$fitMetric <- list(R2 = R2,
                                  Q2 = Q2)
  } else {
    ##This is where the weighting matrix that comes in
    ###########################################################################################################
    #  if(!missing(weighing_matrix))
    #  {warning("Weighing matrix is added (overwrite weigh_added command)")
    #    weigh_added=TRUE
    #    }

    if (weigh_added == FALSE) {
      modelReturn$fitMetric <- list(CR = 1 - (miss / length(Y)))
    } else{
      if (missing(weighing_matrix)) {
        warning("Missing weighing_matrix,weighing_matrix will be diagnoal")
        weighing_matrix <- diag(1, length(levels(Y)), length(levels(Y)))
      }
      if (dim(weighing_matrix)[1] != length(levels(Y))) {
        stop("The dimension of weighing_matrix is not correct")
      }
      if (dim(weighing_matrix)[2] != length(levels(Y))) {
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


      confusion_matrix <-
        table(actual = Y, predicted = t(factor(modelReturn$yClass, levels = levels(Y))))
      scoring_matrix <- confusion_matrix * weighing_matrix
      correct_score <- sum(scoring_matrix)
      modelReturn$fitMetric <-
        list(wCR =  (correct_score / length(Y)))
      modelReturn$fitMetric$CR <- 1 - (miss / length(Y))


    }
    #################################################################################################################

  }





  modelReturn$yPredSeg_list <- yPredSeg_listlist

  ### nVar per repetition is the median of number of variables that have non-zero coefficient in each repetition
  modelReturn$nVarPerRep <- apply(modelReturn$nonZeroRep, 1, median)

  # Stop timer
  end.time <- proc.time()[3]
  modelReturn$calcMins <- (end.time - start.time) / 60
  message('\n Elapsed time', (end.time - start.time) / 60, 'mins \n')
  class(modelReturn) <- c('MUVR',
                          ifelse(DA,
                                 'Classification',
                                 ifelse(ML, 'Multilevel', 'Regression')),
                          "rdCVnet")
  if (!is.null(keep)) {
    modelReturn$keep <- keep
  }
  return(modelReturn)
}
