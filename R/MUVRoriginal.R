#' MUVR: "Multivariate modeling with Unbiased Variable selection" using PLS and RF
#' Repeated double cross validation with tuning of variables in the inner loop.
#' @param X Predictor variables. NB: Variables (columns) must have names/unique identifiers. NAs not allowed in data. For multilevel, only the positive half of the difference matrix is specified.
#' @param Y Response vector (Dependent variable). For classification, a factor (or character) variable should be used. For multilevel, Y is calculated automatically.
#' @param ID Subject identifier (for sampling by subject; Assumption of independence if not specified)
#' @param scale If TRUE, the predictor variable matrix is scaled to unit variance for PLS modeling.
#' @param nRep Number of repetitions of double CV. (Defaults to 5)
#' @param nOuter Number of outer CV loop segments. (Defaults to 6)
#' @param nInner Number of inner CV loop segments. (Defaults to nOuter - 1)
#' @param varRatio Ratio of variables to include in subsequent inner loop iteration. (Defaults to 0.75)
#' @param DA Boolean for Classification (discriminant analysis) (By default, if Y is numeric -> DA = FALSE. If Y is factor (or character) -> DA = TRUE)
#' @param fitness Fitness function for model tuning (choose either 'AUROC' or 'MISS' (default) for classification; or 'RMSEP' (default) for regression.)
#' @param method Multivariate method. Supports 'PLS' and 'RF' (default)
#' @param methParam List with parameter settings for specified MV method (see function code for details)
#' @param ML Boolean for multilevel analysis (defaults to FALSE)
#' @param modReturn Boolean for returning outer segment models (defaults to FALSE). Setting modReturn = TRUE is required for making MUVR predictions using predMV().
#' @param logg Boolean for whether to sink model progressions to `log.txt`
#' @param parallel Boolean for whether to perform `foreach` parallel processing (Requires a registered parallel backend; Defaults to `TRUE`)
#' @param ... Additional argument
#' @return A old MUVR class object
#' @export


MUVRoriginal <- function(X,
                         Y,
                         ID,
                         scale = TRUE,
                         nRep = 5,
                         nOuter = 6,
                         nInner,
                         varRatio = 0.75,
                         DA = FALSE,
                         fitness = c('AUROC', 'MISS', 'BER', 'RMSEP'),
                         method = c('PLS', ' RF'),
                         methParam,
                         ML = FALSE,
                         modReturn = FALSE,
                         logg = FALSE,
                         parallel = TRUE,
                         # keep,
                         ...) {
  # Start timer
  start.time <- proc.time()[3]

  # Initialise modelReturn with function call
  modelReturn <- list(call = match.call())

  # Default core modelling method
  if (missing(method)) {
    method <- 'RF'
  }
  if (missing(methParam)) {
    methParam <- customParams(method = method)
  }
  # Call in relevant package(s)
  library(pROC)
  library(foreach)

  # Parallel processing
  if (parallel) {
    "%doVersion%" <- get("%dopar%")
  } else {
    "%doVersion%" <- get("%do%")
  }

  # Rough check indata
  if (length(dim(X)) != 2) {
    stop('\nWrong format of X matrix.\n')
  }
  if (is.null(colnames(X))) {
    stop('\nNo column names in X matrix.\n')
  }
  # X <- as.matrix(X) # PROBLEM: Will not work for factor variables BUT it will work for PLS with one-hot
  # I don't know how much of a problem data frames are from a time perspective. Check matrix vs DF on same dataset for time difference.

  # One-hot expansion of factor variables
  if (methParam$oneHot) {
    cat('For checking / debugging - remove when it works!')
    whFactor <-
      which(sapply(as.data.frame(X), class) %in% c('factor', 'character'))
    if (length(whFactor) > 0) {
      for (f in whFactor) {
        cat(f)
        # warn if too many (>5) levels!
        # Unpack them into one-hot format - one-hot-encode(X[,f])
        # name them as var_level
        # add them to X
      }
      X <- X[, -whFactor] # Remove original factor variables
    }
  }

  # Remove nearZeroVariance variables for PLS
  if (methParam$NZV) {
    nzv <- MUVR2::nearZeroVar(X) # Function borrowed from mixOmics
    if (length(nzv$Position) > 0) {
      modelReturn$nzv <- colnames(X)[nzv$Position]
      X <- X[, -nzv$Position]
      cat(
        '\n',
        length(nzv$Position),
        'variables with near zero variance detected -> removed from X and stored under $nzv'
      )
    }
  }

  # Sort out which are the "keep" variables (after one-hot and NZV)

  # Number of samples and variables
  nSamp <- nrow(X)
  nVar <- nVar0 <- ncol(X)

  # Sample identifiers; Assume independence if ID is not specified
  if (missing(ID)) {
    cat('\nMissing ID -> Assume all unique (i.e. sample independence)')
    ID <- 1:nSamp
  }

  # Figure out number of iterations in inner CV loop
  # Gets tweaked for "keeps"
  # lower limit is max(c(2, keeps))
  #
  var <- numeric()
  cnt <- 0
  while (nVar > 1) {
    cnt <- cnt + 1
    var <- c(var, nVar)
    nVar <- floor(varRatio * nVar)
  }

  # Number of inner segments
  if (missing(nInner)) {
    nInner <- nOuter - 1
  } # Default value for inner segments

  # methParams
  if (any(names(list(...)) == 'nCompMax')) {
    stop(
      '`nCompMax` is deprecated. Use customParams() and the methParam argument in MUVR instead.'
    )
  }
  if (missing(methParam)) {
    methParam <- customParams(method = method)
  }

  # Set up randomForest package
  if (method == 'RF') {
    if (methParam$rfMethod == 'randomForest') {
      library(randomForest)
    } else if (methParam$rfMethod == 'ranger') {
      library(ranger)
      # } else if (methParam$rfMethod == 'Rborist') {
      # library(Rborist)
    } else
      stop('Random Forest method incorrectly specified in methParam')
  }

  # Set up for multilevel analysis
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
    cat('\nMultilevel -> Regression on (-1, 1) & fitness = "MISS"')
  }

  # No missingness allowed - Please perform imputations before running MUVR
  if (any(is.na(X)) |
      any(is.na(Y))) {
    stop('No missing values allowed in X or Y data.')
  }
  if (!is.null(dim(Y))) {
    stop('Y is not a vector.')
  }

  # DA / Classification
  if (is.character(Y)) {
    Y <- factor(Y)
  }
  if (is.factor(Y)) {
    cat('\nY is factor -> Classification (',
        length(unique(Y)),
        ' classes)',
        sep = '')
    DA <- TRUE
  }
  if (is.numeric(Y) & DA) {
    Y <- as.factor(Y)
    cat('\nDA = TRUE -> Y as factor -> Classification (',
        length(unique(Y)),
        ' classes)',
        sep = '')
  }

  # Check fitness criterion
  if (missing(fitness)) {
    if (DA) {
      fitness <- 'MISS'
      cat('\nMissing fitness -> MISS')
    } else {
      # I.e. for regression
      fitness <- 'RMSEP'
      cat('\nMissing fitness -> RMSEP')
    }
  }

  # Additional sanity check
  if (nrow(X) != length(Y)) {
    stop('Must have same nSamp in X and Y.')
  }

  # Store indata in list for later model return
  InData <- list(
    X = X,
    Y = Y,
    ID = ID,
    scale = scale,
    nRep = nRep,
    nOuter = nOuter,
    nInner = nInner,
    varRatio = varRatio,
    DA = DA,
    fitness = fitness,
    method = method,
    methParam = methParam,
    ML = ML,
    parallel = parallel
  )

  # Sort sampling based on ID and not index & Allocate prediction
  unik <- !duplicated(ID)  # boolean of unique IDs
  unikID <- ID[unik]       # Actual unique IDs
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
    # Allocate final predictions for min mid and max models
    yPredMin <-
      yPredMid <-
      yPredMax <-
      array(
        dim = c(length(Y), # Rows = number of observations
                length(levels(Y)), # Columns = number of classes in Y
                nRep),
        # 3rd = number of repetitions
        dimnames = list(ID, # Observation ID names
                        levels(Y), # Names of levels in Y
                        paste('Rep', 1:nRep, sep = ''))
      ) # Repetitions
    # Allocate predictions per repetition
    yPredMinR <-
      yPredMidR <-
      yPredMaxR <-
      matrix(
        nrow = length(Y),
        # Like above but lacking 3rd dimension (repetitions)
        ncol = length(levels(Y)),
        dimnames = list(ID, levels(Y))
      )
  } else {
    # I.e. for regression and ML
    # Allocate final predictions for min mid and max models
    yPredMin <-
      yPredMid <-
      yPredMax <-
      matrix(
        nrow = length(Y),
        # Like above but matrix instead of array, since there are not multiple classes
        ncol = nRep,
        dimnames = list(ID, paste('Rep', 1:nRep, sep = ''))
      )
    # Allocate predictions per repetition
    yPredMinR <-
      yPredMidR <-
      yPredMaxR <-
      numeric(length(Y)) # Like above but lacking columns (repetitions) -> numeric vector
  }

  # Allocate response vectors and matrices for var's, nComp and VIP ranks over repetitions
  missRep <- numeric(nRep)
  names(missRep) <- paste(rep('rep', nRep), 1:nRep, sep = '')
  varRepMin <-
    varRepMid <-
    varRepMax <-
    nCompRepMin <- nCompRepMid <- nCompRepMax <- missRep
  nCompSegMin <- nCompSegMid <- nCompSegMax <- matrix(nrow = nRep,
                                                      ncol = nOuter,
                                                      dimnames = list(
                                                        paste('repetition', 1:nRep, sep = ''),
                                                        paste('segment', 1:nOuter, sep =
                                                                '')
                                                      ))
  VIRankRepMin <-
    VIRankRepMid <- VIRankRepMax <- matrix(
      data = nVar0,
      nrow = nVar0,
      ncol = nRep,
      dimnames = list(colnames(X),
                      paste(rep('rep', nRep), 1:nRep, sep = ''))
    )

  # Allocate array for validation results
  VAL <- array(dim = c(nOuter, cnt, nRep),
               dimnames = list(
                 paste('outSeg', 1:nOuter, paste = ''),
                 var,
                 paste(rep('rep', nRep), 1:nRep, sep = '')
               ))

  # Choose package/core algorithm according to chosen method
  # And prepare for exporting them in 'foreach' (below)
  packs <- c('pROC')
  if (method == 'RF') {
    packs <- c(packs, methParam$rfMethod)
  }
  exports <- 'vectSamp'

  ####################################################
  ## Start repetitions
  ####################################################

  # For manual debugging purposes
  # reps <- list()
  # for (r in 1:nRep) {

  reps <-
    foreach(r = 1:nRep,
            .packages = packs,
            .export = exports) %doVersion% {
              # For manual debugging purposes
              # r <- 1
              # r <- r + 1

              # Send intermediate outputs to log file: Not a pretty output. For debugging purposes only.
              if (logg) {
                sink('log.txt', append = TRUE)
              }
              # Intermediate info output
              cat('\n', '   Repetition ', r, ' of ', nRep, ':', sep = '')

              # Allocate output for models (i.e. for later prediction of external samples)
              if (modReturn) {
                outMod <- list()
              }

              # PERFORM SEGMENTATION INTO OUTER SEGMENTS
              if (DA & identical(unikID, ID)) {
                groupTest <- list()  ## Allocate list for samples within group
                for (gT in 1:groups) {
                  groupTest[[gT]] <-
                    vectSamp(groupID[[gT]], n = nOuter)  # Draw random samples within group
                }
                allTest <-
                  groupTest[[1]] # Add 1st groups to 'Master' sample of all groups
                for (gT in 2:groups) {
                  # Add subsequent groups
                  allTest <- allTest[order(sapply(allTest, length))]
                  for (aT in 1:nOuter) {
                    allTest[[aT]] <- sort(c(allTest[[aT]], groupTest[[gT]][[aT]]))
                  }
                }
              } else {
                allTest <- vectSamp(unikID, n = nOuter)
              }

              # Allocate intermediate output objects
              nCompOutMax <- numeric(nOuter)
              names(nCompOutMax) <-
                paste(rep('outSeg', nOuter), 1:nOuter, sep = '')
              varOutMin <-
                varOutMid <-
                varOutMax <- nCompOutMin <- nCompOutMid <- nCompOutMax
              VIRankOutMin <-
                VIRankOutMid <- VIRankOutMax <- matrix(
                  data = nVar0,
                  nrow = nVar0,
                  ncol = nOuter,
                  dimnames = list(colnames(X),
                                  paste(rep(
                                    'outSeg', nOuter
                                  ), 1:nOuter, sep = ''))
                )
              VALRep <- matrix(nrow = nOuter, ncol = cnt)

              # Perform outer loop segments -> one "majority vote" MV model per segment
              for (i in 1:nOuter) {
                # For manual debugging purposes
                # i <- 1
                # i <- i + 1

                # Intermediate info output
                cat('\n Segment ', i, ' (variables):', sep = '') # Counter

                # Draw out test set
                testID <-
                  allTest[[i]] # Draw out segment = holdout set BASED ON UNIQUE ID
                testIndex <-
                  ID %in% testID # Boolean for samples corresponding to unique test IDs
                xTest <- X[testIndex,]
                yTest <- Y[testIndex]

                # Inner data (not in test)
                inID <-
                  unikID[!unikID %in% testID]  # IDs not in test set
                if (DA &
                    identical(unikID, ID)) {
                  inY <-
                    unikY[!unikID %in% testID]
                }  # Counterintuitive, but needed for grouping by Ynames

                # Allocate fitness variables for the inner data
                missIn <-
                  berIn <-
                  aucIn <- rmsepIn <- PRESSIn <- nCompIn <- matrix(
                    nrow = nInner,
                    ncol = cnt,
                    dimnames = list(paste(
                      rep('inSeg', nInner), 1:nInner, sep = ''
                    ),
                    var)
                  )
                # Allocate VIRank variable for the inner data
                VIRankInner <- array(
                  data = nVar0,
                  dim = c(nVar0, cnt, nInner),
                  dimnames = list(colnames(X),
                                  var,
                                  paste(
                                    rep('inSeg', nInner), 1:nInner, sep = ''
                                  ))
                )
                # Set initial inclusion list of variables to all variables
                incVar <- colnames(X)

                # Perform steps with successively fewer variables
                for (count in 1:cnt) {
                  # Build models with successively fewer variables.

                  # For manual debugging purposes
                  # count <- 1 # for testing
                  # count <- count + 1

                  # Extract the number of variables at the present count (according to the count loop before the foreach loop)
                  nVar <- var[count]

                  # Intermediate info output
                  cat(nVar)

                  # Tweak method parameters for low number of variables
                  if (method == 'PLS') {
                    comp <- min(c(nVar, methParam$compMax))
                  } # nComp cannot be > nVar
                  if (method == 'RF') {
                    mtryIn <- ifelse(DA,
                                     min(c(
                                       methParam$mtryMaxIn,
                                       floor(sqrt(nVar))
                                     )), # Standard but with upper limit
                                     min(c(
                                       methParam$mtryMaxIn,
                                       floor(nVar / 3)
                                     ))) # Standard but with upper limit
                    mtryIn <- max(c(2, mtryIn)) # Lower limit
                  }

                  # PERFORM SEGMENTATION INTO INNER SEGMENTS
                  if (DA & identical(unikID, ID)) {
                    groupIDVal <- list()
                    for (g in 1:groups) {
                      groupIDVal[[g]] <- inID[inY == Ynames[g]]  # Find indices per group
                    }
                    groupVal <-
                      list()  ## Allocate list for samples within group
                    for (gV in 1:groups) {
                      groupVal[[gV]] <-
                        vectSamp(groupIDVal[[gV]], n = nInner)  # Draw random samples within group
                    }
                    allVal = groupVal[[1]] # Add 1st groups to 'Master' sample of all groups
                    for (gV in 2:groups) {
                      # Add subsequent groups
                      allVal = allVal[order(sapply(allVal, length))]
                      for (aV in 1:nInner) {
                        allVal[[aV]] <- sort(c(allVal[[aV]], groupVal[[gV]][[aV]]))
                      }
                    }
                  } else {
                    allVal <- vectSamp(inID, n = nInner)
                  }

                  # Perform inner CV loop
                  for (j in 1:nInner) {
                    # For manual debugging purposes
                    # j <- 1
                    # j <- j + 1

                    # Intermediate info output
                    cat('.') # Counter

                    # Extract validation segment
                    valID <- allVal[[j]] # Extract IDs
                    valIndex <-
                      ID %in% valID # Corresponding observations
                    xVal <- X[valIndex,]
                    xVal <-
                      subset(xVal, select = incVar) # limit to the current selection of variables (selection below)
                    yVal <- Y[valIndex]

                    # Extract training data
                    trainID <-
                      inID[!inID %in% valID] # The inner but not validation IDs
                    trainIndex <-
                      ID %in% trainID # Corresponding observations
                    xTrain <- X[trainIndex,]
                    xTrain <-
                      subset(xTrain, select = incVar) # limit to the current selection of variables (selection below)
                    yTrain <- Y[trainIndex]

                    # For debugging
                    # sum(trainIndex,valIndex,testIndex)
                    # trainIndex|valIndex|testIndex

                    # Make inner model
                    if (method == 'PLS') {
                      inMod <- MUVR2::plsInner(xTrain,
                                              yTrain,
                                              xVal,
                                              yVal,
                                              DA,
                                              fitness,
                                              comp,
                                              scale = scale)
                      nCompIn[j, count] <- inMod$nComp
                    } else if (method == 'RF') {
                      inMod <- MUVR2::rfInner(
                        xTrain,
                        yTrain,
                        xVal,
                        yVal,
                        DA,
                        fitness,
                        mtry = mtryIn,
                        ntree = methParam$ntreeIn,
                        method = methParam$rfMethod
                      )
                    } else {
                      stop('No other core modelling techniques available at present.')
                    }

                    # Store fitness metric
                    if (fitness == 'MISS') {
                      missIn[j, count] <- inMod$miss
                    } else if (fitness == 'BER') {
                      berIn[j, count] <- inMod$ber
                    } else if (fitness == 'AUROC') {
                      aucIn[j, count] <- inMod$auc
                    } else {
                      rmsepIn[j, count] <- inMod$rmsep
                      PRESSIn[j, count] <-
                        (inMod$rmsep ^ 2) * length(yVal) # Is this really correct???
                    }

                    # Store VIs
                    VIRankInner[match(names(inMod$virank), rownames(VIRankInner)), count, j] <-
                      inMod$virank
                  }

                  # Average inner VIRank ranks before variable elimination - Tweak for `keeps`
                  VIRankInAve <-
                    apply(VIRankInner[, count, ], 1, mean)
                  # VIRankInAve[keeps] <- 0
                  if (count < cnt) {
                    incVar <-
                      names(VIRankInAve[order(VIRankInAve)])[1:var[count + 1]] # Extract the names of the variables kept for the next iteration
                  }
                }

                # PER OUTER SEGMENT:

                # Store fitness curves and construct fitness rank curves for the different "counts"
                if (fitness == 'AUROC') {
                  fitRank <- colMeans(-aucIn)
                  VALRep[i,] <- colMeans(aucIn)
                } else if (fitness == 'MISS') {
                  fitRank <- VALRep[i,] <- colSums(missIn)
                } else if (fitness == 'BER') {
                  fitRank <- VALRep[i,] <- colMeans(berIn)
                } else {
                  fitRank <- colMeans(rmsepIn)
                  VALRep[i,] <-
                    sqrt(colSums(PRESSIn) / sum(!testIndex))
                }
                # Rescale fitRank to range 0 (best) - 1 (worst)
                fitRank <-
                  (fitRank - min(fitRank)) / abs(diff(range(fitRank)))
                # BugCatch: If all VAL have NaN value -> reset all fitRank to 0
                if (all(is.nan(fitRank))) {
                  fitRank <- rep(0, cnt)
                }

                # Extract index of min and max models (remember that counts and nVar go in opposite direction!)
                minIndex <- max(which(fitRank <= methParam$robust))
                maxIndex <- min(which(fitRank <= methParam$robust))

                # nVar at min, mid and max
                varOutMin[i] <- var[minIndex]
                varOutMax[i] <- var[maxIndex]
                varOutMid[i] <-
                  round(exp(mean(log(c(
                    var[minIndex], var[maxIndex]
                  ))))) # Geometric mean of min and max

                # midIndex is closest to nVarMid
                midIndex <- which.min(abs(var - varOutMid[i]))

                # For PLS, extract number of components
                if (method == 'PLS') {
                  nCompOutMin[i] <- round(mean(nCompIn[, minIndex]))
                  nCompOutMid[i] <- round(mean(nCompIn[, midIndex]))
                  nCompOutMax[i] <- round(mean(nCompIn[, maxIndex]))
                }

                # Average VIRank ranks
                VIRankOutMin[, i] <-
                  apply(VIRankInner[, minIndex,], 1, mean)
                VIRankOutMid[, i] <-
                  apply(VIRankInner[, midIndex,], 1, mean)
                VIRankOutMax[, i] <-
                  apply(VIRankInner[, maxIndex,], 1, mean)

                # Build outer model for min, mid and max and predict YTEST
                # Extract all inner data
                xIn <-
                  X[!testIndex, ] # Perform Validation on all samples except holdout set
                yIn <- Y[!testIndex]

                # Determine consensus choice of included variables for min mid and max
                incVarMin <-
                  rownames(VIRankOutMin)[rank(VIRankOutMin[, i]) <= varOutMin[i]]
                incVarMid <-
                  rownames(VIRankOutMid)[rank(VIRankOutMid[, i]) <= varOutMid[i]]
                incVarMax <-
                  rownames(VIRankOutMax)[rank(VIRankOutMax[, i]) <= varOutMax[i]]

                # Consensus models for PLS
                if (method == 'PLS') {
                  # Build min model
                  if (DA) {
                    plsOutMin <- MUVR2::plsda(
                      subset(xIn, select = incVarMin),
                      yIn,
                      ncomp = nCompOutMin[i],
                      near.zero.var = TRUE,
                      scale = scale
                    )
                  } else {
                    plsOutMin <- MUVR2::pls(
                      subset(xIn, select = incVarMin),
                      yIn,
                      ncomp = nCompOutMin[i],
                      near.zero.var = TRUE,
                      scale = scale
                    )
                  }
                  # Extract test data with correct variable selection
                  xTestMin <- subset(xTest, select = incVarMin)
                  # Extract predictions
                  yPredMinR[testIndex] <- predict(plsOutMin,
                                                  newdata = xTestMin,
                                                  scale = scale)$predict[, , nCompOutMin[i]]  #

                  # Build mid model
                  if (DA) {
                    plsOutMid <- MUVR2::plsda(
                      subset(xIn, select = incVarMid),
                      yIn,
                      ncomp = nCompOutMid[i],
                      near.zero.var = TRUE,
                      scale = scale
                    )
                  } else {
                    plsOutMid <- MUVR2::pls(
                      subset(xIn, select = incVarMid),
                      yIn,
                      ncomp = nCompOutMid[i],
                      near.zero.var = TRUE,
                      scale = scale
                    )
                  }
                  # Extract test data with correct variable selection
                  xTestMid <- subset(xTest, select = incVarMid)
                  # Extract predictions
                  yPredMidR[testIndex] <- predict(plsOutMid,
                                                  newdata = xTestMid,
                                                  scale = scale)$predict[, , nCompOutMid[i]]  #

                  # Build max model
                  if (DA) {
                    plsOutMax <- MUVR2::plsda(
                      subset(xIn, select = incVarMax),
                      yIn,
                      ncomp = nCompOutMax[i],
                      near.zero.var = TRUE,
                      scale = scale
                    )
                  } else {
                    plsOutMax <- MUVR2::pls(
                      subset(xIn, select = incVarMax),
                      yIn,
                      ncomp = nCompOutMax[i],
                      near.zero.var = TRUE,
                      scale = scale
                    )
                  }
                  # Extract test data with correct variable selection
                  xTestMax <- subset(xTest, select = incVarMax)
                  # Extract predictions
                  yPredMaxR[testIndex] <- predict(plsOutMax,
                                                  newdata = xTestMax,
                                                  scale = scale)$predict[, , nCompOutMax[i]]  #

                  # Extract models for external predictions
                  if (modReturn) {
                    outMod[[i]] = list(plsOutMin, plsOutMid, plsOutMax)
                  }
                } else if (method == 'RF') {
                  # min model
                  rfOutMin <- rfPred(
                    xTrain = subset(xIn, select = incVarMin),
                    yTrain = yIn,
                    xTest = subset(xTest, select = incVarMin),
                    yTest = yTest,
                    DA = DA,
                    ntree = methParam$ntreeOut,
                    keep.forest = TRUE,
                    method = methParam$rfMethod
                  )
                  # min predictions
                  if (DA) {
                    yPredMinR[testIndex,] <-
                      rfOutMin$predicted
                  } else{
                    yPredMinR[testIndex] <- rfOutMin$predicted
                  }

                  # mid model
                  rfOutMid <- rfPred(
                    xTrain = subset(xIn, select = incVarMid),
                    yTrain = yIn,
                    xTest = subset(xTest, select = incVarMid),
                    yTest = yTest,
                    DA = DA,
                    ntree = methParam$ntreeOut,
                    keep.forest = TRUE,
                    method = methParam$rfMethod
                  )
                  # mid predictions
                  if (DA) {
                    yPredMidR[testIndex,] <-
                      rfOutMid$predicted
                  } else {
                    yPredMidR[testIndex] <- rfOutMid$predicted
                  }

                  # max model
                  rfOutMax <- rfPred(
                    xTrain = subset(xIn, select = incVarMax),
                    yTrain = yIn,
                    xTest = subset(xTest, select = incVarMax),
                    yTest = yTest,
                    DA = DA,
                    ntree = methParam$ntreeOut,
                    keep.forest = TRUE,
                    method = methParam$rfMethod
                  )
                  # max predictions
                  if (DA) {
                    yPredMaxR[testIndex,] <-
                      rfOutMax$predicted
                  } else {
                    yPredMaxR[testIndex] <- rfOutMax$predicted
                  }

                  # Extract models for external predictions
                  if (modReturn) {
                    outMod[[i]] <- list(
                      rfOutMin = rfOutMin$model,
                      rfOutMid = rfOutMid$model,
                      rfOutMax = rfOutMax$model
                    )
                  }
                } else
                  stop('Other core models not implemented') # Needs to be expanded for other cores (SVM; ANN)
              }

              # PER REPETITION:
              # Organize output

              # Individual predictions
              parReturn <- list(yPredMin = yPredMinR,
                                yPredMid = yPredMidR,
                                yPredMax = yPredMaxR)

              # VI ranks
              parReturn$VIRankRepMin <- rowMeans(VIRankOutMin)
              parReturn$VIRankRepMid <- rowMeans(VIRankOutMid)
              parReturn$VIRankRepMax <- rowMeans(VIRankOutMax)

              # PLS components
              if (method == 'PLS') {
                parReturn$nCompRepMin <-
                  round(mean(nCompOutMin)) # Averaged over the nOuter segments
                parReturn$nCompRepMid <- round(mean(nCompOutMid))
                parReturn$nCompRepMax <- round(mean(nCompOutMax))
                parReturn$nCompSegMin <-
                  nCompOutMin # For the individual segments
                parReturn$nCompSegMid <- nCompOutMid
                parReturn$nCompSegMax <- nCompOutMax
              }

              # Validation curves
              parReturn$VAL <- VALRep

              # Recalculate fitness per repetition
              fitRankRep <- colSums(VALRep)
              if (fitness == 'AUROC') {
                fitRankRep <-
                  -fitRankRep
              }# AUROC fitness (higher is better) goes in opposite direction of all other metrics (lower is better)
              fitRankRep <-
                (fitRankRep - min(fitRankRep)) / abs(diff(range(fitRankRep))) # Rescal fitness curve to scale from 0 (best) to 1 (worst)
              if (all(is.nan(fitRankRep))) {
                fitRankRep <-
                  rep(0, cnt)
              } # If all VAL have same value -> reset all fitRankRep to 0

              # Recalculate min/mid/max nVar per repetition from fitness
              minIndex <- max(which(fitRankRep <= methParam$robust))
              maxIndex <- min(which(fitRankRep <= methParam$robust))
              parReturn$varRepMin <- var[minIndex]
              parReturn$varRepMid <-
                round(exp(mean(log(c(
                  var[minIndex], var[maxIndex]
                ))))) # Geometric mean of min and max
              parReturn$varRepMax <- var[maxIndex]

              # Return underlying models
              if (modReturn) {
                parReturn$outModel <- outMod
              }

              # Stop log
              if (logg) {
                sink()
              }

              # Return repetition results
              return(parReturn)

              # For manual debugging purposes
              # reps[[r]] <- parReturn
            }

  # Unpack the results from the repetitions
  if (modReturn) {
    outMods <- list()
  }
  for (r in 1:nRep) {
    # Individual predictions
    if (DA) {
      yPredMin[, , r] <-
        reps[[r]]$yPredMin
    } else{
      yPredMin[, r] <- reps[[r]]$yPredMin
    }
    if (DA) {
      yPredMid[, , r] <-
        reps[[r]]$yPredMid
    } else {
      yPredMid[, r] <- reps[[r]]$yPredMid
    }
    if (DA) {
      yPredMax[, , r] <-
        reps[[r]]$yPredMax
    } else {
      yPredMax[, r] <- reps[[r]]$yPredMax
    }
    # Number of variables
    varRepMin[r] <- reps[[r]]$varRepMin
    varRepMid[r] <- reps[[r]]$varRepMid
    varRepMax[r] <- reps[[r]]$varRepMax
    # VI Ranks
    VIRankRepMin[, r] <- reps[[r]]$VIRankRepMin
    VIRankRepMid[, r] <- reps[[r]]$VIRankRepMid
    VIRankRepMax[, r] <- reps[[r]]$VIRankRepMax
    # PLS components
    if (method == 'PLS') {
      nCompRepMin[r] <- reps[[r]]$nCompRepMin # Average per repetition
      nCompRepMid[r] <- reps[[r]]$nCompRepMid
      nCompRepMax[r] <- reps[[r]]$nCompRepMax
      nCompSegMin[r,] <-
        reps[[r]]$nCompSegMin # For the individual nOuter segments
      nCompSegMid[r,] <- reps[[r]]$nCompSegMid
      nCompSegMax[r,] <- reps[[r]]$nCompSegMax
    }
    # Validation curves
    VAL[, , r] <- reps[[r]]$VAL
    # Models
    if (modReturn) {
      outMods <- c(outMods, reps[[r]]$outModel)
    }
  }

  # Average predictions
  if (DA) {
    yPred <- list()
    yPred[['min']] <- apply(yPredMin, c(1, 2), mean)
    yPred[['mid']] <- apply(yPredMid, c(1, 2), mean)
    yPred[['max']] <- apply(yPredMax, c(1, 2), mean)
  } else {
    yPred <-
      cbind(apply(yPredMin, 1, mean)[1:nrow(X)],
            # Is the [1:nrow(X)] really necessary?
            apply(yPredMid, 1, mean)[1:nrow(X)],
            apply(yPredMax, 1, mean)[1:nrow(X)])
    colnames(yPred) <- c('min', 'mid', 'max')
    rownames(yPred) <- paste(1:nSamp, ID, sep = '_ID')
  }
  modelReturn$yPred <- yPred # Store averaged predictions
  modelReturn$yPredPerRep <-
    list(minModel = yPredMin,
         # And predictions per repetition
         midModel = yPredMid,
         maxModel = yPredMax)

  # Calculate classification-specific characteristics
  if (DA) {
    # Calculate AUCs per class
    auc <- matrix(
      nrow = 3,
      ncol = length(levels(Y)),
      dimnames = list(c('min', 'mid', 'max'), levels(Y))
    )
    for (cl in 1:length(levels(Y))) {
      auc[1, cl] <- roc(Y == (levels(Y)[cl]),
                        yPred[['min']][, cl],
                        quiet = TRUE)$auc
      auc[2, cl] <- roc(Y == (levels(Y)[cl]),
                        yPred[['mid']][, cl],
                        quiet = TRUE)$auc
      auc[3, cl] <- roc(Y == (levels(Y)[cl]),
                        yPred[['max']][, cl],
                        quiet = TRUE)$auc
    }
    # Classify predictions
    miss <- numeric(3)
    yClass <- data.frame(Y)
    for (mo in 1:3) {
      # mo for model
      classPred <-
        factor(apply(yPred[[mo]], 1, function(x)
          levels(Y)[which.max(x)]), levels = levels(Y))
      miss[mo] <- sum(classPred != Y)
      yClass[, mo] <- classPred
    }
    names(miss) <- colnames(yClass) <- c('min', 'mid', 'max')
    rownames(yClass) <- paste(1:nSamp, ID, sep = '_ID')
    # Report
    modelReturn$yClass <- yClass
    modelReturn$miss <- miss
    modelReturn$auc <- auc
  }

  # Calculate multilevel-specific characteristics
  if (ML) {
    modelReturn$yClass <-
      apply(yPred, 2, function(x)
        ifelse(x > 0, 1, -1))
    modelReturn$miss <-
      apply(modelReturn$yClass, 2, function(x)
        sum(x != Y))
    modelReturn$auc <- apply(yPred, 2, function(x)
      roc(Y, x)$auc)
    colnames(modelReturn$yClass) <-
      names(modelReturn$miss) <-
      names(modelReturn$auc) <- c('min', 'mid', 'max')
    rownames(modelReturn$yClass) <- paste(1:nSamp, ID, sep = '_ID')
  }

  # Average VIRank ranks over repetitions
  VIRank <- cbind(rowMeans(VIRankRepMin),
                  rowMeans(VIRankRepMid),
                  rowMeans(VIRankRepMax))
  colnames(VIRank) <- c('min', 'mid', 'max')
  modelReturn$VIRank <- VIRank
  modelReturn$VIRankPerRep <- list(minModel = VIRankRepMin,
                                   midModel = VIRankRepMid,
                                   maxModel = VIRankRepMax)

  # Recalculate fitness averaged over all the repetitions
  fitRankAll <- apply(VAL, 2, mean)
  if (fitness == 'AUROC') {
    fitRankAll <-
      -fitRankAll
  } # AUROC fitness (higher is better) goes in opposite direction of all other metrics (lower is better)
  fitRankAll <-
    (fitRankAll - min(fitRankAll)) / abs(diff(range(fitRankAll))) # rescale from 0 (best) to 1 (worst)
  if (all(is.nan(fitRankAll))) {
    fitRankAll <-
      rep(0, cnt)
  } # If all VAL have same value -> reset all fitRankAll to 0

  # Calculate min/mid/max indices and nVar
  minIndex <- max(which(fitRankAll <= methParam$robust))
  maxIndex <- min(which(fitRankAll <= methParam$robust))
  nVar <- c(var[minIndex],
            round(exp(mean(log(
              c(var[minIndex], var[maxIndex])
            )))),
            var[maxIndex])
  names(nVar) <- c('min', 'mid', 'max')
  modelReturn$nVar <- nVar
  modelReturn$nVarPerRep <- list(minModel = varRepMin,
                                 midModel = varRepMid,
                                 maxModel = varRepMax)

  # PLS components
  if (method == 'PLS') {
    # Average nComp over repetitions
    nComp <- c(round(mean(nCompRepMin)),
               round(mean(nCompRepMid)),
               round(mean(nCompRepMax)))
    names(nComp) <- c('min', 'mid', 'max')
    modelReturn$nComp <- nComp
    modelReturn$nCompPerRep <- list(minModel = nCompRepMin,
                                    midModel = nCompRepMid,
                                    maxModel = nCompRepMax)
    modelReturn$nCompPerSeg <- list(minModel = nCompSegMin,
                                    midModel = nCompSegMid,
                                    maxModel = nCompSegMax)
  }

  # Store validation metric and curves
  modelReturn$VAL$metric <- fitness
  modelReturn$VAL$VAL <- VAL

  # Store segment models
  if (modReturn) {
    modelReturn$outModels <- outMods
  }

  # Store inData
  modelReturn$inData <- InData

  ## Build overall "Fit-Predict" models for calculating R2 and visualisations
  incVarMin <- names(VIRank[rank(VIRank[, 1]) <= round(nVar[1]), 1])
  incVarMid <- names(VIRank[rank(VIRank[, 2]) <= round(nVar[2]), 2])
  incVarMax <- names(VIRank[rank(VIRank[, 3]) <= round(nVar[3]), 3])
  if (method == 'PLS') {
    ######################
    # PLS Min fit-predict
    ######################
    if (DA) {
      plsFitMin <- MUVR2::plsda(
        subset(X, select = incVarMin),
        Y,
        ncomp = round(nComp[1]),
        near.zero.var = TRUE,
        scale = scale
      )
    } else {
      plsFitMin <- MUVR2::pls(
        subset(X, select = incVarMin),
        Y,
        ncomp = round(nComp[1]),
        near.zero.var = TRUE,
        scale = scale
      )
    }
    # Exclude potential near zero variance variables
    if (length(plsFitMin$nzv$Position) > 0) {
      incVarMin <-
        incVarMin[!incVarMin %in% rownames(plsFitMin$nzv$Metrics)]
    }
    # Make Min predictions
    yFitMin <- predict(plsFitMin,
                       newdata = subset(X, select = incVarMin),
                       scale = scale)$predict[, , nComp[1]]  #
    ######################
    # PLS Mid fit-predict
    ######################
    if (DA) {
      plsFitMid <- MUVR2::plsda(
        subset(X, select = incVarMid),
        Y,
        ncomp = round(nComp[2]),
        near.zero.var = TRUE,
        scale = scale
      )
    } else {
      plsFitMid <- MUVR2::pls(
        subset(X, select = incVarMid),
        Y,
        ncomp = round(nComp[2]),
        near.zero.var = TRUE,
        scale = scale
      )
    }
    # Exclude potential near zero variance variables
    if (length(plsFitMid$nzv$Position) > 0) {
      incVarMid <-
        incVarMid[!incVarMid %in% rownames(plsFitMid$nzv$Metrics)]
    }
    # Make Mid predictions
    yFitMid <- predict(plsFitMid,
                       newdata = subset(X, select = incVarMid),
                       scale = scale)$predict[, , nComp[2]]  #
    ######################
    # PLS Max fit-predict
    ######################
    if (DA) {
      plsFitMax <- MUVR2::plsda(
        subset(X, select = incVarMax),
        Y,
        ncomp = round(nComp[3]),
        near.zero.var = TRUE,
        scale = scale
      )
    } else {
      plsFitMax <- MUVR2::pls(
        subset(X, select = incVarMax),
        Y,
        ncomp = round(nComp[3]),
        near.zero.var = TRUE,
        scale = scale
      )
    }
    # Exclude potential near zero variance variables
    if (length(plsFitMax$nzv$Position) > 0) {
      incVarMax <-
        incVarMax[!incVarMax %in% rownames(plsFitMax$nzv$Metrics)]
    }
    # Make Max predictions
    yFitMax <- predict(plsFitMax,
                       newdata = subset(X, select = incVarMax),
                       scale = scale)$predict[, , nComp[3]]  #

    # Combine fit-predictions
    yFit <- cbind(yFitMin, yFitMid, yFitMax)
    yRep <- ncol(yFit) / 3
    colnames(yFit) <- rep(c('min', 'mid', 'max'), each = yRep)
    rownames(yFit) <- ID
    # Report fit-predict
    modelReturn$Fit <- list(
      yFit = yFit,
      plsFitMin = plsFitMin,
      plsFitMid = plsFitMid,
      plsFitMax = plsFitMax
    )
  } else if (method == 'RF') {
    ######################
    # RF Min fit-predict
    ######################
    rfFitMin <-
      suppressWarnings(rfPred(
        xTrain = subset(X, select = incVarMin),
        yTrain = Y,
        DA = DA,
        method = methParam$rfMethod
      ))
    yFitMin <- rfFitMin$fit
    ######################
    # RF Mid fit-predict
    ######################
    rfFitMid <-
      suppressWarnings(rfPred(
        xTrain = subset(X, select = incVarMid),
        yTrain = Y,
        DA = DA,
        method = methParam$rfMethod
      ))
    yFitMid <- rfFitMid$fit
    ######################
    # RF Max fit-predict
    ######################
    rfFitMax <-
      suppressWarnings(rfPred(
        xTrain = subset(X, select = incVarMax),
        yTrain = Y,
        DA = DA,
        method = methParam$rfMethod
      ))
    yFitMax <- rfFitMax$fit
    # Combine fit-predictions
    yFit <- cbind(yFitMin, yFitMid, yFitMax)
    yRep <- ncol(yFit) / 3
    colnames(yFit) <- rep(c('min', 'mid', 'max'), each = yRep)
    rownames(yFit) <- ID
    modelReturn$Fit <- list(
      yFit = yFit,
      rfFitMin = rfFitMin,
      rfFitMid = rfFitMid,
      rfFitMax = rfFitMax
    )
  } else
    stop ('Other ML methods not implemented')

  # Calculate fit statistics
  if (!DA) {
    TSS <- sum((Y - mean(Y)) ^ 2)
    RSSMin <- sum((Y - yFitMin) ^ 2)
    RSSMid <- sum((Y - yFitMid) ^ 2)
    RSSMax <- sum((Y - yFitMax) ^ 2)
    PRESSMin <- sum((Y - yPred[, 1]) ^ 2)
    PRESSMid <- sum((Y - yPred[, 2]) ^ 2)
    PRESSMax <- sum((Y - yPred[, 3]) ^ 2)
    R2 <- c(1 - (RSSMin / TSS),
            1 - (RSSMid / TSS),
            1 - (RSSMax / TSS))
    Q2 <- c(1 - (PRESSMin / TSS),
            1 - (PRESSMid / TSS),
            1 - (PRESSMax / TSS))
    names(R2) <- names(Q2) <- c('min', 'mid', 'max')
    # Report
    modelReturn$fitMetric <- list(R2 = R2,
                                  Q2 = Q2)
  } else {
    modelReturn$fitMetric <- list(CR = 1 - (miss / length(Y)))
  }

  # Set class
  class(modelReturn) <- c('MUVR',
                          ifelse(DA,
                                 'Classification',
                                 ifelse(ML,
                                        'Multilevel',
                                        'Regression')),
                          method)

  # Stop timer
  end.time <- proc.time()[3]
  modelReturn$calcMins <- (end.time - start.time) / 60

  # Output
  cat('\n Elapsed time', modelReturn$calcMins, 'mins \n')

  # Return final object
  return(modelReturn)
}
