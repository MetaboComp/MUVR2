#' rdCV: Wrapper for repeated double cross-validation without variable selection
#' @param X Independent variables. NB: Variables (columns) must have names/unique identifiers. NAs not allowed in data. For ML, X is upper half only (X1-X2)
#' @param Y Response vector (Dependent variable). For DA (classification), Y should be factor or character. For ML, Y is omitted. For regression, Y is numeric.
#' @param ID Subject identifier (for sampling by subject; Assumption of independence if not specified)
#' @param nRep Number of repetitions of double CV.
#' @param nOuter Number of outer CV loop segments.
#' @param nInner Number of inner CV loop segments.
#' @param DA Logical for Classification (discriminant analysis) (Defaults do FALSE, i.e. regression). PLS is limited to two-class problems (see `Y` above).
#' @param fitness Fitness function for model tuning (choose either 'AUROC' or 'MISS'or 'BER' for classification; or 'RMSEP' (default) for regression.)
#' @param method Multivariate method. Supports 'PLS' and 'RF' (default)
#' @param methParam List with parameter settings for specified MV method (defaults to ???)
#' @param ML Logical for multilevel analysis (defaults to FALSE)
#' @param modReturn Logical for returning outer segment models (defaults to FALSE)
#' @param logg Logical for whether to sink model progressions to `log.txt`
#' @return An object containing stuff...
#' @export

rdCV <- function(X,
                 Y,
                 ID,
                 nRep = 5,
                 nOuter = 6,
                 nInner,
                 DA = FALSE,
                 fitness = c('AUROC', 'MISS', 'RMSEP', "BER"),
                 method = c('PLS', 'RF'),
                 methParam,
                 ML = FALSE,
                 modReturn = FALSE,
                 logg = FALSE) {
  ### Code adapted from MVWrap - Brokenness and oddities are most likely due to this
  library(pROC)
  # Initialize modelReturn with function call
  modelReturn <- list(call = match.call())
  # Start timer
  start.time <- proc.time()[3]

  # Check indata
  if (is.null(dim(X))) {
    cat('\nError: Wrong format of X matrix.\n')
    return(NULL)
  }
  if (!is.null(dim(Y))) {
    ####Y must be only one variable
    cat('\nY is not a vector: Return NULL')
    return(NULL)
  }
  nSamp <-
    nrow(X)            ###nSamp number of observation number in X
  nVar <- nVar0 <-
    ncol(X)      ####nVar=nVar0 number of variables in X

  if (missing(ID)) {
    cat('\nMissing ID -> Assume all unique (i.e. sample independence)')
    ID <- 1:nSamp
  }
  if (missing(nInner)) {
    nInner <- nOuter - 1
  }
  if (missing(method)) {
    method <- 'RF'
  }
  if (method == 'RF') {
    library(randomForest)
  } else {
    library(mixOmics)
  }


  if (missing(methParam)) {
    if (method == 'PLS') {
      methParam <-
        list(
          compMax = ifelse(nVar < 5, nVar, 5),
          mode = 'regression',
          method = "PLS"
        )  ##if the variable number is less than 5 then the component number cannot be 5
    } else {
      methParam <- customParams(method = "RF")
    }
  }

  ##############################################################################################
  #(1)When ML, what if Y is not missing
  #

  ###############################################################################################33333


  if (ML) {
    X <- rbind(X, -X)
    if (missing(Y)) {
      Y <- rep(c(-1, 1), each = nSamp)
    }
    nSamp <- 2 * nSamp
    ID <- c(ID, ID)
    DA <- FALSE
    fitness <- 'MISS'
    cat('\nMultilevel -> Regression on (-1,1) & fitness=MISS')
  }


  if (is.character(Y)) {
    Y <- factor(Y)
  }
  if (is.factor(Y)) {
    cat('\nY is factor -> Classification (',
        length(unique(Y)),
        ' classes)',
        sep = '')
    DA <- TRUE
  } else{
    DA <- FALSE
  }

  if (is.numeric(Y) & DA) {
    Y <- as.factor(Y)
    cat('\nDA=TRUE -> Y as factor -> Classification (',
        length(unique(Y)),
        ' classes)',
        sep = '')
  }
  if (missing(fitness)) {
    if (DA) {
      if (length(unique(Y)) > 2) {
        ##when y class>2
        fitness <- 'MISS'
        cat('\nMissing fitness -> MISS')
      } else if (length(levels(Y)) > 2) {
        fitness <- 'AUROC'
        cat('\nMissing fitness -> AUROC')
      } else {
        fitness <- 'BER'
        cat('\nMissing fitness -> BER')
      }
    } else {
      fitness <- 'RMSEP'
      cat('\nMissing fitness -> RMSEP')
    }
  }


  if (nrow(X) != length(Y)) {
    cat('\nMust have same nSamp in X and Y: Return NULL')
    return(NULL)
  }
  ## Store indata in list for later model return
  InData <- list(
    X = X,
    Y = Y,
    ID = ID,
    nRep = nRep,
    nOuter = nOuter,
    nInner = nInner,
    DA = DA,
    fitness = fitness,
    method = method,
    methParam = methParam,
    ML = ML
  )
  ## Sort sampling based in subjects and not index
  unik = !duplicated(ID)  # boolean of unique IDs: the first unique ID is true
  unikID <-
    ID[unik]       ##all the ID that is unik. It is a logical vector

  if (DA == TRUE) {
    unikY <-
      Y[unik]  # Counterintuitive, but needed for groupings by Ynames
    ##those Y of the people who have unik ID
    Ynames <-
      sort(unique(Y))  # Find groups and order them in sequence, could be y group names
    groups <- length(Ynames) # Number of groups
    groupID <- list()  # Allocate list for indices of groups

    for (g in 1:groups) {
      groupID[[g]] <- unikID[unikY == Ynames[g]]  # Find indices per group
      ##find the ID in the people that has unik ID and see if this Y value is in each group
    }  ##group ID is a list of length of Y groups, each group contains the unikId of those people whose Y belongs to this group
    yPred <- array(dim = c(length(Y),         ###row is observations
                           length(levels(Y)), ##column is groups
                           nRep))             ##list is repetitions
    colnames(yPred) <- levels(Y)
    dimnames(yPred)[[3]] <- paste('Rep', 1:nRep, sep = '')
    yPredR <- matrix(nrow = length(Y),
                     ncol = length(levels(Y)))
    colnames(yPredR) <- levels(Y)
  } else {
    ##if not DA
    yPred <- matrix(nrow = length(Y),
                    ncol = nRep)
    colnames(yPred) <- paste('Rep', 1:nRep, sep = '')
    yPredR <- numeric(length(Y))
  }
  # Allocate response vectors and matrices for var's, nComp and VIP ranks over repetitions
  nCompRep <- missRep <- berRep <- numeric(nRep)    ##length is nRep
  names(nCompRep) <-
    names(missRep) <-
    names(berRep) <- paste(rep('rep', nRep), 1:nRep, sep = '')

  VIRankRep <-
    matrix(data = nVar0,
           ###nVar0 is ncol(X) it is the each value in the matrix
           nrow = nVar0,
           ##row is X variable
           ncol = nRep)     ##column is repetition

  rownames(VIRankRep) <- colnames(X)
  colnames(VIRankRep) <- paste(rep('rep', nRep), 1:nRep, sep = '')

  VAL <- matrix(nrow = nOuter,
                ncol = nRep)
  rownames(VAL) <- paste('outSeg', 1:nOuter, paste = '')
  colnames(VAL) <- paste('rep', 1:nRep, paste = '')

  ## Choose package/core algorithm according to chosen method
  packs <- c(ifelse(method == 'PLS',
                    'mixOmics',
                    'randomForest'),
             'pROC')
  exports <- c(ifelse(method == 'PLS',
                      'plsInner',
                      'rfInner'),
               'vectSamp')

  ## Start repetitions
  #reps=list()
  #for (r in 1:nRep){    ##This is used when there is repetition
  reps <- foreach(r = 1:nRep,
                  .packages = packs,
                  .export = exports) %dopar% {
                    # r=1
                    # r=r+1
                    if (logg) {
                      sink('log.txt', append = TRUE)
                    }  ###Logical for whether to sink model progressions to `log.txt`

                    if (modReturn) {
                      outMod <-
                        list()
                    } ##Logical for returning outer segment models (defaults to FALSE)

                    cat('\n', '   Repetition ', r, ' of ', nRep, ':', sep =
                          '')

                    if (DA) {
                      groupTest <- list()  ## Allocate list for samples within group

                      ###To assign the people in Y group1 to
                      for (gT in 1:groups) {
                        groupTest[[gT]] <-
                          vectSamp(groupID[[gT]], n = nOuter)  # Draw random samples within each group
                      }   ##each groupTest[[gT]] is a list of list , each group[[gT]] contains nOuter list

                      allTest <-
                        groupTest[[1]] # Add 1st groups to 'Master' sample of all groups

                      for (gT in 2:groups) {
                        # Add subsequent groups
                        allTest <-
                          allTest[order(sapply(allTest, length))]

                        ###order: the first lowest number is in position
                        ## this is to reorder the items in each list

                        for (aT in 1:nOuter) {
                          allTest[[aT]] <- sort(c(allTest[[aT]], groupTest[[gT]][[aT]]))
                          ###grouptest is a list of list the small list is nOuter items in each Y group,
                          ###the big list is Y groups
                        }
                      }
                      ####After this step, there is a list of nOter groups

                    } else {
                      ###when it is not DA
                      allTest <- vectSamp(unikID, n = nOuter)
                    }
                    nCompOut <- numeric(nOuter)
                    names(nCompOut) <-
                      paste(rep('outSeg', nOuter), 1:nOuter, sep = '')

                    VIRankOut <- matrix(data = nVar0,
                                        ##ncol(X)
                                        nrow = nVar0,
                                        ##row is X variable numbers
                                        ncol = nOuter)  ##column is nOuter names
                    rownames(VIRankOut) <- colnames(X)
                    colnames(VIRankOut) <-
                      paste(rep('outSeg', nOuter), 1:nOuter, sep = '')
                    VALRep <- matrix(nrow = nOuter,
                                     ncol = 1)


                    ## Perform outer loop segments -> one "majority vote" MV model per segment
                    ###ONE "MAJORITY VOTE" MODEL PER SEGMENT

                    for (i in 1:nOuter) {
                      # i=1
                      # i=i+1
                      ###Choose the number ith nOuter as test set in turn
                      cat('\n Segment ', i, ':', sep = '') # Counter
                      ## Draw out test set
                      testID <-
                        allTest[[i]] # Draw out segment = holdout set BASED ON UNIQUE ID
                      ###the unique ID is used, there are nOuter outer segments for all unique ID
                      testIndex <-
                        ID %in% testID # Logical for samples included

                      xTest <-
                        X[testIndex,]   ## chosse the first Outer segment as tes set
                      yTest <- Y[testIndex]

                      inID <-
                        unikID[!unikID %in% testID]  # IDs not in test set

                      if (DA == TRUE) {
                        inY <-
                          unikY[!unikID %in% testID]
                      } # Counterintuitive, but needed for grouping by Ynames

                      ## Allocate variables for later use
                      ## for inner segment
                      missIn <-
                        berIn <-
                        aucIn <-
                        rmsepIn <-
                        PRESSIn <- nCompIn <- matrix(nrow = nInner, ncol = 1)
                      rownames(rmsepIn) <-
                        rownames(berIn) <-
                        rownames(PRESSIn) <-
                        rownames(missIn) <-
                        rownames(aucIn) <-
                        rownames(nCompIn) <-
                        paste(rep('inSeg', nInner), 1:nInner, sep = '')
                      colnames(rmsepIn) <-
                        colnames(berIn) <-
                        colnames(PRESSIn) <-
                        colnames(missIn) <-
                        colnames(aucIn) <- colnames(nCompIn) <- nVar
                      ##nVar is ncol(X)

                      VIRankInner <- matrix(data = nVar0,
                                            nrow = nVar0,
                                            ncol = nInner)
                      rownames(VIRankInner) <- colnames(X)
                      colnames(VIRankInner) <-
                        paste(rep('inSeg', nInner), 1:nInner, sep = '')
                      ## Perform steps with successively fewer variables
                      if (method == 'PLS') {
                        comp <- ifelse(nVar < methParam$compMax, nVar, methParam$compMax)
                      }
                      if (method == 'RF') {
                        mtryIn <- ifelse(
                          DA,
                          ifelse(
                            sqrt(nVar) > methParam$mtryMaxIn,
                            methParam$mtryMaxIn,
                            floor(sqrt(nVar))
                          ),
                          ifelse((nVar / 3) > methParam$mtryMaxIn,
                                 methParam$mtryMaxIn,
                                 floor(nVar / 3)
                          )
                        )
                        mtryIn <- ifelse(mtryIn < 2, 2, mtryIn)
                      }
                      if (DA) {
                        groupIDVal <- list()
                        for (g in 1:groups) {
                          groupIDVal[[g]] <- inID[inY == Ynames[g]]  # Find indices per group
                        }
                        ## a list: each item is a Y group, shows the ID of those who is in this Y group
                        groupVal <-
                          list()  ## Allocate list for samples within group
                        for (gV in 1:groups) {
                          groupVal[[gV]] <-
                            vectSamp(groupIDVal[[gV]], n = nInner)  # Draw random samples within group
                        }

                        allVal <-
                          groupVal[[1]] # Add 1st groups to 'Master' sample of all groups
                        for (gV in 2:groups) {
                          # Add subsequent groups
                          allVal <-
                            allVal[order(sapply(allVal, length))]
                          for (aV in 1:nInner) {
                            allVal[[aV]] <- sort(c(allVal[[aV]], groupVal[[gV]][[aV]]))
                          }
                        }
                      } else {
                        allVal <- vectSamp(inID, n = nInner)
                      }

                      ## Inner CV loop
                      for (j in 1:nInner) {
                        # j <- 1
                        # j <- j+1
                        cat('.') # Counter
                        valID <-
                          allVal[[j]] # Draw out segment = validation set
                        ##in the jth nInner in inner segments each allVal[[i]] has all Y group data
                        valIndex <-
                          ID %in% valID     ##also included those ID that are not unik

                        xVal <- X[valIndex,]   ##ID
                        yVal <- Y[valIndex]    ##ID

                        trainID <-
                          inID[!inID %in% valID]   ##the id is not in the validation Id, which is training ID
                        trainIndex <-
                          ID %in% trainID # Define Training segment
                        xTrain <- X[trainIndex,]
                        yTrain <- Y[trainIndex]
                        # sum(trainIndex,valIndex,testIndex)
                        # trainIndex|valIndex|testIndex
                        ## Make inner model
                        if (method == 'PLS') {
                          inMod <- plsInner(xTrain,
                                            yTrain,
                                            xVal,
                                            yVal,
                                            DA,
                                            fitness,
                                            comp)
                          nCompIn[j, 1] <- inMod$nComp
                        } else {
                          inMod <- rfInner(
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
                        }
                        # Store fitness metric
                        if (fitness == 'MISS') {
                          missIn[j, 1] <- inMod$miss   ###matrix(nrow=nInner,ncol=1)
                        } else if (fitness == 'AUROC') {
                          aucIn[j, 1] <- inMod$auc     ###row is X variable numbers
                        } else if (fitness == 'BER') {
                          berIn[j, 1] <- inMod$ber     ###row is X variable numbers
                        } else {
                          rmsepIn[j, 1] <- inMod$rmsep
                          PRESSIn[j, 1] <-
                            (inMod$rmsep ^ 2) * length(yVal)
                        }

                        # Store VIRanks
                        VIRankInner[match(names(inMod$virank),
                                          rownames(VIRankInner)), j] <-
                          inMod$virank
                        ## match function in R with vectors
                        ##v1 <- c(2,5,6,3,7)
                        ##v2 <- c(15,16,7,3,2,7,5)
                        ##match(v1,v2)
                        ## 5 7 NA 4 3
                        ##Here VIRankInner rearranged as the name sequence as the inMod$virank

                      }    ###col ncol(X),
                      ####################################################################where Inner ends

                      if (fitness == 'AUROC') {
                        fitRank <- -aucIn       ###-aucin smaller the better
                        fitRank[] <- rank(fitRank)
                        fitRank <- colMeans(fitRank)
                        VALRep[i,] <-
                          colMeans(aucIn)   #colmeans of aucIn
                      } else if (fitness == 'MISS') {
                        fitRank <- missIn
                        fitRank[] <- rank(fitRank)
                        fitRank <- colMeans(fitRank)
                        VALRep[i,] <-
                          colSums(missIn)   ##colsum of miss in
                      } else if (fitness == 'BER') {
                        fitRank <- berIn
                        fitRank[] <- rank(fitRank)
                        fitRank <- colMeans(fitRank)
                        VALRep[i,] <-
                          colSums(berIn)   ##colsum of ber in
                      } else {
                        fitRank <- rmsepIn
                        fitRank[] <- rank(fitRank)
                        fitRank <- colMeans(fitRank)
                        VALRep[i,] <-
                          sqrt(colSums(PRESSIn) / sum(!testIndex))
                      }

                      # Per outer segment: Average inner loop variables, nComp and VIP ranks
                      if (method == 'PLS') {
                        nCompOut[i] <- round(mean(nCompIn[, 1]))
                      }

                      VIRankOut[, i] <-
                        apply(VIRankInner, 1, mean, na.rm = TRUE)

                      # Build outer model for min and max nComp and predict YTEST
                      xIn <-
                        X[!testIndex,] # Perform Validation on all samples except holdout set
                      yIn <- Y[!testIndex]


                      if (method == 'PLS') {
                        if (DA) {
                          plsOut <- MUVR2::plsda(xIn,
                                                yIn,
                                                ncomp = nCompOut[i])
                        } else {
                          plsOut <- MUVR2::pls(xIn,
                                              yIn,
                                              ncomp = nCompOut[i])
                        }
                        if (length(plsOut$nzv$Position) > 0) {
                          removeVar <- rownames(plsOut$nzv$Metrics)
                        }
                        else {
                          removeVar <- NA
                        }

                        incVar <-
                          colnames(X)[!colnames(X) %in% removeVar] ##the X variables that does not have nzv

                        xTest <- subset(xTest, select = incVar)

                        yPredR[testIndex] <- predict(plsOut,
                                                     newdata = xTest)$predict[, , nCompOut[i]]  #
                        # Prediction of newdata
                        if (modReturn) {
                          outMod[[i]] <- plsOut
                        }
                      }
                      else {
                        if (DA == TRUE) {
                          if (length(levels(yIn)) != length(levels(droplevels(yIn)))) {
                            #xIn<-Xotu[1:15,]
                            #yIn<-Yotu[1:15]
                            #xVal<-Xotu[16:29,]
                            #yVal<-Yotu[16:29]
                            #original_levels_yIn<-levels(yTrain)
                            #original_levels_yVal<-levels(yVal)
                            #real_levels_yIn<-levels(droplevels(yTrain))
                            #real_levels_yTest<-levels(droplevels(yTest))
                            yIn <- droplevels(yIn)
                            xTest <-
                              xVal[!is.na(factor(yVal, levels = levels(yIn))),]
                            yTest <-
                              factor(yTest[!is.na(factor(yTest, levels = levels(yIn)))], levels = levels(yIn))

                          }
                        }
                        rfOut <- randomForest(xIn,
                                              yIn,
                                              xTest,
                                              yTest)
                        if (DA == TRUE) {
                          yPredR[testIndex,] <-
                            rfOut$test$votes              ####DA output is vote
                        } else {
                          yPredR[testIndex] <-
                            rfOut$test$predicted           ##not DA out put is predicted
                        }
                        if (modReturn) {
                          outMod[[i]] <- rfOut
                        }
                      }
                    }
                    ####################################################################where Outer ends


                    # Per repetition: Average outer loop variables, nComp and VIP ranks
                    parReturn <- list(yPred = yPredR)
                    parReturn$VIRankRep <-
                      apply(VIRankOut, 1, mean, na.rm = TRUE)
                    if (method == 'PLS') {
                      parReturn$nCompRep <- round(mean(nCompOut))
                    }
                    parReturn$VAL <- VALRep

                    if (modReturn) {
                      parReturn$outModel <- outMod
                    }           ##this is the outModel that returns

                    if (logg) {
                      sink()
                    }
                    return(parReturn)
                    # reps[[r]]=parReturn
                  }

  ################where repetition ends
  if (modReturn) {
    outMods <- list()
  }
  for (r in 1:nRep) {
    if (DA == TRUE) {
      yPred[, , r] <- reps[[r]]$yPred
    } ###reps is a list
    else {
      yPred[, r] <- reps[[r]]$yPred
    }

    VIRankRep[, r] <- reps[[r]]$VIRankRep

    if (method == 'PLS') {
      nCompRep[r] <- reps[[r]]$nCompRep
    }
    VAL[, r] <- reps[[r]]$VAL
    if (modReturn == TRUE) {
      outMods <- c(outMods, reps[[r]]$outModel)
    }
  }

  # Average predictions
  if (DA == TRUE) {
    yPredAve <- apply(yPred, c(1, 2), mean, na.rm = TRUE)
  } else {
    yPredAve <- apply(yPred, 1, mean, na.rm = TRUE)
  }

  modelReturn$yPred <- yPredAve

  if (DA == TRUE) {
    auc <- numeric(length(levels(Y)))
    names(auc) <- levels(Y)
    for (cl in 1:length(levels(Y))) {
      auc[cl] <- roc(Y == (levels(Y)[cl]),
                     yPredAve[, cl])$auc
    }
    # Classify predictions
    yClass <- as.factor(apply(yPredAve,
                              1,
                              function(x)
                                levels(Y)[which.max(x)]))
    yClass <- factor(yClass, levels = levels(Y))
    miss <- sum(yClass != Y)
    ber <- getBER(actual = Y,
                  predicted = yClass)
    modelReturn$yClass <- yClass
    modelReturn$miss <- miss
    modelReturn$auc <- auc
    modelReturn$ber <- ber
  } else if (ML == TRUE) {
    modelReturn$yClass <- ifelse(yPredAve >= 0, 1, -1)
    modelReturn$miss <- sum(modelReturn$yClass != Y)
    modelReturn$ber <- getBER(actual = Y,
                              predicted = modelReturn$yClass)
    modelReturn$auc <- roc(Y,
                           yPredAve)$auc
  }
  # Average VIP ranks over repetitions
  VIRank <- apply(VIRankRep, 1, mean, na.rm = TRUE)
  modelReturn$VIRank <- VIRank
  modelReturn$VIRankPerRep <- VIRankRep

  # Average nVar over repetitions
  if (method == 'PLS') {
    # Average nComp over repetitions
    nComp <- mean(nCompRep)
    modelReturn$nComp <- nComp
  }
  modelReturn$VAL$metric <- fitness
  modelReturn$VAL$VAL <- VAL
  if (modReturn) {
    modelReturn$outModels <- outMods
  }
  modelReturn$yPredPerRep <- yPred
  if (method == 'PLS') {
    modelReturn$nCompPerRep <- nCompRep
  }
  modelReturn$inData <- InData


  ## Build overall "Fit" method for calculating R2 and visualisations
  if (method == 'PLS') {
    if (DA == TRUE) {
      plsFit <- MUVR2::plsda(X, Y, ncomp = round(nComp))
    } else
    {
      plsFit <- MUVR2::pls(X, Y, ncomp = round(nComp))
    }

    if (length(plsFit$nzv$Position) > 0) {
      removeVar <- rownames(plsFit$nzv$Metrics)
    } else {
      removeVar <- NA
    }
    incVar <- colnames(X)[!colnames(X) %in% removeVar]
    yFit <- predict(plsFit,
                    newdata = subset(X, select = incVar))$predict[, , round(nComp)]  #
    modelReturn$Fit <-
      list(yFit = yFit,     ##yFit is the predicted one when removed nzc
           plsFit = plsFit) ##plsFir is a list of pls result
    # Prediction of newdata
  } else {
    rfFit <- randomForest(X, Y)          ###ranger needs to be indide
    if (DA == TRUE) {
      yFit <- rfFit$votes
    } else {
      yFit <- rfFit$predicted
    }
    modelReturn$Fit <- list(yFit = yFit,     ###the predicted one
                            rfFit = rfFit)   ###rfFit is the result of randomForest. It is a list
  }

  # Calculate fit statistics
  if (DA == FALSE) {
    TSS <- sum((Y - mean(Y)) ^ 2)
    RSS <- sum((Y - yFit) ^ 2)
    PRESS <- sum((Y - yPredAve) ^ 2)
    R2 <- 1 - (RSS / TSS)
    Q2 <- 1 - (PRESS / TSS)
    modelReturn$fitMetric <- data.frame(R2 = R2, Q2 = Q2)
  }


  # Stop timer
  end.time <- proc.time()[3]

  modelReturn$calcMins <- (end.time - start.time) / 60

  cat('\n Elapsed time', (end.time - start.time) / 60, 'mins \n')

  class(modelReturn) <- c('rdCVObject',
                          method,
                          ifelse(DA, 'Classification',
                                 ifelse(ML,
                                        'Multilevel',
                                        'Regression')))
  return(modelReturn)
}
