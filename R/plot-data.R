# Internal data preparation for the plotting functions.
#
# Every plot in MUVR2 exists twice: a base graphics version (plotMV, plotVAL,
# ...) and a ggplot2 version (ggplotMV, ggplotVAL, ...). The extraction and
# computation that sits behind them lives here once, so the two flavours cannot
# drift apart, and so that the fiddly parts (per-repetition stability metrics,
# the fitness curve of an elastic net model, the selection matrix of an EN
# model) are testable without a graphics device.
#
# These functions are internal and their return values are not part of the
# package API.


#' Translate a 'min'/'mid'/'max' model choice into its column index
#'
#' @param model 'min', 'mid' or 'max' (case-insensitive)
#' @return 1, 2 or 3
#' @keywords internal
#' @noRd
modelIndex <- function(model = "min") {
  if (length(model) != 1) {
    stop("`model` must be one of 'min', 'mid' or 'max'.")
  }
  index <- switch(tolower(as.character(model)),
                  min = 1L,
                  mid = 2L,
                  max = 3L,
                  stop("`model` must be one of 'min', 'mid' or 'max'."))
  index
}


#' Is this an elastic net (rdCVnet) MUVR object?
#'
#' Replaces the `class(object)[3] == "rdCVnet"` idiom, which breaks as soon as
#' anything else touches the class vector (e.g. mergeModels()).
#'
#' @param object any object
#' @return TRUE for elastic net MUVR objects
#' @keywords internal
#' @noRd
isEN <- function(object) {
  inherits(object, "rdCVnet")
}


#' Check that an object is a MUVR object
#'
#' @param object any object
#' @param what name of the calling function, used in the error message
#' @return invisible TRUE, or an error
#' @keywords internal
#' @noRd
assertMUVR <- function(object, what = "This function") {
  if (!inherits(object, "MUVR")) {
    stop(what, " requires an object of class 'MUVR'.")
  }
  invisible(TRUE)
}


#' Extract predicted vs actual Y from a MUVR object
#'
#' Data behind [plotMV()] and [ggplotMV()].
#'
#' @param MUVRclassObject an MUVR object
#' @param model 'min', 'mid' or 'max'
#' @param sampLabels sample labels (classification only)
#' @param ylim optional y-limits
#'
#' @return A list with `type` ("regression", "classification" or "multilevel"),
#'   the raw pieces used by the base plot (`Y`, `YP`, `YPR`, `ylim`) and tidy
#'   data frames used by the ggplot2 version (`overall`, `perRep` and, for
#'   classification, `wrong`).
#' @keywords internal
#' @noRd
mvData <- function(MUVRclassObject,
                   model = "min",
                   sampLabels,
                   ylim = NULL) {
  assertMUVR(MUVRclassObject, "plotMV")
  modNum <- modelIndex(model)

  Y <- MUVRclassObject$inData$Y
  nSamp <- length(Y)

  if (missing(sampLabels) || is.null(sampLabels)) {
    sampLabels <- Y
  }
  if (length(sampLabels) != nSamp) {
    stop("Length of sampLabels not equal to number of samples in Y.")
  }

  ## PLS/RF models keep one prediction object per model size; elastic net models
  ## have a single one, since the min/mid/max split happens after fitting.
  perRepIsList <- is.list(MUVRclassObject$yPredPerRep)
  if (perRepIsList) {
    YPR <- MUVRclassObject$yPredPerRep[[modNum]]
    YP <- if (is.list(MUVRclassObject$yPred)) {
      MUVRclassObject$yPred[[modNum]]
    } else {
      MUVRclassObject$yPred[, modNum]
    }
  } else {
    YPR <- MUVRclassObject$yPredPerRep
    YP <- MUVRclassObject$yPred
  }

  if (is.null(ylim)) {
    ylim <- range(YPR)
  }

  type <- if (inherits(MUVRclassObject, "Regression")) {
    "regression"
  } else if (inherits(MUVRclassObject, "Classification")) {
    "classification"
  } else if (inherits(MUVRclassObject, "Multilevel")) {
    "multilevel"
  } else {
    stop("Unknown MUVR model type.")
  }

  out <- list(type = type,
              modNum = modNum,
              Y = Y,
              YP = YP,
              YPR = YPR,
              nSamp = nSamp,
              sampLabels = sampLabels,
              ylim = ylim)

  if (type == "regression") {
    out$overall <- data.frame(Y = as.numeric(Y),
                              yPred = as.numeric(YP),
                              sample = seq_len(nSamp),
                              stringsAsFactors = FALSE)
    nRep <- ncol(as.matrix(YPR))
    out$perRep <- data.frame(
      Y = rep(as.numeric(Y), times = nRep),
      yPred = as.numeric(as.matrix(YPR)),
      repetition = factor(rep(seq_len(nRep), each = nSamp)),
      stringsAsFactors = FALSE
    )
    out$R2 <- MUVRclassObject$fitMetric$R2[modNum]
    out$Q2 <- MUVRclassObject$fitMetric$Q2[modNum]

  } else if (type == "multilevel") {
    out$overall <- data.frame(sample = seq_len(nSamp),
                              yPred = as.numeric(YP),
                              stringsAsFactors = FALSE)
    nRep <- ncol(as.matrix(YPR))
    out$perRep <- data.frame(
      sample = rep(seq_len(nSamp), times = nRep),
      yPred = as.numeric(as.matrix(YPR)),
      repetition = factor(rep(seq_len(nRep), each = nSamp)),
      stringsAsFactors = FALSE
    )

  } else {
    ## Classification: YP is nSamp x nClass, YPR is nSamp x nClass x nRep.
    classes <- seq_along(levels(Y))
    nClass <- length(classes)
    ## Nudge the classes apart so the per-sample "swimlane" is readable.
    classNudge <- 0.2 * ((classes - mean(classes)) / (mean(classes) - 1))

    yClass <- MUVRclassObject$yClass
    if (is.data.frame(yClass) || is.matrix(yClass)) {
      yClass <- yClass[, modNum]
    }

    out$classes <- classes
    out$classNudge <- classNudge
    out$yClass <- yClass

    out$overall <- data.frame(
      sample = rep(seq_len(nSamp), times = nClass),
      x = rep(seq_len(nSamp), times = nClass) +
        rep(classNudge, each = nSamp),
      class = factor(rep(levels(Y), each = nSamp), levels = levels(Y)),
      probability = as.numeric(YP),
      stringsAsFactors = FALSE
    )

    nRep <- dim(YPR)[3]
    out$perRep <- data.frame(
      sample = rep(seq_len(nSamp), times = nClass * nRep),
      x = rep(seq_len(nSamp), times = nClass * nRep) +
        rep(rep(classNudge, each = nSamp), times = nRep),
      class = factor(rep(rep(levels(Y), each = nSamp), times = nRep),
                     levels = levels(Y)),
      repetition = factor(rep(seq_len(nRep), each = nSamp * nClass)),
      probability = as.numeric(YPR),
      stringsAsFactors = FALSE
    )

    ## Misclassified samples get ringed at the probability of their *true* class.
    whichWrong <- which(as.character(yClass) != as.character(Y))
    if (length(whichWrong) > 0) {
      wrongClass <- as.numeric(Y[whichWrong])
      out$wrong <- data.frame(
        sample = whichWrong,
        x = whichWrong + classNudge[wrongClass],
        class = factor(levels(Y)[wrongClass], levels = levels(Y)),
        probability = YP[cbind(whichWrong, wrongClass)],
        stringsAsFactors = FALSE
      )
    } else {
      out$wrong <- data.frame(sample = numeric(0), x = numeric(0),
                              class = factor(character(0), levels = levels(Y)),
                              probability = numeric(0))
    }
  }

  out
}


#' Extract validation-metric-vs-number-of-variables data from a MUVR object
#'
#' Data behind [plotVAL()] and [ggplotVAL()].
#'
#' @param MUVRclassObject an MUVR object. Elastic net objects must have been
#'   passed through [getVar()] first.
#'
#' @return A list whose `type` is one of "rdCV" (PLS/RF), "fitness" (elastic net
#'   with variable selection by fitness curve) or "quantile" (elastic net with
#'   selection by quantile), plus the pieces each of those needs.
#' @keywords internal
#' @noRd
valData <- function(MUVRclassObject) {
  assertMUVR(MUVRclassObject, "plotVAL")

  metric <- MUVRclassObject$VAL$metric
  nVar <- MUVRclassObject$nVar

  ## Elastic net: variable selection happened in getVar(), not during fitting.
  if (!is.null(MUVRclassObject$varTable)) {
    if (is.null(nVar) || !names(nVar)[1] %in% c("min", "Qmin")) {
      stop("Elastic net models must be passed through getVar() before plotting ",
           "the validation metric.")
    }

    if (names(nVar)[1] == "Qmin") {
      dist <- as.matrix(MUVRclassObject$nonZeroRep)
      return(list(
        type = "quantile",
        metric = metric,
        nVar = nVar,
        nonZeroRep = dist,
        breaks = nrow(dist) * ncol(dist),
        xlim = range(0, max(MUVRclassObject$nonZeroRep) * 1.1)
      ))
    }

    ## "fitness": refit the same curve getVar() used to pick min/mid/max.
    nonZero <- c(t(MUVRclassObject$nonZeroRep))
    fitness <- c(t(MUVRclassObject$fitnessRep))
    grid <- seq(min(nonZero), max(nonZero), 1)
    outlier_info <- MUVRclassObject$outlier_info
    keep <- outlier_info == "black"
    fit_curve <- MUVRclassObject$fit_curve

    if (fit_curve == "gam") {
      k <- MUVRclassObject$k
      dataframe <- as.data.frame(cbind(nonZeroRep_vector = nonZero[keep],
                                       fitnessRep_vector = fitness[keep]))
      gam_model <- mgcv::gam(
        fitnessRep_vector ~ s(nonZeroRep_vector, bs = "ps", k = k),
        data = dataframe
      )
      predicted <- predict.gam(
        gam_model,
        data.frame(nonZeroRep_vector = grid,
                   fitnessRep_vector = rep(0, length(grid)))
      )
    } else {
      span <- MUVRclassObject$span
      fitnessRep_vector <- fitness[keep]
      nonZeroRep_vector <- nonZero[keep]
      fit_temp <- loess(fitnessRep_vector ~ nonZeroRep_vector,
                        span = span,
                        degree = 2)
      predicted <- predict(
        fit_temp,
        newdata = data.frame(nonZeroRep_vector = grid)
      )
    }

    return(list(
      type = "fitness",
      metric = metric,
      nVar = nVar,
      fit_curve = fit_curve,
      points = data.frame(nVar = nonZero,
                          fitness = fitness,
                          outlier = !keep,
                          colour = outlier_info,
                          stringsAsFactors = FALSE),
      curve = data.frame(nVar = grid,
                         fitness = as.numeric(predicted),
                         stringsAsFactors = FALSE)
    ))
  }

  ## PLS / RF: one validation curve per outer segment per repetition.
  VAL <- MUVRclassObject$VAL$VAL
  count <- as.numeric(colnames(VAL))
  nRep <- dim(VAL)[3]
  nSeg <- dim(VAL)[1]

  segments <- data.frame(
    count = rep(count, each = nSeg * nRep),
    value = as.numeric(aperm(VAL, c(1, 3, 2))),
    segment = rep(seq_len(nSeg), times = length(count) * nRep),
    repetition = rep(rep(seq_len(nRep), each = nSeg), times = length(count)),
    stringsAsFactors = FALSE
  )
  segments$series <- paste(segments$repetition, segments$segment, sep = "-")

  ## Mean over outer segments, one curve per repetition
  repMeanMat <- apply(VAL, c(2, 3), mean)
  repMeans <- data.frame(
    count = rep(count, times = nRep),
    value = as.numeric(repMeanMat),
    repetition = factor(rep(seq_len(nRep), each = length(count))),
    stringsAsFactors = FALSE
  )

  overall <- data.frame(count = count,
                        value = apply(VAL, 2, mean),
                        stringsAsFactors = FALSE)

  list(
    type = "rdCV",
    metric = metric,
    nVar = nVar,
    VAL = VAL,
    count = count,
    nRep = nRep,
    segments = segments,
    repMeans = repMeans,
    overall = overall
  )
}


#' Compute variable-selection and prediction stability across repetitions
#'
#' Data behind [plotStability()] and [ggplotStability()]. For each repetition
#' `i`, everything is computed twice: once for repetition `i` alone ("Per
#' repetition") and once cumulatively over repetitions `1:i` ("Cumulative").
#' The cumulative series flattening out is what tells you `nRep` was high enough.
#'
#' @param MUVRrdCVclassObject an MUVR object
#' @param model 'min', 'mid' or 'max'
#' @param VAll reference variable set (defaults to the selection of `model`)
#' @param nVarLim upper limit for the number of variables
#' @param missLim upper limit for the number of misclassifications
#'
#' @return A list with the per-repetition vectors used by the base plot and a
#'   long `data.frame` (`rep`, `metric`, `series`, `value`) used by the ggplot2
#'   version, plus the axis limits and which panels apply to this model type.
#' @keywords internal
#' @noRd
stabilityData <- function(MUVRrdCVclassObject,
                          model = "min",
                          VAll,
                          nVarLim,
                          missLim) {
  assertMUVR(MUVRrdCVclassObject, "plotStability")
  nModel <- modelIndex(model)

  regr <- inherits(MUVRrdCVclassObject, "Regression")
  DA <- MUVRrdCVclassObject$inData$DA
  ML <- MUVRrdCVclassObject$inData$ML
  Y <- MUVRrdCVclassObject$inData$Y
  EN <- isEN(MUVRrdCVclassObject)

  if (is.null(MUVRrdCVclassObject$nVar)) {
    stop("Elastic net models must be passed through getVar() before plotting ",
         "stability.")
  }

  nVar <- round(MUVRrdCVclassObject$nVar[nModel])
  if (missing(VAll) || is.null(VAll)) {
    VAll <- if (EN) {
      MUVRrdCVclassObject$Var[[nModel]]
    } else {
      names(sort(MUVRrdCVclassObject$VIRank[, nModel])[1:nVar])
    }
  }

  nRep <- MUVRrdCVclassObject$inData$nRep
  nVRep <- VARep <- missRep <- berRep <- q2Rep <-
    nV <- VA <- miss <- ber <- q2 <- numeric(nRep)

  for (i in 1:nRep) {
    if (!EN) {
      nVRep[i] <- MUVRrdCVclassObject$nVarPerRep[[nModel]][i]
      nV[i] <- round(mean(MUVRrdCVclassObject$nVarPerRep[[nModel]][1:i]))
      ## How many of this repetition's selected variables are in the reference set
      VARep[i] <- sum(names(sort(
        MUVRrdCVclassObject$VIRankPerRep[[nModel]][, i]
      )[1:nVRep[i]]) %in% VAll)
      ## Same, but for the consensus ranking over repetitions 1:i
      VA[i] <- sum(names(sort(
        rowMeans(MUVRrdCVclassObject$VIRankPerRep[[nModel]][, 1:i, drop = FALSE])
      )[1:nV[i]]) %in% VAll)
    } else {
      nVRep[i] <- MUVRrdCVclassObject$nVarPerRep[i]
      nV[i] <- round(mean(MUVRrdCVclassObject$nVarPerRep[1:i]))
      nOuter <- MUVRrdCVclassObject$inData$nOuter
      VARep_temp <- unique(unlist(
        MUVRrdCVclassObject$varRep[(i - 1) * nOuter + seq_len(nOuter)]
      ))
      VARep[i] <- sum(VARep_temp %in% VAll)
      VA[i] <- mean(VARep[1:i])
    }

    if (DA) {
      predsRep <- if (EN) {
        MUVRrdCVclassObject$yPredPerRep[, , i, drop = FALSE]
      } else {
        MUVRrdCVclassObject$yPredPerRep[[nModel]][, , i, drop = FALSE]
      }
      berRep[i] <- getBER(predicted = levels(Y)[apply(predsRep, 1, which.max)],
                          actual = Y)
      missRep[i] <- sum(levels(Y)[apply(predsRep, 1, which.max)] != Y)

      preds <- if (EN) {
        MUVRrdCVclassObject$yPredPerRep[, , 1:i]
      } else {
        MUVRrdCVclassObject$yPredPerRep[[nModel]][, , 1:i]
      }
      preds <- apply(preds, c(1, 2), mean)
      ber[i] <- getBER(predicted = levels(Y)[apply(preds, 1, which.max)],
                       actual = Y)
      miss[i] <- sum(levels(Y)[apply(preds, 1, which.max)] != Y)

    } else {
      predsRep <- if (EN) {
        MUVRrdCVclassObject$yPredPerRep[, i, drop = FALSE]
      } else {
        MUVRrdCVclassObject$yPredPerRep[[nModel]][, i, drop = FALSE]
      }
      TSS <- sum((Y - mean(Y))^2)
      q2Rep[i] <- 1 - (sum((Y - predsRep)^2) / TSS)

      preds <- if (EN) {
        MUVRrdCVclassObject$yPredPerRep[, 1:i, drop = FALSE]
      } else {
        MUVRrdCVclassObject$yPredPerRep[[nModel]][, 1:i, drop = FALSE]
      }
      preds <- rowMeans(preds)
      q2[i] <- 1 - (sum((Y - preds)^2) / TSS)
    }

    if (ML) {
      class <- ifelse(preds < 0, -1, 1)
      miss[i] <- sum(class != Y)
      ber[i] <- getBER(predicted = class, actual = Y)
    }
  }

  VARep <- VARep / length(VAll)
  VA <- VA / length(VAll)

  if (missing(nVarLim) || is.null(nVarLim)) {
    pot <- 10^floor(log10(max(nV)))
    nVarLim <- ceiling(max(c(nV, nVRep)) / pot) * pot
  }
  if (missing(missLim) || is.null(missLim)) {
    missLim <- length(Y)
  }

  ## Long form for the faceted ggplot2 version. Panel order matches the base
  ## plot's top-to-bottom ordering.
  panels <- c("Number of selected variables", "Proportion of selected variables")
  if (DA | ML) {
    panels <- c(panels, "Number of misclassifications", "Balanced error rate")
  }
  if (regr | ML) {
    panels <- c(panels, "Q2")
  }

  makeLong <- function(metric, perRep, cumulative) {
    data.frame(
      repetition = rep(seq_len(nRep), 2),
      metric = factor(metric, levels = panels),
      series = factor(rep(c("Per repetition", "Cumulative"), each = nRep),
                      levels = c("Per repetition", "Cumulative")),
      value = c(perRep, cumulative),
      stringsAsFactors = FALSE
    )
  }

  long <- rbind(
    makeLong("Number of selected variables", nVRep, nV),
    makeLong("Proportion of selected variables", VARep, VA)
  )
  if (DA | ML) {
    long <- rbind(long,
                  makeLong("Number of misclassifications", missRep, miss),
                  makeLong("Balanced error rate", berRep, ber))
  }
  if (regr | ML) {
    long <- rbind(long, makeLong("Q2", q2Rep, q2))
  }

  list(
    nRep = nRep,
    regr = regr,
    DA = DA,
    ML = ML,
    VAll = VAll,
    nVRep = nVRep, nV = nV,
    VARep = VARep, VA = VA,
    missRep = missRep, miss = miss,
    berRep = berRep, ber = ber,
    q2Rep = q2Rep, q2 = q2,
    nVarLim = nVarLim,
    missLim = missLim,
    panels = panels,
    long = long
  )
}


#' Extract variable importance rankings from a MUVR object
#'
#' Data behind [plotVIRank()] and [ggplotVIRank()]. PLS and RF models rank every
#' variable in every repetition, so the natural summary is a boxplot of ranks.
#' Elastic net models instead either select a variable in a given calibration
#' model or not, so the natural summary is a selected/not-selected matrix.
#'
#' @param MUVRclassObject an MUVR object
#' @param n number of top variables to include
#' @param model 'min', 'mid' or 'max'
#' @param cut truncate variable names to this many characters
#' @param cluster elastic net only. When TRUE, cluster the variables that vary
#'   between calibration models and order the models by how many variables they
#'   selected (what the heatmap shows). When FALSE, leave variables in
#'   selection-ratio order and models in their original order (what the dot plot
#'   shows).
#'
#' @return For PLS/RF a list of `type` "rank" with the rank matrix and a long
#'   data frame; for elastic net a list of `type` "selection" with the
#'   selected/not-selected matrix and its long form.
#' @keywords internal
#' @noRd
viRankData <- function(MUVRclassObject,
                       n,
                       model = "min",
                       cut,
                       cluster = TRUE) {
  assertMUVR(MUVRclassObject, "plotVIRank")
  nModel <- modelIndex(model)

  if (isEN(MUVRclassObject)) {
    if (is.null(MUVRclassObject$nVar)) {
      stop("Elastic net models must be passed through getVar() before plotting ",
           "variable importance.")
    }
    nFeat <- round(MUVRclassObject$nVar[nModel])
    if (missing(n) || is.null(n)) {
      n <- nFeat
    }

    nModels <- length(MUVRclassObject$varRep)
    ## 1 where a variable had a non-zero coefficient in a given calibration model
    matrix_count <- matrix(0,
                           nrow = length(MUVRclassObject$nonZeroRep),
                           ncol = ncol(MUVRclassObject$inData$X))
    rownames(matrix_count) <- seq_len(nrow(matrix_count))
    colnames(matrix_count) <- names(MUVRclassObject$varTable)

    for (i in seq_len(nModels)) {
      selected <- colnames(matrix_count) %in% MUVRclassObject$varRep[[i]]
      matrix_count[i, selected] <- 1
    }

    if (isTRUE(cluster)) {
      ## Cluster the variables that actually vary, then push the invariant ones
      ## to the front, so the heatmap has structure rather than noise.
      matrix_count_t <- t(matrix_count)
      varies <- apply(matrix_count_t, 2, function(x) length(table(x)) != 1)
      if (any(varies)) {
        clust <- hclust(as.dist(1 - cor(matrix_count_t[, varies, drop = FALSE])))
        ordered <- cbind(matrix_count_t[, !varies, drop = FALSE],
                         matrix_count_t[, varies, drop = FALSE][, clust$order])
      } else {
        ordered <- matrix_count_t
      }
      matrix_count <- t(ordered)

      ## Order calibration models by how many variables they selected
      number_of1 <- apply(matrix_count, 1, function(x) sum(x == 1))
      matrix_count <- matrix_count[order(number_of1), , drop = FALSE]
    }

    n <- min(n, ncol(matrix_count))
    subset <- matrix_count[, 1:n, drop = FALSE]

    long <- data.frame(
      model = rep(seq_len(nrow(subset)), times = ncol(subset)),
      variable = factor(rep(colnames(subset), each = nrow(subset)),
                        levels = colnames(subset)),
      selected = factor(ifelse(as.numeric(subset) == 1,
                               "Selected", "Not selected"),
                        levels = c("Selected", "Not selected")),
      stringsAsFactors = FALSE
    )

    return(list(
      type = "selection",
      n = n,
      nFeat = nFeat,
      matrix_count = matrix_count,
      subset = subset,
      long = long,
      nModels = MUVRclassObject$inData$nRep * MUVRclassObject$inData$nOuter
    ))
  }

  ## PLS / RF
  nFeat <- round(MUVRclassObject$nVar[nModel])
  if (missing(n) || is.null(n)) {
    n <- nFeat
  }
  VIRank <- MUVRclassObject$VIRank[, nModel]
  VIRankRep <- MUVRclassObject$VIRankPerRep[[nModel]]
  n <- min(n, nrow(VIRankRep))
  VIRankRep <- VIRankRep[order(VIRank), , drop = FALSE][1:n, , drop = FALSE]

  labels <- rownames(VIRankRep)
  if (!missing(cut) && !is.null(cut)) {
    labels <- substring(labels, 1, cut)
  }

  long <- data.frame(
    variable = factor(rep(labels, times = ncol(VIRankRep)),
                      levels = rev(labels)),
    repetition = factor(rep(seq_len(ncol(VIRankRep)), each = nrow(VIRankRep))),
    rank = as.numeric(VIRankRep),
    selected = rep(seq_len(nrow(VIRankRep)) <= nFeat, times = ncol(VIRankRep)),
    stringsAsFactors = FALSE
  )

  list(
    type = "rank",
    n = n,
    nFeat = nFeat,
    VIRankRep = VIRankRep,
    labels = labels,
    long = long
  )
}


#' Extract PCA scores for plotting
#'
#' Data behind [plotPCA()] and [ggplotPCA()].
#'
#' @param pca a `prcomp` object
#' @param PC1,PC2 principal components to plot
#' @param colVar continuous colouring variable
#' @param symbVar categorical symbol variable
#'
#' @return A list with a `scores` data frame and the axis labels.
#' @keywords internal
#' @noRd
pcaData <- function(pca, PC1 = 1, PC2 = 2, colVar, symbVar) {
  if (!inherits(pca, "prcomp")) {
    stop("`pca` must be a 'prcomp' object.")
  }
  pcVar <- summary(pca)$importance[2, ]

  labels <- rownames(pca$x)
  if (is.null(labels)) {
    labels <- as.character(seq_len(nrow(pca$x)))
  }
  scores <- data.frame(
    x = pca$x[, PC1],
    y = pca$x[, PC2],
    label = labels,
    stringsAsFactors = FALSE
  )
  if (!missing(colVar) && !is.null(colVar)) {
    scores$colVar <- colVar
  }
  if (!missing(symbVar) && !is.null(symbVar)) {
    scores$symbVar <- factor(symbVar)
  }

  list(
    scores = scores,
    xlab = paste("PC", PC1, " (R2X=", signif(pcVar[PC1], 3), ")", sep = ""),
    ylab = paste("PC", PC2, " (R2X=", signif(pcVar[PC2], 3), ")", sep = "")
  )
}


#' Extract scores and loadings from a PLS fit
#'
#' Data behind [biplotPLS()] and [ggbiplotPLS()]. `scaleFactor` maps the loadings
#' onto the scores' scale, which is how both versions get scores and loadings
#' into one set of axes.
#'
#' @param fit a PLS fit (e.g. `MUVRclassObject$Fit[[2]]`)
#' @param comps which two components to plot
#' @param vars which variables to show (defaults to all)
#'
#' @return A list with `scores`, `loads` and `scaleFactor`.
#' @keywords internal
#' @noRd
biplotData <- function(fit, comps = 1:2, vars) {
  if (is.null(fit$variates$X) || is.null(fit$loadings$X)) {
    stop("`fit` does not look like a PLS fit: no $variates$X / $loadings$X.")
  }
  scores <- fit$variates$X[, comps, drop = FALSE]
  loads <- fit$loadings$X[, comps, drop = FALSE]

  if (missing(vars) || is.null(vars)) {
    vars <- rownames(loads)
  }
  loads <- loads[rownames(loads) %in% vars, , drop = FALSE]

  scoreLabels <- rownames(scores)
  if (is.null(scoreLabels)) {
    scoreLabels <- as.character(seq_len(nrow(scores)))
  }
  scoresDf <- data.frame(x = scores[, 1],
                         y = scores[, 2],
                         label = scoreLabels,
                         stringsAsFactors = FALSE)
  loadsDf <- data.frame(x = loads[, 1],
                        y = loads[, 2],
                        label = rownames(loads),
                        stringsAsFactors = FALSE)

  rSc <- max(abs(scores))
  rLo <- max(abs(loads)) * 1.1
  scaleFactor <- rSc / rLo

  list(scores = scoresDf,
       loads = loadsDf,
       comps = comps,
       rSc = rSc,
       rLo = rLo,
       scaleFactor = scaleFactor)
}


#' Tidy true and predicted Y values
#'
#' Data behind [plotPred()] and [ggplotPred()].
#'
#' @param Ytrue true Y (a vector)
#' @param Ypreds predicted Y (a vector, matrix or data frame)
#'
#' @return A list with a long `perPred` data frame and a `consensus` data frame.
#' @keywords internal
#' @noRd
predData <- function(Ytrue, Ypreds) {
  if (is.list(Ypreds) && !is.data.frame(Ypreds)) {
    stop("Ypreds should be either vector or dataframe or matrix")
  }

  if (!is.data.frame(Ypreds) && !is.matrix(Ypreds)) {
    if (length(Ytrue) != length(Ypreds)) {
      stop("The YTrue and YPreds should have same number of observations.")
    }
    perPred <- data.frame(Ytrue = as.numeric(Ytrue),
                          Ypred = as.numeric(Ypreds),
                          series = factor(1),
                          stringsAsFactors = FALSE)
    consensus <- data.frame(Ytrue = as.numeric(Ytrue),
                            Ypred = as.numeric(Ypreds),
                            stringsAsFactors = FALSE)
  } else {
    if (length(Ytrue) != nrow(Ypreds)) {
      stop("The YTrue and YPreds should have same number of obsevations.")
    }
    Ypreds <- as.matrix(Ypreds)
    nSeries <- ncol(Ypreds)
    perPred <- data.frame(
      Ytrue = rep(as.numeric(Ytrue), times = nSeries),
      Ypred = as.numeric(Ypreds),
      series = factor(rep(seq_len(nSeries), each = nrow(Ypreds))),
      stringsAsFactors = FALSE
    )
    consensus <- data.frame(Ytrue = as.numeric(Ytrue),
                            Ypred = rowMeans(Ypreds),
                            stringsAsFactors = FALSE)
  }

  list(perPred = perPred, consensus = consensus)
}
