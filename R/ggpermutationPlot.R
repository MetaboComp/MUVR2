#' Plot permutation analysis (ggplot2)
#'
#' `ggplot2` version of [permutationPlot()]. A convenience wrapper around
#' [ggplotPerm()] that pulls the actual model fitness and the null hypothesis
#' distribution out of a model object and its [H0_test()] result, so you do not
#' have to dig them out yourself.
#'
#' @param MUVRclassObject A 'MUVR' class object
#' @param permutation_result A permutation/resampling result from [H0_test()]
#' @param model 'min' (default), 'mid' or 'max'
#' @param type 't' (default; for Student's t) or 'non' for non-parametric (rank)
#' @param side 'smaller' for actual lower than H0 or 'greater' for actual larger
#'   than H0 (guessed if not specified)
#' @param xlab Optional x label
#' @param xlim Optional x-range
#' @param ylim Optional y-range
#' @param bins Number of histogram bins
#' @param main Optional plot title
#' @param ... Further arguments passed to [ggplotPerm()]
#'
#' @return A `ggplot` object; for AUROC, where there is one test per class, a
#'   named list of `ggplot` objects.
#' @seealso [permutationPlot()] for the base graphics version
#' @export
#' @examples
#' \donttest{
#' data("freelive2")
#' regrModel <- MUVR2(X = XRVIP2,
#'                    Y = YR2,
#'                    nRep = 2,
#'                    nOuter = 4,
#'                    varRatio = 0.6,
#'                    method = "PLS",
#'                    modReturn = TRUE)
#' permutation_result <- H0_test(regrModel, n = 10)
#' ggpermutationPlot(regrModel, permutation_result)
#' }
ggpermutationPlot <- function(MUVRclassObject,
                              permutation_result,
                              model = "min",
                              type = "t",
                              side,
                              xlab = NULL,
                              xlim = NULL,
                              ylim = NULL,
                              bins = 30,
                              main = NULL,
                              ...) {
  assertMUVR(MUVRclassObject, "ggpermutationPlot")
  nModel <- modelIndex(model)
  EN <- isEN(MUVRclassObject)

  metric <- MUVRclassObject$inData$fitness
  if (metric == "RMSEP") {
    metric <- "Q2"
  }
  if (!metric %in% c("Q2", "MISS", "AUROC", "BER")) {
    stop("Unsupported fitness metric: ", metric)
  }
  if (metric == "Q2" && !inherits(MUVRclassObject, c("Regression", "Multilevel"))) {
    stop("Classification and multilevel models must use AUROC, MISS or BER.")
  }

  ## Actual model fitness, and the label that goes with it. Elastic net models
  ## carry one value; PLS/RF models carry one per model size.
  actual <- switch(
    metric,
    Q2 = if (EN) MUVRclassObject$fitMetric$Q2 else MUVRclassObject$fitMetric$Q2[nModel],
    MISS = if (EN) MUVRclassObject$miss else MUVRclassObject$miss[nModel],
    BER = if (EN) MUVRclassObject$ber else MUVRclassObject$ber[nModel],
    AUROC = if (EN) MUVRclassObject$auc else MUVRclassObject$auc[nModel, ]
  )
  if (is.null(xlab)) {
    xlab <- switch(metric,
                   Q2 = "Q2",
                   MISS = "Misclassifications",
                   BER = "Balanced error rate",
                   AUROC = "AUROC")
  }
  modelName <- deparse(substitute(MUVRclassObject))

  if (metric != "AUROC") {
    h0 <- if (EN) {
      as.vector(as.matrix(permutation_result))
    } else {
      permutation_result[, nModel]
    }
    if (missing(side) || is.null(side)) {
      side <- ifelse(actual < median(h0), "smaller", "greater")
    }
    if (is.null(main)) {
      main <- paste("Permutation analysis of", modelName, metric)
    }
    return(ggplotPerm(actual = actual,
                      distribution = h0,
                      type = type,
                      side = side,
                      xlab = xlab,
                      xlim = xlim,
                      ylim = ylim,
                      bins = bins,
                      main = main,
                      ...))
  }

  ## AUROC: one test per class
  nGroup <- if (EN) dim(permutation_result)[2] else dim(permutation_result)[3]
  groups <- if (!is.null(names(actual))) names(actual) else as.character(seq_len(nGroup))

  plots <- lapply(seq_len(nGroup), function(s) {
    h0 <- if (EN) {
      permutation_result[, s]
    } else {
      permutation_result[, nModel, s]
    }
    sideS <- if (missing(side) || is.null(side)) {
      ifelse(actual[s] < median(h0), "smaller", "greater")
    } else {
      side
    }
    ggplotPerm(actual = actual[s],
               distribution = h0,
               type = type,
               side = sideS,
               xlab = xlab,
               xlim = xlim,
               ylim = ylim,
               bins = bins,
               main = if (is.null(main)) {
                 paste("Permutation analysis of", modelName, metric,
                       "- group", groups[s])
               } else {
                 paste(main, "- group", groups[s])
               },
               ...)
  })
  names(plots) <- groups
  plots
}
