#' Plot predictions (ggplot2)
#'
#' `ggplot2` version of [plotMV()]. Plots predicted and actual target variables,
#' with a different plot depending on the modelling approach: predicted vs actual
#' for regression, a per-sample "swimlane" of class probabilities for
#' classification, and predicted values per sample for multilevel analysis.
#'
#' Grey points are the predictions of the individual repetitions; black (or
#' coloured, for classification) points are the consensus predictions. In the
#' classification plot, misclassified samples are ringed at the probability of
#' their true class.
#'
#' @param MUVRclassObject An MUVR class object
#' @param model What type of model to plot ('min', 'mid' or 'max'). Defaults to 'min'.
#' @param factCols An optional vector with colors for the factor levels (in the same order as the levels)
#' @param sampLabels Sample labels (optional; implemented for classification)
#' @param ylim Optional for imposing y-limits for regression and classification analysis
#' @return A `ggplot` object
#' @seealso [plotMV()] for the base graphics version
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
#' ggplotMV(regrModel, model = "min")
#' }
ggplotMV <- function(MUVRclassObject,
                     model = "min",
                     factCols,
                     sampLabels,
                     ylim = NULL) {
  d <- mvData(MUVRclassObject,
              model = model,
              sampLabels = sampLabels,
              ylim = ylim)

  if (d$type == "regression") {
    fit <- lm(d$overall$yPred ~ d$overall$Y)
    label <- paste0("Model R2 = ", signif(d$R2, 3), "\n",
                    "Model Q2 = ", signif(d$Q2, 3))

    p <- ggplot(d$overall, aes(x = .data$Y, y = .data$yPred)) +
      geom_point(data = d$perRep, colour = "grey", size = 1) +
      geom_point(colour = "black", size = 1.5) +
      geom_abline(intercept = coef(fit)[1],
                  slope = coef(fit)[2]) +
      annotate("text",
               x = min(d$overall$Y),
               y = max(d$ylim),
               hjust = 0,
               vjust = 1,
               label = label) +
      labs(x = "Original Y", y = "Predicted Y") +
      theme_bw()
    return(p)
  }

  if (d$type == "multilevel") {
    p <- ggplot(d$overall, aes(x = .data$yPred, y = .data$sample)) +
      geom_point(data = d$perRep, colour = "grey", size = 1) +
      geom_point(colour = "black", size = 1.5) +
      geom_hline(yintercept = d$nSamp / 2 + 0.5, linetype = 2) +
      geom_vline(xintercept = 0, linetype = 2) +
      scale_y_reverse() +
      labs(x = "Predicted Y", y = "Sample number") +
      theme_bw()
    return(p)
  }

  ## Classification swimlane
  Y <- d$Y
  classes <- levels(Y)
  if (missing(factCols) || is.null(factCols)) {
    ## Same default colours as the base version: palette entries 2, 3, ...
    factCols <- palette()[seq_along(classes) + 1]
  }
  if (length(factCols) != length(classes)) {
    stop("Length of factCols not equal to number of levels in Y.")
  }
  names(factCols) <- classes

  p <- ggplot(d$overall, aes(x = .data$x, y = .data$probability,
                             colour = .data$class)) +
    geom_vline(xintercept = seq_len(d$nSamp + 1) - 0.5,
               linetype = 3, colour = "grey") +
    geom_point(data = d$perRep, size = 0.8, alpha = 0.5) +
    geom_point(size = 1.6) +
    scale_colour_manual(values = factCols, name = NULL) +
    scale_x_continuous(breaks = seq_len(d$nSamp),
                       labels = as.character(d$sampLabels)) +
    labs(x = NULL, y = "Class prediction probability") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          legend.position = "top")

  if (nrow(d$wrong) > 0) {
    p <- p +
      geom_point(data = d$wrong,
                 aes(x = .data$x, y = .data$probability),
                 shape = 1, size = 3.5, colour = "black",
                 inherit.aes = FALSE)
  }

  if (!is.null(ylim)) {
    p <- p + scale_y_continuous(limits = ylim)
  }

  p
}
