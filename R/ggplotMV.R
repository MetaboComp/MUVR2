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

    ## The R2/Q2 inlay is a geom in data coordinates, and it is *centred* on its
    ## anchor rather than left-aligned at the panel edge. plotly ignores hjust and
    ## centres text on its anchor point, so a left-aligned label at the edge of
    ## the panel gets sliced in half by ggplotly(). Centring it, and putting the
    ## anchor a short way in from the edge, renders identically in both.
    xRange <- range(d$overall$Y)
    yRange <- range(c(d$perRep$yPred, d$ylim))
    step <- diff(yRange) * 0.07
    labels <- data.frame(
      Y = xRange[1] + diff(xRange) * 0.18,
      yPred = c(yRange[2] + step, yRange[2]),
      label = c(paste("Model R2 =", signif(d$R2, 3)),
                paste("Model Q2 =", signif(d$Q2, 3))),
      stringsAsFactors = FALSE
    )

    ## `text` is not a ggplot2 aesthetic (it warns and is ignored on a static
    ## plot), but plotly reads it for the hover tooltip -- pass tooltip = "text"
    ## to ggplotly(). It names the sample, since the points alone do not.
    d$overall$tooltip <- paste0("Sample: ", d$overall$label,
                                "\nActual: ", signif(d$overall$Y, 3),
                                "\nPredicted: ", signif(d$overall$yPred, 3))
    d$perRep$tooltip <- paste0("Sample: ", d$perRep$label,
                               "\nRepetition: ", d$perRep$repetition,
                               "\nPredicted: ", signif(d$perRep$yPred, 3))

    p <- ggplot(d$overall, aes(x = .data$Y, y = .data$yPred)) +
      geom_point(data = d$perRep, aes(text = .data$tooltip),
                 colour = "grey", size = 1, shape = 16) +
      geom_point(aes(text = .data$tooltip),
                 colour = "black", size = 1.5, shape = 16) +
      geom_abline(intercept = coef(fit)[1],
                  slope = coef(fit)[2]) +
      geom_text(data = labels,
                aes(label = .data$label),
                hjust = 0.5,
                vjust = 0.5,
                size = 3.5) +
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.14))) +
      labs(x = "Original Y", y = "Predicted Y") +
      theme_muvr()
    return(p)
  }

  if (d$type == "multilevel") {
    d$overall$tooltip <- paste0("Sample: ", d$overall$label,
                                "\nPredicted: ", signif(d$overall$yPred, 3))
    d$perRep$tooltip <- paste0("Sample: ", d$perRep$label,
                               "\nRepetition: ", d$perRep$repetition,
                               "\nPredicted: ", signif(d$perRep$yPred, 3))
    p <- ggplot(d$overall, aes(x = .data$yPred, y = .data$sample)) +
      geom_point(data = d$perRep, aes(text = .data$tooltip),
                 colour = "grey", size = 1, shape = 16) +
      geom_point(aes(text = .data$tooltip),
                 colour = "black", size = 1.5, shape = 16) +
      geom_hline(yintercept = d$nSamp / 2 + 0.5, linetype = 2) +
      geom_vline(xintercept = 0, linetype = 2) +
      scale_y_reverse() +
      labs(x = "Predicted Y", y = "Sample number") +
      theme_muvr()
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

  d$overall$tooltip <- paste0("Sample: ", d$overall$label,
                              "\nClass: ", d$overall$class,
                              "\nProbability: ", signif(d$overall$probability, 3))
  d$perRep$tooltip <- paste0("Sample: ", d$perRep$label,
                             "\nClass: ", d$perRep$class,
                             "\nRepetition: ", d$perRep$repetition,
                             "\nProbability: ", signif(d$perRep$probability, 3))

  p <- ggplot(d$overall, aes(x = .data$x, y = .data$probability,
                             colour = .data$class)) +
    geom_vline(xintercept = seq_len(d$nSamp + 1) - 0.5,
               linetype = 3, colour = "grey") +
    ## Solid dots, fully opaque, as in the base version (pch = 20): the smaller
    ## ones are the individual repetitions, the larger the consensus
    geom_point(data = d$perRep, aes(text = .data$tooltip), size = 0.8, shape = 16) +
    geom_point(aes(text = .data$tooltip), size = 1.6, shape = 16) +
    scale_colour_manual(values = factCols, name = NULL) +
    scale_x_continuous(breaks = seq_len(d$nSamp),
                       labels = as.character(d$sampLabels)) +
    labs(x = NULL, y = "Class prediction probability") +
    theme_muvr() +
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
