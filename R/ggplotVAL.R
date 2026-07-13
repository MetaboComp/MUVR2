#' Plot validation metric (ggplot2)
#'
#' `ggplot2` version of [plotVAL()]. Plots the validation metric against the
#' number of variables in the model, and marks the 'min', 'mid' and 'max'
#' variable selections with the same coloured, dashed lines the base version
#' uses.
#'
#' The plot differs by core method. For PLS and RF, one line is drawn per inner
#' validation segment, per repetition, plus the overall mean. For elastic net,
#' each point is one calibration model and the curve is the fitness curve fitted
#' by [getVar()] (or, when [getVar()] selected variables by quantile, a histogram
#' of the number of variables selected).
#'
#' @param MUVRclassObject An object of class `MUVR`. Elastic net models must be
#'   passed through [getVar()] first.
#' @param show_outlier Boolean, show the outliers that were excluded from the
#'   fitness curve (elastic net only)
#' @return A `ggplot` object
#' @seealso [plotVAL()] for the base graphics version
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
#' ggplotVAL(regrModel)
#' }
ggplotVAL <- function(MUVRclassObject,
                      show_outlier = TRUE) {
  d <- valData(MUVRclassObject)

  ## The min/mid/max cut-offs, drawn as the base version draws them: red, green
  ## and blue, solid, dashed and dotted.
  cutNames <- names(d$nVar)
  cutoffs <- data.frame(
    nVar = as.numeric(d$nVar),
    model = factor(cutNames, levels = cutNames)
  )
  cutColours <- cutoffColours(cutNames)
  cutTypes <- cutoffLinetypes(cutNames)

  if (d$type == "quantile") {
    p <- ggplot(data.frame(nVar = as.numeric(d$nonZeroRep)),
                aes(x = .data$nVar)) +
      geom_histogram(bins = min(30, d$breaks),
                     fill = "grey80", colour = "grey40") +
      geom_vline(data = cutoffs,
                 aes(xintercept = .data$nVar,
                     colour = .data$model,
                     linetype = .data$model),
                 linewidth = 0.8) +
      scale_colour_manual(values = cutColours, name = "Selection") +
      scale_linetype_manual(values = cutTypes, name = "Selection") +
      labs(x = "Number of variables selected across nOuter*nRep loops",
           y = "Count") +
      theme_muvr()
    return(p)
  }

  if (d$type == "fitness") {
    points <- if (isTRUE(show_outlier)) d$points else d$points[!d$points$outlier, ]

    p <- ggplot(points, aes(x = .data$nVar, y = .data$fitness)) +
      geom_point(aes(shape = .data$outlier, alpha = .data$outlier),
                 colour = "black") +
      geom_line(data = d$curve, aes(x = .data$nVar, y = .data$fitness),
                colour = "black", linewidth = 0.8) +
      geom_vline(data = cutoffs,
                 aes(xintercept = .data$nVar,
                     colour = .data$model,
                     linetype = .data$model),
                 linewidth = 0.8) +
      scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 4),
                         name = "Outlier",
                         labels = c("FALSE" = "No", "TRUE" = "Yes")) +
      scale_alpha_manual(values = c("FALSE" = 1, "TRUE" = 0.4),
                         name = "Outlier",
                         labels = c("FALSE" = "No", "TRUE" = "Yes")) +
      scale_colour_manual(values = cutColours, name = "Selection") +
      scale_linetype_manual(values = cutTypes, name = "Selection") +
      labs(x = "Number of variables selected across nOuter*nRep loops",
           y = d$metric) +
      theme_muvr()

    if (!any(d$points$outlier)) {
      p <- p + guides(shape = "none", alpha = "none")
    }
    return(p)
  }

  ## PLS / RF. The validation curves and the cut-off lines share one colour
  ## scale, so that both can keep their own legend entries.
  seriesColours <- c("Validation segments" = "lightgrey",
                     "Repetitions" = "darkgrey",
                     "Overall" = "black")

  ggplot(d$segments, aes(x = .data$count, y = .data$value)) +
    geom_line(aes(group = .data$series, colour = "Validation segments"),
              linewidth = 0.3) +
    geom_line(data = d$repMeans,
              aes(group = .data$repetition, colour = "Repetitions"),
              linewidth = 0.5) +
    geom_line(data = d$overall,
              aes(colour = "Overall"),
              linewidth = 0.9) +
    geom_vline(data = cutoffs,
               aes(xintercept = .data$nVar,
                   colour = .data$model,
                   linetype = .data$model),
               linewidth = 0.8) +
    scale_colour_manual(
      values = c(seriesColours, cutColours),
      breaks = c(names(seriesColours), cutNames),
      name = NULL
    ) +
    scale_linetype_manual(values = cutTypes, name = "Selection") +
    scale_x_log10() +
    labs(x = "Number of variables (log scale)", y = d$metric) +
    theme_muvr()
}
