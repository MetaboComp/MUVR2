#' Plot variable importance ranking (ggplot2)
#'
#' `ggplot2` version of [plotVIRank()]. Variables are sorted by rank, where lower
#' is better.
#'
#' For PLS and RF models, this is a boxplot of each variable's rank across the
#' model repetitions. As in the base version, the boxes are only coloured when
#' `n` is larger than the model's variable selection: the variables inside the
#' selection are then yellow and those outside it grey, separated by a line. When
#' every variable shown is inside the selection there is nothing to distinguish,
#' and the boxes are left plain.
#'
#' For elastic net models ranks do not exist in the same sense: a variable is
#' either given a non-zero coefficient in a given calibration model or it is not.
#' Those models therefore get a selected/not-selected map instead, as either a
#' heatmap (`maptype = "heatmap"`) or a dot plot (`maptype = "dotplot"`).
#'
#' @param MUVRclassObject An MUVR class object. Elastic net models must be passed
#'   through [getVar()] first.
#' @param n Number of top ranking variables to plot (defaults to those selected
#'   by MUVR2)
#' @param model Which model to choose ('min' (default), 'mid' or 'max')
#' @param cut Optional value to cut length of variable names to `cut` number of
#'   characters
#' @param maptype For elastic net models: "heatmap" (default) or "dotplot"
#' @return A `ggplot` object
#' @seealso [plotVIRank()] for the base graphics version
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
#' ggplotVIRank(regrModel, n = 20)
#' }
ggplotVIRank <- function(MUVRclassObject,
                         n,
                         model = "min",
                         cut,
                         maptype = c("heatmap", "dotplot")) {
  maptype <- match.arg(maptype)
  d <- viRankData(MUVRclassObject,
                  n = n,
                  model = model,
                  cut = cut,
                  ## the heatmap clusters the variables; the dot plot leaves
                  ## them in selection-ratio order
                  cluster = maptype == "heatmap")

  if (d$type == "selection") {
    xlab <- paste(d$n, "variables ordered by selection ratio")
    ylab <- paste(d$nModels, "calibration set models")

    if (maptype == "heatmap") {
      p <- ggplot(d$long, aes(x = .data$variable,
                              y = .data$model,
                              fill = .data$selected)) +
        geom_tile() +
        scale_fill_manual(values = c("Selected" = "red",
                                     "Not selected" = "white"),
                          name = "Variable")
    } else {
      p <- ggplot(d$long, aes(x = .data$variable,
                              y = .data$model,
                              colour = .data$selected)) +
        geom_point(shape = 1) +
        scale_colour_manual(values = c("Selected" = "red",
                                       "Not selected" = "grey90"),
                            name = "Variable")
    }

    return(p +
             labs(x = xlab, y = ylab) +
             theme_muvr() +
             theme(axis.text.x = element_text(angle = 90, vjust = 0.5,
                                              hjust = 1, size = 7)))
  }

  ## PLS / RF: boxplot of ranks per variable.
  ##
  ## Built with the variable on x and flipped, rather than with the variable on
  ## y: plotly renders a boxplot whose grouping variable is on y as a set of bare
  ## vertical lines, and coord_flip() is the way to get a horizontal boxplot that
  ## survives ggplotly().
  showSelection <- d$n > d$nFeat

  if (showSelection) {
    p <- ggplot(d$long, aes(x = .data$variable,
                            y = .data$rank,
                            fill = .data$selected)) +
      geom_boxplot(outlier.size = 0.6) +
      scale_fill_manual(values = c("TRUE" = "yellow", "FALSE" = "grey"),
                        breaks = c("TRUE", "FALSE"),
                        labels = c("TRUE" = "Selected", "FALSE" = "Not selected"),
                        name = "Variable") +
      ## The line separating the variables inside the selection from those outside
      geom_vline(xintercept = d$n - d$nFeat + 0.5)
  } else {
    ## Every variable shown is inside the selection: nothing to colour-code, so
    ## leave the boxes plain, as the base version does.
    p <- ggplot(d$long, aes(x = .data$variable, y = .data$rank)) +
      geom_boxplot(outlier.size = 0.6)
  }

  p +
    coord_flip() +
    labs(x = NULL, y = "Variable importance rank (lower is better)") +
    theme_muvr()
}
