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
#' One deliberate difference from [plotVIRank()]: in the dot plot, variables that
#' were *not* selected are drawn as faint grey dots. The base version draws them
#' in white, i.e. not at all, which leaves the reader unable to tell an empty cell
#' from a missing one. Showing them makes the grid legible as a grid.
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
  ## The boxes are drawn from statistics computed by boxplot.stats(), i.e. with
  ## Tukey's hinges, rather than by geom_boxplot's own type-7 quantiles. With a
  ## handful of repetitions the two disagree: type-7 gives a narrower box, a
  ## tighter 1.5*IQR fence, and so whiskers cut short and points flagged as
  ## outliers. Same data, different picture. This way both flavours of the plot
  ## show the same box.
  ##
  ## The box is drawn from primitives -- a rectangle for the box, and segments for
  ## the median, the whiskers and their end caps -- rather than with geom_boxplot.
  ## Two reasons: ggplotly() turns a stat = "identity" geom_boxplot into an empty
  ## plotly box trace, so the interactive version came out blank; and geom_boxplot
  ## draws no whisker caps, which the base boxplot() does. Built from rects and
  ## segments, the plot survives ggplotly() and gets the caps for free.
  ##
  ## Variable on a continuous x (its factor position) and flipped, so the box
  ## width -- and hence the gap between boxes -- is ours to set, and so the axis
  ## shows the variable names.
  showSelection <- d$n > d$nFeat
  stats <- d$stats
  stats$pos <- as.integer(stats$variable)

  hw <- 0.3   # box half-width; the 0.4 gap left on either side spaces the boxes
  cw <- 0.15  # whisker-cap (staple) half-width

  segs <- rbind(
    ## median
    data.frame(x = stats$pos - hw, xend = stats$pos + hw,
               y = stats$middle, yend = stats$middle),
    ## whiskers
    data.frame(x = stats$pos, xend = stats$pos, y = stats$ymin, yend = stats$lower),
    data.frame(x = stats$pos, xend = stats$pos, y = stats$upper, yend = stats$ymax),
    ## caps
    data.frame(x = stats$pos - cw, xend = stats$pos + cw, y = stats$ymin, yend = stats$ymin),
    data.frame(x = stats$pos - cw, xend = stats$pos + cw, y = stats$ymax, yend = stats$ymax)
  )

  box <- geom_rect(data = stats,
                   aes(xmin = .data$pos - hw, xmax = .data$pos + hw,
                       ymin = .data$lower, ymax = .data$upper,
                       fill = .data$selected),
                   colour = "black")

  if (showSelection) {
    p <- ggplot() +
      box +
      scale_fill_manual(values = c("TRUE" = "yellow", "FALSE" = "grey"),
                        breaks = c("TRUE", "FALSE"),
                        labels = c("TRUE" = "Selected", "FALSE" = "Not selected"),
                        name = "Variable") +
      ## The line separating the variables inside the selection from those outside
      geom_vline(xintercept = d$n - d$nFeat + 0.5)
  } else {
    ## Every variable shown is inside the selection: nothing to colour-code, so
    ## leave the boxes white, as the base version does.
    p <- ggplot() +
      geom_rect(data = stats,
                aes(xmin = .data$pos - hw, xmax = .data$pos + hw,
                    ymin = .data$lower, ymax = .data$upper),
                fill = "white", colour = "black")
  }

  p <- p +
    geom_segment(data = segs,
                 aes(x = .data$x, xend = .data$xend,
                     y = .data$y, yend = .data$yend))

  if (nrow(d$outliers) > 0) {
    outliers <- d$outliers
    outliers$pos <- as.integer(outliers$variable)
    p <- p + geom_point(data = outliers,
                        aes(x = .data$pos, y = .data$rank),
                        size = 0.8)
  }

  p +
    scale_x_continuous(breaks = stats$pos,
                       labels = as.character(stats$variable),
                       expand = expansion(add = 0.6)) +
    coord_flip() +
    labs(x = NULL, y = "Variable importance rank (lower is better)") +
    theme_muvr()
}
