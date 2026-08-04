#' Plot stability (ggplot2)
#'
#' `ggplot2` version of [plotStability()]. Plots the stability of the selected
#' variables and of the prediction fitness as a function of the number of
#' repetitions, as a faceted plot with one panel per metric.
#'
#' Each panel shows two series: the light line is the value obtained in each
#' repetition on its own, and the dark line is the cumulative value over
#' repetitions 1 to i. When the cumulative line has flattened out, `nRep` was
#' high enough.
#'
#' As in the base version, each panel keeps the y-axis it deserves rather than
#' one fitted to its data: proportions and balanced error rates run 0 to 1,
#' misclassifications run from 0 to the number of samples. A free y-axis would
#' zoom into the noise and make a converged model look unstable.
#'
#' @param MUVRrdCVclassObject MUVR class object or rdCV object. Elastic net
#'   models must be passed through [getVar()] first.
#' @param model 'min' (default), 'mid' or 'max'
#' @param VAll Option of specifying which variables (i.e. names) to consider as
#'   reference set. Defaults to the variables selected by `model`.
#' @param nVarLim Option of specifying upper limit for number of variables
#' @param missLim Option of specifying upper limit for number of misclassifications
#' @return A `ggplot` object
#' @seealso [plotStability()] for the base graphics version
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
#' ggplotStability(regrModel, model = "min")
#' }
ggplotStability <- function(MUVRrdCVclassObject,
                            model = "min",
                            VAll,
                            nVarLim,
                            missLim) {
  d <- stabilityData(MUVRrdCVclassObject,
                     model = model,
                     VAll = VAll,
                     nVarLim = nVarLim,
                     missLim = missLim)

  ## One colour pair per metric, matching the base version panel for panel.
  panelColours <- c(
    "Number of selected variables"     = "grey",
    "Proportion of selected variables" = "pink",
    "Number of misclassifications"     = "lightblue",
    "Balanced error rate"              = "lightblue",
    "Q2"                               = "lightgreen"
  )
  cumulativeColours <- c(
    "Number of selected variables"     = "black",
    "Proportion of selected variables" = "red",
    "Number of misclassifications"     = "blue",
    "Balanced error rate"              = "blue",
    "Q2"                               = "darkgreen"
  )

  long <- d$long
  long$key <- paste(long$metric, long$series, sep = " | ")

  values <- c(
    stats::setNames(panelColours[d$panels],
                    paste(d$panels, "Per repetition", sep = " | ")),
    stats::setNames(cumulativeColours[d$panels],
                    paste(d$panels, "Cumulative", sep = " | "))
  )

  ## Each panel has its own colour pair, so a legend cannot show them all without
  ## becoming a wall of ten entries. It instead shows the two *series*, keyed to
  ## the first panel's colours, and the panels vary the hue exactly as the base
  ## plot's per-panel legends do. Light is always per repetition; dark is always
  ## cumulative.
  firstPanel <- d$panels[1]
  legendBreaks <- paste(firstPanel, c("Per repetition", "Cumulative"),
                        sep = " | ")

  ggplot(long, aes(x = .data$repetition,
                   y = .data$value,
                   colour = .data$key)) +
    geom_line() +
    ## Anchors the y-axis of each panel; draws nothing itself
    geom_blank(data = d$limits,
               aes(x = .data$repetition, y = .data$value),
               inherit.aes = FALSE) +
    facet_wrap(~ .data$metric, ncol = 1, scales = "free_y",
               strip.position = "left") +
    scale_colour_manual(values = values,
                        breaks = legendBreaks,
                        labels = c("Per repetition", "Cumulative"),
                        name = NULL) +
    labs(x = "Number of repetitions", y = NULL) +
    theme_muvr() +
    theme(strip.background = element_blank(),
          strip.placement = "outside",
          legend.position = "top")
}
