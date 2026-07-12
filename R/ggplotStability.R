#' Plot stability (ggplot2)
#'
#' `ggplot2` version of [plotStability()]. Plots the stability of the selected
#' variables and of the prediction fitness as a function of the number of
#' repetitions, as a faceted plot with one panel per metric.
#'
#' Each panel shows two series: the value obtained in each repetition on its own,
#' and the cumulative value over repetitions 1 to i. When the cumulative series
#' has flattened out, `nRep` was high enough.
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

  ggplot(d$long, aes(x = .data$repetition,
                     y = .data$value,
                     colour = .data$series)) +
    geom_line() +
    facet_wrap(~ .data$metric, ncol = 1, scales = "free_y",
               strip.position = "left") +
    scale_colour_manual(values = c("Per repetition" = "grey60",
                                   "Cumulative" = "black"),
                        name = NULL) +
    labs(x = "Number of repetitions", y = NULL) +
    theme_bw() +
    theme(strip.background = element_blank(),
          strip.placement = "outside",
          legend.position = "top")
}
