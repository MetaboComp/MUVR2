#' PLS biplot (ggplot2)
#'
#' `ggplot2` version of [biplotPLS()]. Plots observation scores and variable
#' loadings of a PLS fit in one set of axes: scores on the primary (bottom/left)
#' axes, loadings on the secondary (top/right) axes.
#'
#' @param fit A PLS fit (e.g. from `MUVRclassObject$Fit[[2]]`)
#' @param comps Which two components to plot
#' @param xCol (Optional) Continuous vector for a colour gradient over the
#'   observations (e.g. the Y vector in a regression analysis)
#' @param labPlSc Boolean to plot observation (sample) names (defaults to TRUE)
#' @param labs (Optional) Label names for the observations
#' @param vars Which variables to plot (names in `rownames(loadings)`)
#' @param labPlLo Boolean to plot variable names (defaults to TRUE)
#' @param colSc Colour for observation scores (only used if `xCol` is omitted)
#' @param colLo Colour for variable loadings (defaults to red)
#' @param supLeg Boolean for whether to suppress the legend
#' @return A `ggplot` object
#' @seealso [biplotPLS()] for the base graphics version
#' @export
#' @examples
#' \donttest{
#' data("freelive2")
#' regrModel <- MUVR2(X = XRVIP2,
#'                    Y = YR2,
#'                    nRep = 2,
#'                    nOuter = 3,
#'                    method = "PLS",
#'                    modReturn = TRUE)
#' ggbiplotPLS(regrModel$Fit[[2]],
#'             comps = 1:2,
#'             xCol = YR2,
#'             labPlSc = FALSE,
#'             labPlLo = FALSE)
#' }
ggbiplotPLS <- function(fit,
                        comps = 1:2,
                        xCol,
                        labPlSc = TRUE,
                        labs,
                        vars,
                        labPlLo = TRUE,
                        colSc = "black",
                        colLo = "red",
                        supLeg = FALSE) {
  d <- biplotData(fit, comps = comps, vars = vars)
  scores <- d$scores
  loads <- d$loads

  ## Loadings are put on the scores' scale so both fit in one panel; the
  ## secondary axes then read them back in their own units.
  loads$xScaled <- loads$x * d$scaleFactor
  loads$yScaled <- loads$y * d$scaleFactor

  hasCol <- !missing(xCol) && !is.null(xCol)
  if (hasCol) {
    scores$xCol <- xCol
  }
  if (!missing(labs) && !is.null(labs)) {
    scores$label <- as.character(labs)
  }

  p <- ggplot() +
    geom_hline(yintercept = 0, linetype = 2, colour = "grey70") +
    geom_vline(xintercept = 0, linetype = 2, colour = "grey70") +
    geom_segment(
      data = loads,
      aes(x = 0, y = 0, xend = .data$xScaled, yend = .data$yScaled),
      colour = colLo,
      arrow = grid::arrow(length = grid::unit(0.15, "cm"))
    )

  if (hasCol) {
    p <- p +
      geom_point(data = scores,
                 aes(x = .data$x, y = .data$y, colour = .data$xCol),
                 size = 2) +
      scale_colour_gradient(low = "grey85", high = "grey10")
  } else {
    p <- p +
      geom_point(data = scores,
                 aes(x = .data$x, y = .data$y),
                 colour = colSc,
                 size = 2)
  }

  if (isTRUE(labPlLo)) {
    p <- p + geom_text(data = loads,
                       aes(x = .data$xScaled * 1.1,
                           y = .data$yScaled * 1.1,
                           label = .data$label),
                       colour = colLo,
                       fontface = "italic",
                       size = 2.5)
  }
  if (isTRUE(labPlSc)) {
    p <- p + geom_text(data = scores,
                       aes(x = .data$x, y = .data$y, label = .data$label),
                       vjust = -0.7,
                       size = 2.5)
  }

  p <- p +
    scale_x_continuous(
      name = paste("Component", d$comps[1], "Scores"),
      limits = c(-d$rSc, d$rSc),
      sec.axis = sec_axis(~ . / d$scaleFactor, name = "Loadings")
    ) +
    scale_y_continuous(
      name = paste("Component", d$comps[2], "Scores"),
      limits = c(-d$rSc, d$rSc),
      sec.axis = sec_axis(~ . / d$scaleFactor, name = "Loadings")
    ) +
    theme_bw()

  if (isTRUE(supLeg)) {
    p <- p + theme(legend.position = "none")
  } else if (hasCol) {
    ## ggplot2::labs() qualified: the `labs` argument of this function shadows it
    p <- p + ggplot2::labs(colour = NULL)
  }

  p
}
