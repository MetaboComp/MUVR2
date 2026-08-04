#' Plot actual model fitness against a permutation/resampling distribution (ggplot2)
#'
#' `ggplot2` version of [plotPerm()]. Plots a histogram of the null hypothesis
#' (permutation or resampling) distribution, the actual model fitness as a
#' vertical line, and the cumulative p-value of the actual fitness under that
#' distribution (see [pPerm()]).
#'
#' Unlike [plotPerm()], which draws the Student's t curve on a second, hidden
#' axis, the ggplot2 version puts the histogram and every curve on the same
#' density scale, so the curve and the histogram can be read against each other.
#'
#' @param actual Actual model fitness (e.g. Q2, AUROC or number of misclassifications)
#' @param distribution Null hypothesis (permutation) distribution of a similar
#'   metric as `actual`
#' @param xlab Label for x-axis
#' @param ylab Label for y-axis
#' @param side Cumulative p either "greater" or "smaller" than the H0
#'   distribution (defaults to the side of median(H0))
#' @param type One of 't', 'non', "smooth", "rank" or "ecdf"
#' @param xlim Optional x-limits
#' @param ylim Optional y-limits
#' @param breaks Histogram breaks, passed to [graphics::hist()] exactly as in
#'   [plotPerm()], so both flavours bin the distribution identically. Defaults to
#'   "Sturges".
#' @param main Optional plot title
#' @param permutation_visual Mark the "median" or "mean" of the H0 distribution,
#'   or "none" (default)
#' @param curve Whether to add the fitted curve for the chosen `type`
#' @param extend How far beyond the data range to extend the curve, as a
#'   proportion of the data range
#' @param show_p Whether to annotate the p-value
#' @param show_actual_value Whether to annotate the actual value
#' @param multiple_p_shown Optional vector of types whose p-values should all be
#'   shown (e.g. `c("t", "smooth")`)
#' @param round_number Number of significant digits in the annotations
#' @return A `ggplot` object
#' @seealso [plotPerm()] for the base graphics version, [pPerm()] for the p-value
#' @export
#' @examples
#' data("freelive2")
#' actual <- sample(YR2, 1)
#' distribution <- YR2
#' ggplotPerm(actual, distribution)
ggplotPerm <- function(actual,
                       distribution,
                       xlab = NULL,
                       ylab = "Density",
                       side = c("greater", "smaller"),
                       type = "t",
                       xlim = NULL,
                       ylim = NULL,
                       breaks = "Sturges",
                       main = NULL,
                       permutation_visual = "none",
                       curve = TRUE,
                       extend = 0.1,
                       multiple_p_shown = NULL,
                       show_actual_value = TRUE,
                       show_p = TRUE,
                       round_number = 4) {
  validTypes <- c("t", "non", "smooth", "ecdf", "rank")
  if (!permutation_visual %in% c("mean", "median", "none")) {
    stop("`permutation_visual` must be one of 'mean', 'median' or 'none'.")
  }
  if (!is.null(multiple_p_shown) && !all(multiple_p_shown %in% validTypes)) {
    stop("`multiple_p_shown` must contain only: ",
         paste(validTypes, collapse = ", "))
  }
  if (!all(type %in% validTypes)) {
    stop("`type` must be one of: ", paste(validTypes, collapse = ", "))
  }
  if (missing(side) || is.null(side) || length(side) > 1) {
    side <- ifelse(actual < median(distribution), "smaller", "greater")
  }

  types <- if (is.null(multiple_p_shown)) type[1] else multiple_p_shown
  pPerms <- lapply(types, function(ty) {
    pPerm(actual, distribution, side, type = ty, extend = extend)
  })
  names(pPerms) <- types

  ## pPerm returns a character (e.g. "<0.001") when the p-value hits its floor
  pLabels <- vapply(pPerms, function(pp) {
    if (is.numeric(pp$p)) {
      paste0("p = ", signif(pp$p, round_number))
    } else {
      paste0("p ", pp$p)
    }
  }, character(1))
  if (length(types) > 1) {
    pLabels <- paste(types, pLabels)
  }

  ran <- range(c(actual, distribution))
  from <- ran[1] - diff(ran) * extend
  to <- ran[2] + diff(ran) * extend
  if (is.null(xlim)) {
    xlim <- c(from, to)
  }

  ## Bin with hist() itself, and draw the bars it decided on, rather than asking
  ## ggplot2 for a similar-looking histogram: `breaks` is a suggestion that hist()
  ## turns into pretty breakpoints, so the same argument must go through the same
  ## function if the two flavours are to bin the data the same way.
  h <- hist(distribution, breaks = breaks, plot = FALSE)
  hist_df <- data.frame(
    xmin = h$breaks[-length(h$breaks)],
    xmax = h$breaks[-1],
    density = h$density
  )
  p <- ggplot(hist_df) +
    geom_rect(aes(xmin = .data$xmin, xmax = .data$xmax,
                  ymin = 0, ymax = .data$density),
              fill = "grey80",
              colour = "grey40")

  ## Density curves, on the same scale as the histogram
  curves <- list()
  if (isTRUE(curve)) {
    if ("t" %in% types && sd(distribution) > 0) {
      x <- seq(from, to, length.out = 500)
      curves$t <- data.frame(
        x = x,
        y = dt((x - mean(distribution)) / sd(distribution),
               df = length(distribution) - 1) / sd(distribution),
        curve = "t"
      )
    }
    if ("smooth" %in% types) {
      dens <- pPerms[["smooth"]]$dens
      curves$smooth <- data.frame(x = dens$x, y = dens$y, curve = "smooth")
    }
  }
  if (length(curves) > 0) {
    curveDf <- do.call(rbind, curves)
    ## Same colours as the base version: a lone curve is red, whichever type it
    ## is; when several are drawn together, t is darkgreen and smooth is red.
    curveColours <- if (length(curves) == 1) {
      stats::setNames("red", names(curves))
    } else {
      c("t" = "darkgreen", "smooth" = "red")
    }
    p <- p +
      geom_line(data = curveDf,
                aes(x = .data$x, y = .data$y, colour = .data$curve),
                linewidth = 0.9,
                inherit.aes = FALSE) +
      scale_colour_manual(values = curveColours, name = NULL)
    if (length(curves) == 1) {
      p <- p + guides(colour = "none")
    }
  }

  ## Where to hang the annotations: at the top of whatever is tallest
  yMax <- max(c(unlist(lapply(curves, function(d) d$y)), h$density))

  p <- p + geom_vline(xintercept = actual, linewidth = 0.6)

  ## The labels are geom_text, not annotate(): ggplotly() silently drops plot
  ## annotations, so the p-value and the actual value would vanish from the
  ## interactive version. A geom survives as a text trace.
  hj <- if (side == "smaller") -0.1 else 1.1

  if (isTRUE(show_p)) {
    p <- p + geom_text(
      data = data.frame(x = actual, y = yMax * 0.9,
                        label = paste(pLabels, collapse = "\n")),
      aes(x = .data$x, y = .data$y, label = .data$label),
      hjust = hj, inherit.aes = FALSE)
  }
  if (isTRUE(show_actual_value)) {
    p <- p + geom_text(
      data = data.frame(x = actual, y = 0,
                        label = as.character(signif(actual, round_number))),
      aes(x = .data$x, y = .data$y, label = .data$label),
      vjust = -0.5, hjust = hj, inherit.aes = FALSE)
  }
  if (permutation_visual != "none") {
    centre <- if (permutation_visual == "mean") {
      mean(distribution)
    } else {
      median(distribution)
    }
    p <- p +
      geom_vline(xintercept = centre, linetype = 2, colour = "grey30") +
      geom_text(
        data = data.frame(x = centre, y = yMax,
                          label = paste0(permutation_visual, " = ",
                                         signif(centre, round_number))),
        aes(x = .data$x, y = .data$y, label = .data$label),
        vjust = 1, hjust = -0.1, inherit.aes = FALSE)
  }

  ## coord_cartesian rather than scale limits: zooming should not drop the
  ## histogram bars that fall outside the window
  p +
    coord_cartesian(xlim = xlim, ylim = ylim) +
    labs(x = xlab, y = ylab, title = main) +
    theme_muvr()
}
