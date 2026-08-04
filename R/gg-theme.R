#' Theme shared by all the ggplot2 versions of the MUVR2 plots
#'
#' Deliberately close to the base graphics look the package has always had: a
#' plain white panel with a box around it and no grid lines. Being a normal
#' ggplot2 theme, it can be overridden by adding another theme to the plot.
#'
#' @param base_size base font size
#' @return a ggplot2 theme
#' @keywords internal
#' @noRd
theme_muvr <- function(base_size = 11) {
  theme_bw(base_size = base_size) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}


#' Colours of the min/mid/max variable selection cut-offs
#'
#' The base plots draw these with `col = 2:4` and `lty = 1:3`, i.e. red, green
#' and blue in solid, dashed and dotted. Keep that, so the two flavours of a
#' plot are recognisably the same plot.
#'
#' @param names names of the cut-offs, e.g. c("min", "mid", "max") or the
#'   c("Qmin", "Qmid", "Qmax") that getVar(option = "quantile") produces
#' @return a named character vector of colours
#' @keywords internal
#' @noRd
cutoffColours <- function(names) {
  colours <- c("red", "green3", "blue")
  names(colours) <- names
  colours
}

#' @keywords internal
#' @noRd
cutoffLinetypes <- function(names) {
  linetypes <- c(1, 2, 3)
  names(linetypes) <- names
  linetypes
}
