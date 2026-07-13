#' PCA score plot (ggplot2)
#'
#' `ggplot2` version of [plotPCA()]. Plots PCA scores with the possibility to
#' choose which principal components to show, to colour observations by a
#' continuous variable and to use different plotting symbols for a categorical
#' variable.
#'
#' Two deliberate differences from [plotPCA()]:
#'
#' * The colour legend is a continuous scale showing the actual values of
#'   `colVar`, rather than the base version's three-swatch "low / mid / high"
#'   key. `colVar` *is* continuous -- the base plot cuts it into 40 shades and
#'   then labels only three of them -- so a colour bar says what is plotted and
#'   at what value, where the key only says which end is which. Both legends are
#'   titled with the name of the variable, so it is clear what is being shown.
#' * There is no `file` argument. This returns a `ggplot` object, which you save
#'   with `ggplot2::ggsave()`.
#'
#' @param pca A `prcomp` object
#' @param PC1 Principal component on x-axis
#' @param PC2 Principal component on y-axis
#' @param colVar Continuous variable for colouring observations
#' @param symbVar Categorical/discrete variable for multiple plot symbols
#' @param colLab Legend title for `colVar` (defaults to the name of the variable
#'   passed in)
#' @param symbLab Legend title for `symbVar` (defaults to the name of the
#'   variable passed in)
#' @param main Optional plot title
#' @return A `ggplot` object
#' @seealso [plotPCA()] for the base graphics version
#' @export
#' @examples
#' data("freelive2")
#' pca_object <- prcomp(XRVIP2)
#'
#' # Without colLab the legend is titled "YR2", i.e. whatever expression was
#' # passed in, which tells the reader nothing about what is being coloured
#' ggplotPCA(pca_object, colVar = YR2, colLab = "Rye intake")
ggplotPCA <- function(pca,
                      PC1 = 1,
                      PC2 = 2,
                      colVar,
                      symbVar,
                      colLab = NULL,
                      symbLab = NULL,
                      main = NULL) {
  ## Name the legends after whatever was passed in, so the reader can tell what
  ## the colours and symbols mean without going back to the call
  if (is.null(colLab)) {
    colLab <- if (missing(colVar)) NULL else deparse(substitute(colVar))
  }
  if (is.null(symbLab)) {
    symbLab <- if (missing(symbVar)) NULL else deparse(substitute(symbVar))
  }

  d <- pcaData(pca, PC1 = PC1, PC2 = PC2,
               colVar = colVar, symbVar = symbVar)
  scores <- d$scores

  hasCol <- !is.null(scores$colVar)
  hasSymb <- !is.null(scores$symbVar)

  mapping <- if (hasCol && hasSymb) {
    aes(x = .data$x, y = .data$y,
        colour = .data$colVar, shape = .data$symbVar)
  } else if (hasCol) {
    aes(x = .data$x, y = .data$y, colour = .data$colVar)
  } else if (hasSymb) {
    aes(x = .data$x, y = .data$y, shape = .data$symbVar)
  } else {
    aes(x = .data$x, y = .data$y)
  }

  p <- ggplot(scores, mapping) +
    geom_hline(yintercept = 0, linetype = 2, colour = "grey60") +
    geom_vline(xintercept = 0, linetype = 2, colour = "grey60") +
    geom_point(size = 2) +
    labs(x = d$xlab, y = d$ylab, title = main,
         colour = colLab, shape = symbLab) +
    theme_muvr()

  if (hasCol) {
    ## Same blue-yellow-red gradient as the base version, but as a colour bar
    ## reading in the variable's own units rather than three unlabelled swatches
    p <- p + scale_colour_gradientn(colours = c("blue", "yellow", "red"))
  }

  p
}
