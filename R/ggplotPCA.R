#' PCA score plot (ggplot2)
#'
#' `ggplot2` version of [plotPCA()]. Plots PCA scores with the possibility to
#' choose which principal components to show, to colour observations by a
#' continuous variable and to use different plotting symbols for a categorical
#' variable.
#'
#' Unlike [plotPCA()], this function has no `file` argument: it returns a
#' `ggplot` object, which you save with `ggplot2::ggsave()`.
#'
#' @param pca A `prcomp` object
#' @param PC1 Principal component on x-axis
#' @param PC2 Principal component on y-axis
#' @param colVar Continuous variable for colouring observations
#' @param symbVar Categorical/discrete variable for multiple plot symbols
#' @param main Optional plot title
#' @return A `ggplot` object
#' @seealso [plotPCA()] for the base graphics version
#' @export
#' @examples
#' data("freelive2")
#' pca_object <- prcomp(XRVIP2)
#' ggplotPCA(pca_object, colVar = YR2)
ggplotPCA <- function(pca,
                      PC1 = 1,
                      PC2 = 2,
                      colVar,
                      symbVar,
                      main = NULL) {
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
    geom_point() +
    labs(x = d$xlab, y = d$ylab, title = main,
         colour = NULL, shape = NULL) +
    theme_bw()

  if (hasCol) {
    ## Same blue-yellow-red gradient as the base version
    p <- p + scale_colour_gradientn(colours = c("blue", "yellow", "red"))
  }

  p
}
