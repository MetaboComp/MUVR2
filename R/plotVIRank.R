#' Plot variable importance ranking
#'
#' Plot variable importance ranking in MUVR object. Regardless of MV core method, variables are sorted by rank, where lower is better.
#' `plotVIRank` produces boxplots of variable rankings for all model repetitions.
#' @param MUVRclassObject An MUVR class object only applied to PLS, RF not rdCVnet
#' @param n Number of top ranking variables to plot (defaults to those selected by MUVR2)
#' @param cut Optional value to cut length of variable names to `cut` number of characters
#' @param model Which model to choose ('min', 'mid' (default) or 'max')
#' @param maptype for rdCvnet dot plot or heat map
#' @param add_blank put more blank when the rownames is too long,
#' @param cextext the cex of the text
#' @return Barplot of variable rankings (lower is better)
#' @seealso [ggplotVIRank()] for the ggplot2 version
#' @export
#' @examples
#' \donttest{
#' data("freelive2")
#' nRep <- 2
#' nOuter <- 4
#' varRatio <-0.6
#' regrModel <- MUVR2(X = XRVIP2,
#'                    Y = YR2,
#'                    nRep = nRep,
#'                   nOuter = nOuter,
#'                    varRatio = varRatio,
#'                    method = "PLS",
#'                    modReturn = TRUE)
#' plotVIRank(regrModel, n=20)
#' }
plotVIRank <- function(MUVRclassObject,
                       n,
                       model = 'min',
                       cut,
                       maptype = c("heatmap", "dotplot"),
                       add_blank = 4,
                       cextext = 1) {
  maptype <- match.arg(maptype)
  d <- viRankData(MUVRclassObject,
                  n = n,
                  model = model,
                  cut = cut,
                  ## the heatmap clusters the variables; the dot plot leaves
                  ## them in selection-ratio order
                  cluster = maptype == "heatmap")
  n <- d$n
  nFeat <- d$nFeat

  if (d$type == "selection") {
    ##############################################################################
    ## Elastic net: a variable is either given a non-zero coefficient in a given
    ## calibration model, or it is not. Show that as a map rather than as ranks.
    ##############################################################################
    matrix_count <- d$matrix_count

    if (maptype == "heatmap") {
      heatmap(
        matrix_count[, 1:n],
        Colv = NA,
        Rowv = NA,
        col = c("white", "red"),
        scale = "none",
        labRow = NA,
        revC = FALSE,
        xlab = paste(n, "variables ordered by selection ratio"),
        ylab = paste(d$nModels, "calibration set models")
      )
      legend(
        x = "topright",
        inset = c(-0.5, 0.1),
        legend = c("Selected", "Not selected"),
        fill = c("red", "white"),
        cex = 0.5,
        trace = FALSE,
        title = "Variable"
      )

    } else {
      plot(
        1,
        type = "n",
        xlab = paste(n, "variables ordered by selection ratio"),
        ylab = paste(d$nModels, "calibration set models"),
        ylim = c(0, nrow(matrix_count)),
        xlim = c(0, n)
      )
      box(col = "black")

      for (i in 1:nrow(matrix_count)) {
        for (j in 1:n) {
          points(x = j,
                 y = i,
                 col = ifelse(matrix_count[i, j] == 1, "red", "white"))
        }
      }

      legend(
        "topright",
        legend = c("Selected", "Not selected"),
        pch = 1,
        inset = c(-0.2, 0.1),
        col = c("red", "white"),
        cex = 0.5,
        trace = FALSE,
        title = "Variable"
      )
    }

    ##############################################################################
    ## PLS or RF: boxplot of each variable's rank across repetitions
    ##############################################################################
  } else {
    VIRankRep <- d$VIRankRep
    labels <- d$labels

    if (n > nFeat) {
      ## Blank row separating the variables inside the selection from those
      ## outside it; inside is yellow, outside is grey
      VIRankRep <- rbind(VIRankRep[1:nFeat, ],
                         rep(NA, ncol(VIRankRep)),
                         VIRankRep[(nFeat + 1):n, ])
      labels <- c(labels[1:nFeat], "", labels[(nFeat + 1):n])
      col <- rep(c('yellow', 'grey'),
                 c(nFeat, (n - nFeat + 1)))
    } else {
      col <- NULL
    }

    ## Reverse so the best-ranked variable ends up at the top of the plot
    VIRankRep <- VIRankRep[nrow(VIRankRep):1, ]
    labels <- rev(labels)
    col <- rev(col)

    boxplot(
      t(VIRankRep),
      horizontal = TRUE,
      axes = FALSE,
      col = col,
      xlab = "Variable Importance",
      cex.axis = cextext,
      cex.lab = cextext,
      cex.names = cextext
    )
    axis(1)
    axis(2,
         las = 1,
         at = 1:nrow(VIRankRep),
         labels = labels,
         cex.axis = cextext)

    if (n > nFeat) {
      abline(h = (n - nFeat + 1))
    }
    box(bty = 'o')
  }
}
