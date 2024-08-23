#' PCA score plot from prcomp()
#' Customised PCA score plots with the possibility to choose PCs, exporting to png and the possibility to add color or different plotting symbols according to variable.
#' @param pca A `prcomp` object
#' @param PC1 Principal component on x-axis
#' @param PC2 Principal component on y-axis
#' @param file If specified provides the name of a png export file. Otherwise normal plot.
#' @param colVar Continuous variable for coloring observations (40 cuts)
#' @param symbVar Categorical/discrete variable for multiple plot symbols
#' @param main If provided provides a main title of the plot
#' @return A PCA score plot. Exported as png if `file` specified in function call.
#' @importFrom psych principal
#' @export
#' @examples
#' \donttest{
#' data("freelive2")
#' pca_object<-prcomp(XRVIP2)
#' plotPCA(pca_object)
#' }
plotPCA <- function(pca,
                    PC1 = 1,
                    PC2 = 2,
                    file,
                    colVar,
                    symbVar,
                    main = '') {
  ############################################################################
  ##for color variable
  if (missing(colVar)) {
    col <- 1                         ##black
    colLeg <- FALSE
  } else {
    cols <-
      colorRampPalette(c('blue', 'yellow', 'red'))(40)   ####divide color to smaller color
    col <-
      cols[cut(colVar, 40)]               ################cut color to 40 intervals and it is transformed to a factor variable now
    #################what is the meaning of colVar
    colLeg <- TRUE                        ##there is color legend
  }

  #########################################################################################
  ##For symbol variable
  if (missing(symbVar)) {
    pch <- 1           ###point type hollow circle
    symbLeg <-
      FALSE   ####Categorical/discrete variable for multiple plot symbols
  } else {
    symbVar <- factor(symbVar)   ###symbVar is also number
    symbs <- c(1, 2, 0, 6)
    nSymb <- length(levels(symbVar))   ######length of symbVar
    if (nSymb > 4)
    {
      ####if length of symbol is bigger than 4 then 1,2,0,6,   9,10...
      symbs <- c(symbs, 9:(nSymb + 4))
    }
    pch <- symbs[symbVar]
    symbLeg <- TRUE                    ###there is symbol legend
  }

  #########################################################################################
  ##For png
  plotPNG <- ifelse(missing(file),
                    FALSE,
                    TRUE)

  if (plotPNG) {
    png(
      filename = file,
      width = 1024,
      ### The width of the device.
      height = 1024,
      ### The height of the device.
      pointsize = 36
    )
  }  ### The default pointsize of plotted text, interpreted as big points (1/72 inch) at res ppi.
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  if (main == '') {
    par(mar = c(4, 4, 0, 0) + .5)
  }  ######c(bottom, left, top, right)
  else {
    par(mar = c(4, 4, 2, 0) + .5)
  }

  pcVar <-
    summary(pca)$importance[2, ]    ####proportion of variabce

  xlab <-
    paste('PC', PC1, ' (R2X=', signif(pcVar[PC1], 3), ')', sep = '')   ###PC1 label proportion of variance
  ylab <-
    paste('PC', PC2, ' (R2X=', signif(pcVar[PC2], 3), ')', sep = '')   ###PC2 label proportion of variance


  plot(
    pca$x[, c(PC1, PC2)],
    ##observations
    main = main,
    col = col,
    ### repeated color is sequenced by observations
    xlab = xlab,
    ylab = ylab,
    pch = pch
  ) # scoreplot

  if (colLeg) {
    legend(
      'topleft',
      col = c('blue', 'yellow', 'red'),
      legend = c('low', 'mid', 'high'),
      pch = 1
    )
  }                       ###hollow circle

  if (symbLeg) {
    legend(
      'bottomleft',
      col = 1,
      legend = levels(symbVar),
      pch = symbs[1:length(levels(symbVar))]
    )
  }

  abline(h = 0,
         lty = 2)
  abline(v = 0,
         lty = 2)

  if (plotPNG) {
    dev.off()
  }
}
