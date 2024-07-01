#' PLS biplot:  Makes a biplot of a fitted object (e.g. from a MUVR with PLS core)
#' @param fit A PLS fit (e.g. from MUVRclassObject$Fit[[2]])
#' @param comps Which components to plot
#' @param xCol (Optional) Continuous vector for grey scale gradient of observation (sample) color (e.g. Y vector in regression analysis)
#' @param labPlSc Boolean to plot observation (sample) names (defaults to TRUE)
#' @param labs (Optional) Labels names
#' @param vars Which variables to plot (names in rownames(loadings))
#' @param labPlLo Boolean to plot variable names (defaults to TRUE)
#' @param pchSc Plotting character for observation scores
#' @param colSc Colors for observation scores (only if xCol omitted)
#' @param colLo Colors for variable loadings (defaults to red)
#' @param supLeg Boolean for whether to suppress legends
#' @return A PLS biplot
#' @export
#' @examples
#'nRep <- 2 # Number of MUVR2 repetitions
#'nOuter <- 3 # Number of outer cross-validation segments
#'varRatio <- 0.75 # Proportion of variables kept per iteration
#'method <- 'PLS' # Selected core modeling algorithm
#'regrModel <- MUVR2(X = XRVIP2,
#'                   Y = YR2,
#'                   nRep = nRep,
#'                   nOuter = nOuter,
#'                   method = method)
#'biplotPLS(regrModel$Fit[[2]])
biplotPLS <- function(fit,
                      #####A PLS fit (e.g. from MUVRclassObject$Fit[[2]]) could be [[3]],[[4]]
                      comps = 1:2,
                      xCol,
                      labPlSc = TRUE,
                      labs,
                      vars,
                      labPlLo = TRUE,
                      pchSc = 16,
                      colSc,
                      colLo = 2,
                      supLeg = FALSE) {
  cex <-
    par()$cex  ###A numerical value giving the amount by which plotting text and symbols should be magnified relative
  ##to the default. This starts as 1 when a device is opened, and is reset when the layout is changed, e.g. by setting mfrow.

  par(mar = c(4, 4, 4, 4) + .5)

  scores <- fit$variates$X[, comps]   ##scores for component 1 and 2
  loads <-
    fit$loadings$X[, comps]    ##loadings for component 1 and 2

  if (missing(vars)){
    vars <-
    rownames(loads) }##variable wanted to be shown, if not specified, choose all

  loads <-
    loads[rownames(loads) %in% vars, ]  ###only choose the name that is specified in vars

  nSamp <- nrow(scores)          ###observation number
  nVar <- nrow(loads)           ##selected variables number

  if (missing(xCol)) {
    ###(Optional) Continuous vector for grey scale gradient of observation (sample) color (e.g. Y vector in regression analysis)
    if (missing(colSc)) {
      ### Colors for observation scores (only if xCol omitted)
      colSc <-
        rep(1, nSamp)    ### Colors for observation scores (only if xCol omitted)
      legPlot <- FALSE         ###there is no legend plot
    } else {
      colScLeg <- colSc    ##color on legend
      colSc <- as.factor(colSc)
      legPlot <- TRUE      ###there is legend plot
    }
  } else {
    x.col <- 10 + round(85 * ((max(xCol) - xCol) / (diff(range(
      xCol
    )))))   ###
    colSc <- paste("gray", x.col, sep = "")
    legPlot <- TRUE         ####there is legend plot
  }

  if (supLeg)
    {legPlot <- FALSE}     ###supress legend

  rLo <- max(abs(loads))  ###for limit of loding
  rLo <- 1.1 * c(-rLo, rLo)

  #########################################################################a loading pplot
  plot(
    loads,
    xlim = rLo,
    ylim = rLo,
    type = 'n',
    xlab = '',
    ylab = '',
    main = '',
    axes = FALSE
  )   ##no axes

  box(bty = 'o')   ###border is squire
  axis(3)    ###1=below, 2=left, 3=above and 4=right.
  axis(4,
       las = 1)   ###horizontal ( 1 ), always perpendicular to the axis ( 2 ), and always vertical ( 3 ).
  mtext('Loadings',    ##this is mtext
        3,
        line = 3,       ###the place of line
        cex = cex)
  mtext('Loadings',
        4,
        line = 3,
        cex = cex)
  abline(
    h = 0,
    ###add axis at origin point
    v = 0,
    lty = 2,
    ##line type
    col = 'grey'
  )

  arrows(rep(0, nrow(loads)),  ##x0  coordinates of points from which to draw.
         rep(0, nrow(loads)),  ##y0  coordinates of points from which to draw.
         loads[, 1],         ### x1 coordinates of points to which to draw. At least one must the supplied
         loads[, 2],         ### y1 coordinates of points to which to draw. At least one must the supplied
         col = colLo)         ###Colors for variable loadings (defaults to red)

  #####label on loading
  if (labPlLo)
   { text(1.1 * loads,
         ###numeric vectors of coordinates where the text labels should be written. If the length of x and y differs, the shorter one is recycled.
         rownames(loads),
         ###a character vector or expression specifying the text to be written.
         cex = .7 * cex,
         ##numeric character expansion factor; multiplied by par("cex") yields the final character size. NULL and NA are equivalent to 1.0.
         font = 3)  ###Boolean to plot variable names (defaults to TRUE)
}
  ###tells R to make the second plot without cleaning the first.
  par(new = TRUE)

  #################score plot
  rSc <- max(abs(scores))   ###scores range

  rSc <- c(-rSc, rSc)

  plot(
    scores,
    ####  Scores with x and y value
    col = colSc,
    ####Colors for observation scores (only if xCol omitted)
    pch = pchSc,
    ###pch for observation scores (only if xCol omitted)
    ylim = rSc,
    xlim = rSc,
    bty = 'l',
    xlab = paste('Component', comps[1], 'Scores'),
    ylab = paste('Component', comps[2], 'Scores'),
    axes = FALSE
  )

  axis(1)
  axis(2,
       las = 1)   ###axis label orientatio

  if (labPlSc) {
    if (missing(labs))
      {labs <- rownames(scores)}

    # cat(labs)
    text(scores,      ####numeric vectors of coordinates where the text labels should be written.
         as.character(labs),  ###a character vector or expression specifying the text to be written
         pos = 3)      ####	a position specifier for the text. If specified this overrides any adj value given.
    ####Values of 1, 2, 3 and 4, respectively indicate positions below, to the left of, above and to the right of the specified (x,y) coordinates.
  }

  if (legPlot) {
    ###do legend plot or not
    if (missing(xCol)) {
      legend(
        'topright',
        legend = unique(colScLeg),
        ###for score legend
        pch = unique(pchSc),
        ###Plotting character for observation scores
        col = unique(colSc),
        ###Colors for observation scores (only if xCol omitted)
        bty = 'n'
      )
    } else {
      whUnik <-
        !duplicated(xCol)    ##(Optional) Continuous vector for grey scale gradient of observation (sample) color (e.g. Y vector in regression analysis)
      ##whUnik is a logical variable, if it is duplicated output TRUE, here the duplicated is FALSE
      unik <- xCol[whUnik]           ###the unik ones
      cols <-
        colSc[whUnik][order(unik)]   ###Colors for observation scores (only if xCol omitted)
      unik <- sort(unik)
      if (length(unik) > 10) {
        k <- (length(unik) - 1) / 5
        n <- 1 + k * 0:5
        cols <- cols[n]
        unik <- unik[n]
      }

      legend(
        'topright',
        legend = signif(unik, 3),
        ##round it to 3 digits
        fill = cols,
        ###if specified, this argument will cause boxes filled with the specified colors (or shaded in the specified colors) to appear beside the legend text.
        bty = 'n'
      )
    }
  }
}
