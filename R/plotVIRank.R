#' Plots variable importance ranking in MUVR object
#' Regardless of MV core method, variables are sorted by rank, where lower is better. `plotVIRank` produces boxplots of variable rankings for all model repetitions.
#' @param MUVRclassObject An MUVR class object only applied to PLS, RF not rdCVnet
#' @param n Number of top ranking variables to plot (defaults to those selected by MUVR2)
#' @param cut Optional value to cut length of variable names to `cut` number of characters
#' @param model Which model to choose ('min', 'mid' {default} or 'max')
#' @param maptype for rdCvnet dot plot or heat map
#' @param add_blank put more blank when the rownames is too long,
#' @param cextext the cex of the text
#' @return Barplot of variable rankings (lower is better)
#' @export
plotVIRank <- function(MUVRclassObject,
                       n,
                       model = 'min',
                       cut,
                       maptype = c("heatmap", "dotplot"),
                       add_blank = 4) {
  par(mar = c(4, 4, 4, 4) + .5)
  if (!(class(MUVRclassObject)[1] == 'MUVR')) {
    cat('\nWrong object class: Return NULL')

  }
  # if(missing(n)){n=ncol(MUVRclassObject$inData$X)}

  if ((class(MUVRclassObject)[3] == 'rdCVnet')) {
    nModel <- ifelse(model == 'min',
                     1,
                     ifelse(model == 'mid', 2, 3))
    nFeat <- round(MUVRclassObject$nVar[nModel])
    if (missing(n)) {
      n <- nFeat
    }
    matrix_count <- matrix(
      0,
      nrow = length(MUVRclassObject$nonZeroRep),
      ncol = ncol(MUVRclassObject$inData$X)
    )
    rownames(matrix_count) <- 1:nrow(matrix_count)

    colnames(matrix_count) <-
      names(MUVRclassObject$varTable)   ### the variables that are selected more times are in the beginning of the plot

    for (i in 1:length(MUVRclassObject$varRep)) {
      for (j in 1:ncol(MUVRclassObject$inData$X)) {
        if (colnames(matrix_count)[j] %in% MUVRclassObject$varRep[[i]]) {
          matrix_count[i, j] <- 1
        }
      }
    }
    if (missing(maptype)) {
      maptype  <-  "heatmap"
    }
    if (!maptype %in% c("heatmap", "dotplot")) {
      stop("maptype can only be heatmap or dotplot")
    }
    if (maptype == "heatmap") {
      ################################################################
      ##### par(mar=c(5, 4, 4, 8),xpd=TRUE)  ### This is to give some place to legend
      par(mar = c(add_blank, 4, 4, 8),
          xpd = TRUE)
      matrix_count_t <- t(matrix_count)
      matrix_count_t_sdnot0 <-
        matrix(NA, nrow = nrow(matrix_count_t), 0)
      rownames(matrix_count_t_sdnot0) <- rownames(matrix_count_t)
      matrix_count_t_sd0 <-
        matrix(NA, nrow = nrow(matrix_count_t), 0)
      rownames(matrix_count_t_sd0) <- rownames(matrix_count_t)
      matrix_count_t_sdnot0_colnames <- c()
      matrix_count_t_sd0_colnames <- c()

      for (i in 1:ncol(matrix_count_t)) {
        if (!length(table(matrix_count_t[, i])) == 1) {
          matrix_count_t_sdnot0 <- cbind(matrix_count_t_sdnot0,
                                         matrix_count_t[, i])
          matrix_count_t_sdnot0_colnames <-
            c(matrix_count_t_sdnot0_colnames,
              colnames(matrix_count_t)[i])
        } else{
          matrix_count_t_sd0 <- cbind(matrix_count_t_sd0,
                                      matrix_count_t[, i])
          matrix_count_t_sd0_colnames <-
            c(matrix_count_t_sd0_colnames,
              colnames(matrix_count_t)[i])
        }
      }
      colnames(matrix_count_t_sd0) <- matrix_count_t_sd0_colnames
      colnames(matrix_count_t_sdnot0) <-
        matrix_count_t_sdnot0_colnames
      clust <- hclust(as.dist(1 - cor(matrix_count_t_sdnot0)))

      #####################################################################################
      ###############################################################
      ### This is to use the clust
      matrix_count <-
        t(cbind(matrix_count_t_sd0, matrix_count_t_sdnot0[, clust$order]))

      ######################################################################################
      #####################################################################################
      ## This is to use the number of variables selected to order
      number_of1 <- c()
      for (i in 1:nrow(matrix_count))
      {
        number_of1 <- c(number_of1,
                        table(matrix_count[i, ])["1"])

      }
      matrix_count_temp <- matrix_count[order(number_of1), ]
      matrix_count <- matrix_count_temp
      #### add hierarchial clustering
      heatmap(
        matrix_count[, 1:n],
        Colv = NA,
        Rowv = NA,
        col = c("white", "red"),
        scale = "none",
        #labCol = NA,
        labRow = NA,
        #labRow=rownames(matrix_count[,1:n]),
        #labCol=colnames(matrix_count[,1:n]),
        revC = FALSE,
        xlab = paste(n, "variables ordered by selection ratio"),
        ylab = paste(
          MUVRclassObject$inData$nRep * MUVRclassObject$inData$nOuter,
          "calibration set models"
        )
      )
      # box(col="black")
      legend(
        x = "topright",
        inset = c(-0.5, 0.1),
        legend = c("Selected", "Not selected"),
        fill = c("red", "white"),
        cex = 0.5,
        trace = FALSE,
        title = "Variable"
      )

    } else{
      par(mar = c(5, 4, 4, 8),
          xpd = TRUE)
      plot(
        1,
        type = "n",
        # Remove all elements of plot
        xlab = paste(n, "variables ordered by selection ratio"),
        ylab = paste(
          MUVRclassObject$inData$nRep * MUVRclassObject$inData$nOuter,
          "calibration set models"
        ),
        ylim = c(0, length(MUVRclassObject$varRep)),
        xlim = c(0, n)
      )
      box(col = "black")

      for (i in 1:length(MUVRclassObject$varRep)) {
        for (j in 1:n) {
          col <- ifelse(matrix_count[i, j] == 1,
                        "red",
                        "white")
          points(x = j,
                 y = i,
                 col = col)

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




    ##############################################################################################################
    ## PLS or RF
  } else{
    par(mar = c(4, add_blank , 2, 2),
        xpd = TRUE)
    nModel  <-  ifelse(model == 'min',
                       1,
                       ifelse(model == 'mid', 2, 3))
    nFeat  <-  round(MUVRclassObject$nVar[nModel])
    if (missing(n)) {
      n  <-  nFeat
    }
    VIRank <- MUVRclassObject$VIRank[, nModel]
    VIRankRep <- MUVRclassObject$VIRankPerRep[[nModel]]
    ####################################################################################################
    VIRankRep <- VIRankRep[order(VIRank), ][1:n, ]
    ####the first lowest number is in position (the first output 1)
    ####the second lowest number is in position (the output 1)
    ##

    ########################################################################################################

    if (n > nFeat) {
      VIRankRep <- rbind(VIRankRep[1:nFeat, ],
                         rep(NA, ncol(VIRankRep)),
                         VIRankRep[(nFeat + 1):n, ])
      col <- rep(c('yellow', 'grey'),
                 c(nFeat, (n - nFeat + 1)))    ###repeat yellow nFeat times, repear grey n-nFeat+1 times
    } else {
      col <- NULL

    }
    ####
    VIRankRep <-
      VIRankRep[nrow(VIRankRep):1, ]             ###reverse
    col <-
      rev(col)                                          ###reverse the sequence of col
    boxplot(
      t(VIRankRep),
      ### row is rep,column is variable
      horizontal = TRUE,
      ###	logical indicating  the boxplots should be horizontal;
      axes = FALSE,
      ### Do not add any axes
      col = col,
      xlab = "Variable Importance",
      cex.axis = cextext,
      cex.lab = cextext
    )                                 ###when outside n it is grey,  inside it is yellow
    axis(1)                                         ###manually add x axis
    labels <- rownames(VIRankRep)                    ##add labels

    if (!missing(cut)) {
      labels <- substring(labels, 1, cut)
    }  ###Extract or replace substrings in a character vector
    ###xxx <- c("asfef", "qwerty", "yuiop[", "b", "stuff.blah.yech")
    ###substr(xxx, 2, 5)
    ###[1] "sfef" "wert" "uiop" ""     "tuff"
    axis(2,
         ###manually add y axis,
         las = 1,
         ###label is horzontal
         at = 1:nrow(VIRankRep),
         ###label position
         labels = labels)

    if (n > nFeat) {
      abline(h = (n - nFeat + 1))
    }      ###add a line that separate the variable inside and outside n
    box(bty = 'o')                                       ###add a box that has o shape


  }

}
