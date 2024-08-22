#' Plot validation metric
#' Produces a plot of validation metric vs number of variables in model (inner segment)
#' @param MUVRclassObject An object of class `MUVR`
#' @param show_outlier deciding show outlier on the information of not
#' @return A plot
#' @export
#' @examples
#' \dontrun{
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
#' plotVAL(regrModel)
#' }
plotVAL <- function(MUVRclassObject,
                    show_outlier=TRUE) {
  if (class(MUVRclassObject)[1] != "MUVR") {
    stop("Wrong classobject")
  }
  if (!is.null(MUVRclassObject$varTable)) {
    if (names(MUVRclassObject$nVar)[1] == "Qmin") {
      metric <- MUVRclassObject$VAL$metric
      ###### for quantile
      dist <- as.matrix(MUVRclassObject$nonZeroRep)
      s <- hist(
        dist,
        xlab = "Number of variables selected across nOuter*nRep loops",
        main = NULL,
        #log="x",
        #ylim=c(0,5),
        #ylab=metric,
        xlim = range(0,
                     max(MUVRclassObject$nonZeroRep) * 1.1),
        breaks = nrow(dist) * ncol(dist)
      )
      for (i in 1:3) {
        ####add vertical line
        abline(
          v = MUVRclassObject$nVar[i],
          lty = i,
          col = i + 1,
          lwd = 1.5
        )                                 ####line wide
      }

      legend(
        'topright',
        legend = c("Min", "Median", "Max"),
        lty = 1:3,
        cex = 0.5,
        trace = FALSE,
        ####line type
        col = 2:4,
        bty = 'n'
      )                   ####no border of the legend
      # abline(v = MUVRclassObject$nVar_quantile[1])
      #  text(MUVRclassObject$nVar_quantile[1],max(s$counts)*0.8, "min",  adj = c(0, -.1))
      #  abline(v =  MUVRclassObject$nVar_quantile[2], lwd = 2)
      #  text(MUVRclassObject$nVar_quantile[2],max(s$counts)*0.8, "mid",  adj = c(0, -.1))
      #  abline(v =  MUVRclassObject$nVar_quantile[3])
      #  text(MUVRclassObject$nVar_quantile[3],max(s$counts)*0.8, "max",  adj = c(0, -.1))

    } else if (names(MUVRclassObject$nVar)[1] == "min") {
      nonZeroRep <- MUVRclassObject$nonZeroRep
      fitnessRep <- MUVRclassObject$fitnessRep

      nonZeroRep_vector <- c(t(nonZeroRep))
      #nonZeroRep_vector<-log(nonZeroRep_vector)
      fitnessRep_vector <- c(t(fitnessRep))
      nonZeroRep_vector_grid <-
        seq(min(nonZeroRep_vector), max(nonZeroRep_vector), 1)

      metric <- MUVRclassObject$VAL$metric

      fit_curve <- MUVRclassObject$fit_curve
      if (fit_curve == "gam") {
        ### for smoothcurve
        #######################################################
        ###################################33
        ### The limitation of using orginal variable
        k <- MUVRclassObject$k
        dataframe <-
          as.data.frame(cbind(nonZeroRep_vector, fitnessRep_vector))
        ssqRatio <- 1.5
        fitnessRep_vector_origin <- fitnessRep_vector
        nonZeroRep_vector_origin <- nonZeroRep_vector
        fitnessRep_vector <-
          fitnessRep_vector[MUVRclassObject$outlier_info == "black"]
        nonZeroRep_vector <-
          nonZeroRep_vector[MUVRclassObject$outlier_info == "black"]
        gam_model <- mgcv::gam(fitnessRep_vector ~
                                 s(nonZeroRep_vector, bs = 'ps', k = k),
                               # knots=5,
                               data =    dataframe)
        dataframe_forpredict <-
          data.frame(nonZeroRep_vector = nonZeroRep_vector_grid,
                     fitnessRep_vector = rep(0, length(nonZeroRep_vector_grid)))

        predict_result_gam <-
          predict.gam(gam_model, dataframe_forpredict)
        fitnessRep_vector <- fitnessRep_vector_origin
        nonZeroRep_vector <- nonZeroRep_vector_origin
        if(show_outlier==TRUE){
        plot(
          #exp(nonZeroRep_vector),
          #fitnessRep_vector,
          #log='x',
          #xlim = c(0, max(nonZeroRep_vector_grid)),
          nonZeroRep_vector,
          ##
          fitnessRep_vector,
          # data = dataframe,
          col = MUVRclassObject$outlier_info,
          ylab = 'RMSEP'
        )
          }else{
            plot(
              #exp(nonZeroRep_vector),
              #fitnessRep_vector,
              #log='x',
              #xlim = c(0, max(nonZeroRep_vector_grid)),
              nonZeroRep_vector[MUVRclassObject$outlier_info == "black"],
              ##
              fitnessRep_vector[MUVRclassObject$outlier_info == "black"],
              # data = dataframe,
             # col = MUVRclassObject$outlier_info,
              ylab = 'RMSEP'
            )
        }
        #lines(exp(nonZeroRep_vector_grid),
        #      predict_result_gam)
        lines(nonZeroRep_vector_grid,
              predict_result_gam)
        for (i in 1:3) {
          ####add vertical line
          abline(
            #v=exp(MUVRclassObject$nVar[i]),
            v = MUVRclassObject$nVar[i],
            lty = i,
            col = i + 1,
            lwd = 1.5
          )                                 ####line wide
        }
      }
      #######################################################
      ###################################################
      if (fit_curve == "loess") {
        span <- MUVRclassObject$span
        fitnessRep_vector_origin <- fitnessRep_vector
        nonZeroRep_vector_origin <- nonZeroRep_vector
        fitnessRep_vector <-
          fitnessRep_vector[MUVRclassObject$outlier_info == "black"]
        nonZeroRep_vector <-
          nonZeroRep_vector[MUVRclassObject$outlier_info == "black"]
        fit_temp <- loess(fitnessRep_vector ~
                            nonZeroRep_vector,
                          span = span,
                          degree = 2)
        predict_temp <- predict(fit_temp,
                                newdata = data.frame(nonZeroRep_vector = nonZeroRep_vector_grid))
        fitnessRep_vector <- fitnessRep_vector_origin
        nonZeroRep_vector <- nonZeroRep_vector_origin
        if(show_outlier==TRUE){
        plot(
          #exp(nonZeroRep_vector),
          #fitnessRep_vector,
          #log='x',
          nonZeroRep_vector,
          fitnessRep_vector,
          col = MUVRclassObject$outlier_info,
          xlab = "Number of variables selected across nOuter*nRep loops",
          ylab = metric
        )
        }else{
          plot(
            #exp(nonZeroRep_vector),
            #fitnessRep_vector,
            #log='x',
            nonZeroRep_vector[MUVRclassObject$outlier_info == "black"],
            fitnessRep_vector[MUVRclassObject$outlier_info == "black"],
            #col = MUVRclassObject$outlier_info,
            xlab = "Number of variables selected across nOuter*nRep loops",
            ylab = metric
          )
        }
        #lines(exp(nonZeroRep_vector_grid),
        #      predict_temp)
        lines(nonZeroRep_vector_grid, predict_temp)
        for (i in 1:3) {
          ####add vertical line
          abline(
            #v=exp(MUVRclassObject$nVar[i]),
            v = MUVRclassObject$nVar[i],
            lty = i,
            col = i + 1,
            lwd = 1.5
          )                                 ####line wide
        }

        legend(
          'topright',
          legend = c(
            "'Min (Minimal-optimal)",
            "'Mid'",
            "'Max' (All-relevant)"
          ),
          lty = 1:3,
          cex = 0.5,
          trace = FALSE,
          ####line type
          col = 2:4,
          bty = 'n'
        )
      }
    }
    ##############################################################################################
    ##############################################################################################
    #################################################################################################
  } else{
    VAL <-
      MUVRclassObject$VAL$VAL      ### a list of nRep array,  row is outer segment,column is 1147,917,713
    metric <- MUVRclassObject$VAL$metric
    count <-
      as.numeric(colnames(VAL))      ###colnames transformed to number
    nRep <- dim(VAL)[3]
    plot(
      count,
      count,
      ylim = range(VAL),
      ##### range of miss classification numbers
      xlim = range(count),
      ######range of variables numbers
      log = 'x',
      ###########log scale x axis
      type = 'n',
      ####What type of plot should be drawn, n is for no plotting
      bty = 'l',
      #####border type
      ylab = metric,
      xlab = 'Number of variables (log scale)'
    )
    for (r in 1:nRep) {
      matlines(
        count,
        t(VAL[, , r]),
        type = 'l',
        ##This is to say to plot line
        lty = 1,
        ###line type
        col = 'lightgrey'
      )
    }
    for (r in 1:nRep) {
      lines(count,
            colMeans(VAL[, , r]), col = 'darkgrey')     ####mean of each column,output has ncol number of means
    }
    lines(count,                  ####mean of all the repetitions  ###x
          apply(VAL, 2, mean),      ###y mean for all segments and all repetitions  for each column of variable numbers
          col = 'black')

    for (i in 1:3) {
      ####add vertical line
      abline(
        v = MUVRclassObject$nVar[i],
        lty = i,
        col = i + 1,
        lwd = 1.5
      )                                 ####line wide
    }
    legend(
      'topleft',
      legend = c('Validation segments',
                 'Repetitions',
                 'Overall'),
      cex = 0.5,
      trace = FALSE,
      lty = 1,
      col = c('lightgrey', 'darkgrey', 'black'),
      bty = 'n'
    )
    legend(
      'topright',
      legend = c("'Min (Minimal-optimal)", "'Mid'", "'Max' (All-relevant)"),
      lty = 1:3,
      cex = 0.5,
      trace = FALSE,
      ####line type
      col = 2:4,
      bty = 'n'
    )                   ####no border of the legend


  }
}
