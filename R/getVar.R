#' The function will obtain the min, mid, max number of variables for an object generated from the rdCVnet() function
#' @param rdCVnetObject an object obtained from the rdCVnet() function
#' @param option quantile or fitness: In which way do you perform variable selection
#' @param fit_curve gam or loess method for fitting the curve in the fitness option
#' @param span parameter for using loess to fit curve in the fitness option:  how smooth the curve need to be
#' @param k parameter for using gam to fit curve in the fitness option
#' @param outlier if remove ourlier variables or not. There are 3 options: "none","IRQ", "residual"
#' @param quantile If the option is quantile, this value decides the cut for the first quantile, ranging from 0 to 0.5.
#' @param robust If the option is fitness, robust parameter decides how much deviation it is allowed from the optimal perdiction performance for min and max variabel selection.
#' @return a rdCVnet object
#' @export
#' @examples
#' \donttest{
#' data("mosquito")
#' nRep <- 2
#' nOuter <- 4
#' varRatio <-0.6
#' classModel <- MUVR2_EN(X = Xotu,
#'                        Y = Yotu,
#'                        nRep = nRep,
#'                        nOuter = nOuter,
#'                        DA = TRUE,
#'                        modReturn = TRUE)
#' classModel<-getVar(classModel)
#' }
getVar <- function(rdCVnetObject,
                   option = c("quantile", "fitness"),
                   fit_curve = c("loess", "gam"),
                   span = 1,
                   # c(0.5, 0.75, 1,1.25),
                   k = 5,
                   outlier = c("none", "IQR", "residual"),
                   robust = 0.05,
                   quantile = 0.25) {
  if(class(rdCVnetObject)[3]!="rdCVnet"){
    stop("Only rdCVnet(MUVR-elastic net) is allowed for getVar")}
  if (missing(fit_curve)) {
    fit_curve <- "loess"
  }
  if (missing(outlier)) {
    outlier <- "none"
  }
  if (quantile <= 0 | quantile >= 0.5 | !is.numeric(quantile)) {
    stop("\npercent_quantile must be between 0 and 0.5")
  }
  if (robust <= 0 | robust > 1 | !is.numeric(robust)) {
    stop("\npercent_smoothcurve must be between 0 and 1")
  }
  if (missing(option)) {
    option <- "fitness"
  }
  if (!outlier %in% c("none", "IQR", "residual")) {
    stop("\nThis option is not supported")
  }

  if (outlier == "IQR") {
    quartile_075 <- quantile(rdCVnetObject$nonZeroRep)[4]
    quartile_025 <- quantile(rdCVnetObject$nonZeroRep)[2]
    IQR <- quartile_075 - quartile_025
    low_boundary <- quartile_025 - 1.5 * IQR
    high_boundary <- quartile_075 + 1.5 * IQR
    num_of_variables <- vector()
    fitness <- vector()
    color_dot <- vector()
    for (i in 1:length(rdCVnetObject$nonZeroRep)) {
      if (t(rdCVnetObject$nonZeroRep)[i] <= high_boundary &
          t(rdCVnetObject$nonZeroRep)[i] >= low_boundary) {
        num_of_variables <- c(num_of_variables, t(rdCVnetObject$nonZeroRep)[i])
        fitness <- c(fitness, t(rdCVnetObject$fitnessRep)[i])
        color_dot <- c(color_dot, "black")
      } else{
        warning(
          "\n",
          t(rdCVnetObject$nonZeroRep)[i],
          "is an outlier for the number of variables selected. Therefore the combination of",
          "number of variables =",
          t(rdCVnetObject$nonZeroRep)[i],
          ",fitness =",
          t(rdCVnetObject$fitnessRep)[i],
          ",is removed from fitting curves."
        )
        color_dot <- c(color_dot, "grey")
      }
    }

  } else if (outlier == "none") {
    num_of_variables <- rdCVnetObject$nonZeroRep
    fitness <- rdCVnetObject$fitnessRep
    color_dot <- rep("black", length(rdCVnetObject$nonZeroRep))
  } else if (outlier == "residual") {
    nonZeroRep_vector <- as.vector(t(rdCVnetObject$nonZeroRep))
    fitnessRep_vector <- as.vector(t(rdCVnetObject$fitnessRep))
    dataframe <-
      as.data.frame(cbind(nonZeroRep_vector, fitnessRep_vector))
    ssqRatio <- 1.5
    gam_model <-
      mgcv::gam(fitnessRep_vector ~ s(nonZeroRep_vector, bs = 'ps'),
                # knots=5,
                data =    dataframe)
    pred <- predict(gam_model)
    SSq <- resid(gam_model) ^ 2
    meanSSq <- mean(SSq)
    keep <- ifelse((SSq / meanSSq) > ssqRatio, FALSE, TRUE)
    color_dot <- c()
    for (z in 1:length(keep)) {
      if (keep[z] == TRUE) {
        color_dot <- c(color_dot, "black")
      } else{
        color_dot <- c(color_dot, "grey")
        warning(
          "\n",
          t(rdCVnetObject$nonZeroRep)[z],
          "is an outlier for the number of variables selected. Therefore the combination of",
          "number of variables =",
          t(rdCVnetObject$nonZeroRep)[z],
          ",fitness =",
          t(rdCVnetObject$fitnessRep)[z],
          ",is removed from fitting curves."
        )
      }
    }

    num_of_variables <-
      t(rdCVnetObject$nonZeroRep)[color_dot == "black"]
    fitness <- t(rdCVnetObject$fitnessRep)[color_dot == "black"]
  }
  ##############################################################
  if (option == "quantile") {
    cum_varTable <- rdCVnetObject$cum_varTable
    varTable <- rdCVnetObject$varTable
    minmidmax_quantile <- quantile(num_of_variables,
                                   c(quantile, 0.5, 1 - quantile))
    minlimit_quantile <-
      floor(minmidmax_quantile[1])  ### take the floor value in case no value is selected
    midlimit_quantile <- floor(minmidmax_quantile[2])  ###
    maxlimit_quantile <- floor(minmidmax_quantile[3])
    ##min limit: take less
    for (s in 1:length(cum_varTable)) {
      ## set safeguard argument in case there are 0 values
      if (s != 1) {
        if (minlimit_quantile < cum_varTable[s]) {
          minlimit_num_quantile <- as.numeric(names(cum_varTable)[1:s - 1])
          minlimit_num_quantile <-
            minlimit_num_quantile[!is.na(minlimit_num_quantile)]
          break
        } else if (minlimit_quantile == cum_varTable[s] &
                   s == length(cum_varTable)) {
          minlimit_num_quantile <- as.numeric(names(cum_varTable)[1:s])
          minlimit_num_quantile <-
            minlimit_num_quantile[!is.na(minlimit_num_quantile)]
          break
        }
      } else{
        if (minlimit_quantile < cum_varTable[s]) {
          minlimit_num_quantile <- as.numeric(names(cum_varTable)[1])
          minlimit_num_quantile <-
            minlimit_num_quantile[!is.na(minlimit_num_quantile)]
          break
        }
      }
    }

    ##min limit: take less
    for (s in 1:length(cum_varTable)) {
      ## set safeguard argument in case there are 0 values
      if (s != 1) {
        if (midlimit_quantile < cum_varTable[s]) {
          midlimit_num_quantile <- as.numeric(names(cum_varTable)[1:s - 1])
          midlimit_num_quantile <-
            midlimit_num_quantile[!is.na(midlimit_num_quantile)]
          break
        } else if (midlimit_quantile == cum_varTable[s] &
                   s == length(cum_varTable)) {
          midlimit_num_quantile <- as.numeric(names(cum_varTable)[1:s])
          midlimit_num_quantile <-
            midlimit_num_quantile[!is.na(midlimit_num_quantile)]
          break
        }
      } else{
        if (midlimit_quantile < cum_varTable[s]) {
          midlimit_num_quantile <- as.numeric(names(cum_varTable)[1])
          midlimit_num_quantile <-
            midlimit_num_quantile[!is.na(midlimit_num_quantile)]
          break
        }
      }
    }

    ##min limit: take less
    for (s in 1:length(cum_varTable)) {
      ## set safeguard argument in case there are 0 values
      if (s != 1) {
        if (maxlimit_quantile < cum_varTable[s]) {
          maxlimit_num_quantile <- as.numeric(names(cum_varTable)[1:s - 1])
          maxlimit_num_quantile <-
            maxlimit_num_quantile[!is.na(maxlimit_num_quantile)]
          break
        } else if (maxlimit_quantile == cum_varTable[s] &
                   s == length(cum_varTable)) {
          maxlimit_num_quantile <- as.numeric(names(cum_varTable)[1:s])
          maxlimit_num_quantile <-
            maxlimit_num_quantile[!is.na(maxlimit_num_quantile)]
          break
        }
      } else{
        if (maxlimit_quantile < cum_varTable[s]) {
          maxlimit_num_quantile <- as.numeric(names(cum_varTable)[1])
          maxlimit_num_quantile <-
            maxlimit_num_quantile[!is.na(maxlimit_num_quantile)]
          break
        }
      }
    }

    minnames_quantile <- vector()
    midnames_quantile <- vector()
    maxnames_quantile <- vector()

    for (s in 1:length(varTable)) {
      if (varTable[s] %in% minlimit_num_quantile) {
        minnames_quantile <- c(minnames_quantile, names(varTable)[s])
      }
      if (varTable[s] %in% midlimit_num_quantile) {
        midnames_quantile <- c(midnames_quantile, names(varTable)[s])
      }
      if (varTable[s] %in% maxlimit_num_quantile) {
        maxnames_quantile <- c(maxnames_quantile, names(varTable)[s])
      }
    }

    #  nVar<- c(minlimit_quantile,midlimit_quantile,maxlimit_quantile)
    nVar <- c(
      length(minnames_quantile),
      length(midnames_quantile),
      length(maxnames_quantile)
    )
    Var <- list(min = minnames_quantile,
                mid = midnames_quantile,
                max = maxnames_quantile)

    names(nVar) <- c("Qmin", "Qmid", "Qmax")

  } else if (option == "fitness") {
    cum_varTable <- rdCVnetObject$cum_varTable
    varTable <- rdCVnetObject$varTable
    nonZeroRep_vector <- c(num_of_variables)
    fitnessRep_vector <- c(fitness)
    nonZeroRep_vector_grid <-
      seq(min(nonZeroRep_vector), max(nonZeroRep_vector), 1)

    if (fit_curve == "loess") {
      fit_temp <-
        loess(fitnessRep_vector ~ nonZeroRep_vector,
              span = span,
              degree = 2)
      predict_temp <- predict(fit_temp,
                              newdata = data.frame(nonZeroRep_vector = nonZeroRep_vector_grid))
      rdCVnetObject$span <- span

    } else if (fit_curve == "gam") {
      dataframe <- as.data.frame(cbind(nonZeroRep_vector, fitnessRep_vector))
      dataframe_forpredict <-
        data.frame(nonZeroRep_vector = nonZeroRep_vector_grid,
                   fitnessRep_vector = rep(0, length(nonZeroRep_vector_grid)))
      fit_temp <- mgcv::gam(fitnessRep_vector ~
                              s(nonZeroRep_vector, bs = 'ps'),
                            # knots=5,
                            data =    dataframe)
      predict_temp <- predict.gam(fit_temp, dataframe_forpredict)
      rdCVnetObject$k <- k
    }
    rdCVnetObject$fit_curve <- fit_curve
    #fit_temp<-lm(fitnessRep_vector ~ bs(nonZeroRep_vector,
    #                          df=3,  ### when intercept is false degree of freedom = df-degree  df must >=3,  df = length(knots) + degree
    #                          degree=3))
    #predict_temp<-predict(fit_temp,
    #        newdata = list(nonZeroRep_vector=seq(min(nonZeroRep_vector),
    #                                           max(nonZeroRep_vector),
    #                                           1)))


    scaled_predict_temp <-
      (predict_temp - min(predict_temp)) / abs(diff(range(predict_temp)))
    maxIndex_smoothcurve <-
      max(which(scaled_predict_temp <= robust))
    minIndex_smoothcurve <-
      min(which(scaled_predict_temp <= robust))
    varMin_smoothcurve <-
      nonZeroRep_vector_grid[minIndex_smoothcurve]
    varMax_smoothcurve <-
      nonZeroRep_vector_grid[maxIndex_smoothcurve]
    varMid_smoothcurve <-
      round(exp(mean(log(
        c(nonZeroRep_vector_grid[minIndex_smoothcurve], nonZeroRep_vector_grid[maxIndex_smoothcurve])
      )))) # Geometric mean of min and max. This one has decimals



    ##min limit: take less
    for (s in 1:length(cum_varTable)) {
      ## set safeguard argument in case there are 0 values
      if (s != 1) {
        if (varMin_smoothcurve < cum_varTable[s]) {
          varMin_num_smoothcurve <- as.numeric(names(cum_varTable)[1:s - 1])
          varMin_num_smoothcurve <-
            varMin_num_smoothcurve[!is.na(varMin_num_smoothcurve)]
          break
        } else if (varMin_smoothcurve == cum_varTable[s] &
                   s == length(cum_varTable)) {
          varMin_num_smoothcurve <- as.numeric(names(cum_varTable)[1:s])
          varMin_num_smoothcurve <-
            varMin_num_smoothcurve[!is.na(varMin_num_smoothcurve)]
          break
        }
      } else{
        if (varMin_smoothcurve < cum_varTable[s]) {
          varMin_num_smoothcurve <- as.numeric(names(cum_varTable)[1])
          varMin_num_smoothcurve <-
            varMin_num_smoothcurve[!is.na(varMin_num_smoothcurve)]
          break
        }
      }
    }

    ##mid limit: take less
    for (s in 1:length(cum_varTable)) {
      ## set safeguard argument in case there are 0 values
      if (s != 1) {
        if (varMid_smoothcurve < cum_varTable[s]) {
          varMid_num_smoothcurve <- as.numeric(names(cum_varTable)[1:s - 1])
          varMid_num_smoothcurve <-
            varMid_num_smoothcurve[!is.na(varMid_num_smoothcurve)]
          break
        } else if (varMid_smoothcurve == cum_varTable[s] &
                   s == length(cum_varTable)) {
          varMid_num_smoothcurve <- as.numeric(names(cum_varTable)[1:s])
          varMid_num_smoothcurve <-
            varMid_num_smoothcurve[!is.na(varMid_num_smoothcurve)]
          break
        }
      } else{
        if (varMid_smoothcurve < cum_varTable[s]) {
          varMid_num_smoothcurve <- as.numeric(names(cum_varTable)[1])
          varMid_num_smoothcurve <-
            varMid_num_smoothcurve[!is.na(varMid_num_smoothcurve)]
          break
        }
      }
    }

    ##max limit: take less
    for (s in 1:length(cum_varTable)) {
      ## set safeguard argument in case there are 0 values
      if (s != 1) {
        if (varMax_smoothcurve < cum_varTable[s]) {
          varMax_num_smoothcurve <- as.numeric(names(cum_varTable)[1:s - 1])
          varMax_num_smoothcurve <-
            varMax_num_smoothcurve[!is.na(varMax_num_smoothcurve)]
          break
        } else if (varMax_smoothcurve == cum_varTable[s] &
                   s == length(cum_varTable)) {
          varMax_num_smoothcurve <- as.numeric(names(cum_varTable)[1:s])
          varMax_num_smoothcurve <-
            varMax_num_smoothcurve[!is.na(varMax_num_smoothcurve)]
          break
        }
      } else{
        if (varMax_smoothcurve < cum_varTable[s]) {
          varMax_num_smoothcurve <- as.numeric(names(cum_varTable)[1])
          varMax_num_smoothcurve <-
            varMax_num_smoothcurve[!is.na(varMax_num_smoothcurve)]
          break
        }
      }
    }

    minnames_smoothcurve <- vector()
    midnames_smoothcurve <- vector()
    maxnames_smoothcurve <- vector()

    for (s in 1:length(varTable)) {
      if (varTable[s] %in% varMin_num_smoothcurve) {
        minnames_smoothcurve <- c(minnames_smoothcurve, names(varTable)[s])
      }
      if (varTable[s] %in% varMid_num_smoothcurve) {
        midnames_smoothcurve <- c(midnames_smoothcurve, names(varTable)[s])
      }
      if (varTable[s] %in% varMax_num_smoothcurve) {
        maxnames_smoothcurve <- c(maxnames_smoothcurve, names(varTable)[s])
      }
    }

    #nVar<- c(varMin_smoothcurve,varMid_smoothcurve,varMax_smoothcurve)
    nVar <- c(
      length(minnames_smoothcurve),
      length(midnames_smoothcurve),
      length(maxnames_smoothcurve)
    )
    Var <- list(min = minnames_smoothcurve,
                mid = midnames_smoothcurve,
                max = maxnames_smoothcurve)

    names(nVar) <- c("min", "mid", "max")

  } else{
    stop("\n There is no such option")
  }

  rdCVnetObject$Var <- Var
  rdCVnetObject$nVar <- nVar
  rdCVnetObject$outlier_info <- color_dot
  return(rdCVnetObject)
}
