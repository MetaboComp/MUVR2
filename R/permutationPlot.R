#' Plot permutation analysis using actual model and permutations()
#' This is basically a wrapper for the MUVR::plotPerm() function using model objects to make coding nicer and cleaner
#' @param MUVRclassObject A 'MUVR' class object
#' @param permutation_result  A permutation result. It is a list of 1 items:  permutation_output
#' @param model 'Min', 'Mid', or 'Max'
#' @param type 't' (default; for Student's t) or 'non' for "non-parametric" (i.e. rank) studen'ts
#' @param side 'smaller' for actual lower than H0 or 'greater' for actual larger than H0 (automatically selected if not specified)
#' @param pos which side of actual to put p-value on
#' @param xlab optional xlabel
#' @param xlim optional x-range
#' @param ylim otional y-range
#' @param breaks optional custom histogram breaks (defaults to 'sturges')
#' @param main optional plot title (or TRUE for autoname)
#' @return A permutation plot
#' @export
##library(MUVR)
##library(doParallel)
## nCore=detectCores()-1
## cl=makeCluster(nCore)
## registerDoParallel(cl)
## nRep=2*nCore
## varRatio=.75
## nOuter=6
## nPerm=50
## R12ML=MUVR(X=mlr12,ML=TRUE,nRep=nRep,nOuter=nOuter,varRatio=varRatio,method='RF')
## permR12=permutations(R12ML)
## stopCluster(cl)
## permutationPlot(R12ML,permR12)
##
permutationPlot <- function(MUVRclassObject,
                            permutation_result,
                            ####For AUROC, the result is a permutation list. The length of the list is the group number
                            model = 'Mid',
                            type = "t",
                            #c('t','non'),  ##'t' (default; for Student's t) or 'non' for "non-parametric" (i.e. rank) studen'ts
                            side = c("greater", "smaller"),  ##smaller for actual lower than H0 or greater for actual larger than H0, automatically selected if not specified
                            pos,
                            ##which side of actual to put p-value on
                            xlab = NULL,
                            xlim,
                            ylim = NULL,
                            breaks = 'Sturges',
                            ###optional custom histogram breaks (defaults to 'sturges')
                            main = NULL) {
  ###################################################################################################################
  if (!any(class(MUVRclassObject) == 'MUVR')) {
    stop("The input is not a MUVR class Object")
  }

  permutation_type <- MUVRclassObject$inData$fitness
  permutation_output <- permutation_result

  if (!missing(permutation_type)) {
    if (permutation_type != "AUROC" &
        permutation_type != "MISS" &
        permutation_type != "Q2" & permutation_type != "BER")
    {
      stop("permutation_type is not correct")
    }
    if (permutation_type == "Q2" &
        !any(class(MUVRclassObject) %in% c('Regression', "Multilevel")))
    {
      stop("Classification and Multilevel must use AUROC or MISS for permutation")
    }

    if (!(permutation_type %in% c("Q2")) &
        any(class(MUVRclassObject) == 'Regression'))
    {
      stop("Regression must use Q2 for permutation")
    }

  }

  if (!missing(model)) {
    if (!model %in% c('Min', 'min', "Mid", "mid", "Max", "max"))
      stop("There is only min, mid, max model ")
  }

  nModel <- ifelse(model %in% c('Min', 'min'),
                   1,
                   ifelse(model %in% c('Mid', 'mid'),
                          2,
                          ifelse(model %in% c('Max', 'max'), 3, stop(
                            "This model is not an option"
                          ))))

  # if(!missing(type)){if(type!="t"&type!="non")stop("This type can not be implemented")}
  if (missing(type)) {
    type <- 't'
  }


  ###############################################################################################################################33
  ###For actual value or vector in 3 scenario
  if (class(MUVRclassObject)[3] == 'rdCVnet') {
    if (permutation_type == "Q2") {
      actual <- MUVRclassObject$fitMetric$Q2
      if (missing(xlab)) {
        xlab <- 'Q2'
      }
    } else if (permutation_type == "MISS") {
      actual <- MUVRclassObject$miss
      if (missing(xlab)) {
        xlab <- 'Misclassifications'
      }
    } else if (permutation_type == "AUROC")
    {
      if (dim(permutation_output)[2] != 1) {
        actual <- MUVRclassObject$auc
      } else{
        actual <- MUVRclassObject$auc

      }    ###a vector
      if (missing(xlab)) {
        xlab <- 'AUROC'
      }
    }
    else {
      actual <- MUVRclassObject$ber
      if (missing(xlab)) {
        xlab <- 'Balance Error Rate'
      }
    }


  } else{
    if (permutation_type == "Q2") {
      actual <- MUVRclassObject$fitMetric$Q2[nModel]
      if (missing(xlab)) {
        xlab <- 'Q2'
      }
    } else if (permutation_type == "MISS") {
      actual <- MUVRclassObject$miss[nModel]
      if (missing(xlab)) {
        xlab <- 'Misclassifications'
      }
    } else if (permutation_type == "AUROC")
    {
      actual <- ifelse(dim(permutation_output)[3] != 1,
                       MUVRclassObject$auc[nModel,],
                       MUVRclassObject$auc[nModel])    ###a vector
      if (missing(xlab)) {
        xlab <- 'AUROC'
      }
    }
    else {
      actual <- MUVRclassObject$ber[nModel]
      if (missing(xlab)) {
        xlab <- 'Balance Error Rate'
      }
    }
  }
  #########################################################################################################################
  ########when it is Q2 or MISS
  if (permutation_type == "Q2" |
      permutation_type == "MISS" | permutation_type == "BER") {
    if (class(MUVRclassObject)[3] == 'rdCVnet') {
      if (!missing(side)) {
        if (side != "smaller" &
            side != "greater") {
          stop("This side can not be implemented")
        }
      }
      if (!missing(side)) {
        if (side != ifelse(actual < median(permutation_output),
                           'smaller',
                           'greater')) {
          side <- ifelse(actual < median(permutation_output),
                         'smaller',
                         'greater')
        }
      }
      if (missing(side)) {
        side <-
          ifelse(actual < median(as.vector(as.matrix(
            permutation_output
          ))),
          'smaller',
          'greater')
      }

      if (missing(pos)) {
        pos <- ifelse(side == 'smaller', 4, 2)
      }

      h0 = as.vector(as.matrix(permutation_output))

      if (missing(xlim)) {
        if (side == 'smaller') {
          xlim <- c(0, max(h0))
        }
        else {
          xlim <- c(min(h0), 1)
        }
      }

      #####for miss classification and BER, it is smaller, for greater it is Q2 or AUC the biggest value is 1

      if (is.null(main)) {
        main <- paste('Permutation analysis of',
                      deparse(substitute(MUVRclassObject)),
                      permutation_type)
      }

      plotPerm(
        actual = actual,
        distribution = h0,
        type = type,
        pos = pos,
        side = side,
        xlab = xlab,
        xlim = xlim,
        ylim = ylim,
        breaks = breaks,
        main = main
      )



    } else{
      if (!missing(side)) {
        if (side != "smaller" &
            side != "greater") {
          stop("This side can not be implemented")
        }
      }
      if (!missing(side)) {
        if (side != ifelse(actual < median(permutation_output[, nModel]),
                           'smaller',
                           'greater')) {
          side <- ifelse(actual < median(permutation_output[, nModel]),
                         'smaller',
                         'greater')
        }
      }
      if (missing(side)) {
        side <- ifelse(actual < median(permutation_output[, nModel]),
                       'smaller',
                       'greater')
      }

      if (missing(pos)) {
        pos = ifelse(side == 'smaller', 4, 2)
      }

      h0 <- permutation_output[, nModel]

      if (missing(xlim)) {
        if (side == 'smaller') {
          xlim <- c(0, max(h0))
        }
        else{
          xlim <- c(min(h0), 1)
        }
      }

      #####for miss classification and BER, it is smaller, for greater it is Q2 or AUC the biggest value is 1

      if (is.null(main)) {
        main <- paste('Permutation analysis of',
                      deparse(substitute(MUVRclassObject)),
                      permutation_type)
      }

      plotPerm(
        actual = actual,
        distribution = h0,
        type = type,
        pos = pos,
        side = side,
        xlab = xlab,
        xlim = xlim,
        ylim = ylim,
        breaks = breaks,
        main = main
      )
    }

  }
  ############################################################################################################
  #######When it is AUROC
  if (permutation_type == "AUROC") {
    if (class(MUVRclassObject)[3] == 'rdCVnet') {
      for (s in 1:dim(permutation_output)[2]) {
        if (!missing(side)) {
          if (side != "smaller" &
              side != "greater") {
            stop("This side can not be implemented")
          }
        }
        if (!missing(side)) {
          if (side != ifelse(actual[s] < median(permutation_output[, s]),
                             'smaller',
                             'greater')) {
            side <- ifelse(actual[s] < median(permutation_output[, s]),
                           'smaller',
                           'greater')
          }
        }
        if (missing(side)) {
          side <- ifelse(actual[s] < median(permutation_output[, s]),
                         'smaller',
                         'greater')
        }

        if (missing(pos)) {
          pos <- ifelse(side == 'smaller', 4, 2)
        }
        h0 <- matrix(
          0L,
          nrow = dim(permutation_output)[1],
          ncol = dim(permutation_output)[2]
        )
        h0[, s] <-
          permutation_output[, s]   ####   h0 row is permutation, column is group, They are all under nModel

        if (missing(xlim)) {
          xlim <- list()
          if (side == 'smaller') {
            xlim <- c(0, max(h0[, s]))
          }
          else {
            xlim <- c(min(h0[, s]), 1)
          }
        }

        #####for miss classification, it is smaller, for greater it is Q2 or AUC the biggest value is 1



        ###if main=NULL, isTRUE(main) is False

        main <- paste(
          'Permutation analysis of',
          deparse(substitute(MUVRclassObject)),
          permutation_type,
          "group",
          s
        )

        plotPerm(
          actual = actual[s],
          distribution = h0[, s],
          type = type,
          pos = pos,
          side = side,
          xlab = xlab,
          xlim = xlim,
          ylim = ylim,
          breaks = breaks,
          main = main
        )

      }


    } else{
      for (s in 1:dim(permutation_output)[3]) {
        if (!missing(side)) {
          if (side != "smaller" &
              side != "greater") {
            stop("This side can not be implemented")
          }
        }
        if (!missing(side)) {
          if (side != ifelse(actual[s] < median(permutation_output[, nModel, s]),
                             'smaller',
                             'greater')) {
            side <- ifelse(actual[s] < median(permutation_output[, nModel, s]),
                           'smaller',
                           'greater')
          }
        }
        if (missing(side)) {
          side <- ifelse(actual[s] < median(permutation_output[, nModel, s]),
                         'smaller',
                         'greater')
        }

        if (missing(pos)) {
          pos <- ifelse(side == 'smaller', 4, 2)
        }
        h0 <- matrix(
          0L,
          nrow = dim(permutation_output)[1],
          ncol = dim(permutation_output)[3]
        )
        h0[, s] <-
          permutation_output[, nModel, s]   ####   h0 row is permutation, column is group, They are all under nModel

        if (missing(xlim)) {
          xlim <- list()
          if (side == 'smaller') {
            xlim <- c(0, max(h0[, s]))
          }
          else {
            xlim <- c(min(h0[, s]), 1)
          }
        }

        #####for miss classification, it is smaller, for greater it is Q2 or AUC the biggest value is 1



        ###if main=NULL, isTRUE(main) is False

        main <- paste(
          'Permutation analysis of',
          deparse(substitute(MUVRclassObject)),
          permutation_type,
          "group",
          s
        )

        plotPerm(
          actual = actual[s],
          distribution = h0[, s],
          type = type,
          pos = pos,
          side = side,
          xlab = xlab,
          xlim = xlim,
          ylim = ylim,
          breaks = breaks,
          main = main
        )
      }




    }

  }

}
