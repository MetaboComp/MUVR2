#' Calculate permutation p-value of actual model performance vs null hypothesis distribution
#' `pPerm` will calculate the cumulative (1-tailed) probability of `actual` belonging to `permutation_distribution`.
#' Side is guessed by actual value compared to median(permutation_distribution).
#' Test is performed on original data OR ranked for non-parametric statistics.
#' @param actual Actual model performance (e.g. misclassifications or Q2)
#' @param permutation_distribution Null hypothesis distribution from permutation test (same metric as `actual`)
#' @param side Smaller or greater than (automatically guessed if omitted) (Q2 and AUC is a "greater than" test, whereas misclassifications is "smaller than")
#' @param type one of ('t','non',"smooth","ecdf","rank")
#' @param extend extend how much it extend
#' @return p-value
#' @export
#' @examples
#' \donttest{
#' data("freelive2")
#' actual <- sample(YR2, 1)
#' permutation_distribution <- YR2
#' pPerm(actual, permutation_distribution)
#' }
pPerm <- function(actual,
                  ###a value
                  permutation_distribution,
                  ###a distribution
                  side = c('smaller', 'greater'),
                  type = "t",
                  #               type=c('t','non',"smooth","ecdf","rank"),
                  extend = 0.1) {
  p_object <- list()
  ##########################################################################################################################
  #it needs to be take into consideration when the type of side is error
  if (is.numeric(actual) == FALSE) {
    stop("actual needs to be numeric")
  }
  if (is.numeric(permutation_distribution) == FALSE) {
    stop("permutation_distribution needs to be a numeric distribution")
  }
  if (length(permutation_distribution) < 5) {
    stop("permutation_distribution has too few values to form a distribution")
  }
  if (!missing(type)) {
    if (!any(type %in% c('t', 'non', "smooth", "ecdf", "rank"))) {
      stop("This type can not be implemented")
    }
  }
  if (missing(type)) {
    type <- 't'
  }     ###Student's t distribution
  if (!missing(side)) {
    if (side != "smaller" & side != "greater") {
      stop("This side can not be implemented")
    }
  }
  if (missing(side)) {
    side <- ifelse(actual < median(permutation_distribution),
                   'smaller',
                   'greater')
  }   ###This is to see which side

  ######################################################################################################################################################################################################

  if (type == 'ecdf') {
    if (side == "smaller") {
      if (length(table(permutation_distribution)) == 1 &
          as.numeric(names(table(permutation_distribution))[1]) == actual) {
        p_actual <- 1
        p <- p_actual
      } else{
        sort_x <- sort(permutation_distribution)
        fun_ecdf <- ecdf(sort_x)
        ecdf_curve <- fun_ecdf(sort_x)

        if (actual < min(sort_x)) {
          p_actual <- 1 / (length(permutation_distribution))
        } else{
          for (i in 1:(length(permutation_distribution) - 1)) {
            if (actual > sort_x[i] & actual <= sort_x[i + 1]) {
              p_actual <- ecdf_curve[i] + 1 / (length(permutation_distribution))
            }

          }
          if (p_actual == 0) {
            p_actual <- 1 / (length(permutation_distribution))
          }
        }
        p <- p_actual
      }
    } else{
      if (length(table(permutation_distribution)) == 1 &
          as.numeric(names(table(permutation_distribution))[1]) == actual) {
        p_actual <- 1
        p <- p_actual
      } else{
        sort_x <- sort(permutation_distribution)
        fun_ecdf <- ecdf(sort_x)
        ecdf_curve <- fun_ecdf(sort_x)


        if (actual > max(sort_x)) {
          p_actual <- 1 / (length(permutation_distribution))
        } else{
          for (i in 1:(length(permutation_distribution) - 1)) {
            if (actual >= sort_x[i] & actual < sort_x[i + 1]) {
              p_actual <- 1 - ecdf_curve[i] + 1 / (length(permutation_distribution))
            }
          }
          if (p_actual == 0) {
            p_actual <- 1 / (length(permutation_distribution))
          }
        }
        p <- p_actual
      }
    }
    p_object$p <- p
    p_object$points <- NULL
  }



  ######################################################################################################################
  ## redo p value from cumulative curve
  if (type == 'non') {
    if (side == "smaller") {
      rank <-
        rank(c(actual, permutation_distribution))     ###the sequence of each value
      actual <-
        rank[1]              ###actual is not the smallest if side is greater this apply to Q2 and AUC
      permutation_distribution <- rank[-1]
    } else{
      rank <- rank(c(actual, permutation_distribution))
      actual <- rank[(length(permutation_distribution))]
      permutation_distribution <-
        rank[-(length(permutation_distribution))]
    }

    p <-
      pt((actual - mean(permutation_distribution)) / sd(permutation_distribution),
         ##### pt gives the probability before the input point
         # p=(actual-mean(permutation_distribution))/sd(permutation_distribution)
         df = length(permutation_distribution) - 1
      )

    if (side == 'greater') {
      p <- 1 - p
    }
    p_object$p <- p
    p_object$points <- NULL
  }

  ######################################################################################################################
  if (type == 'rank') {
    if (side == "smaller") {
      rank <-
        rank(c(actual, permutation_distribution))     ###the sequence of each value
      p_actual <-
        rank[1] / (length(rank) - 1)              ###actual is not the smallest if side is greater this apply to Q2 and AUC
      p_permutation_distribution <- rank[-1]
      if (length(table(permutation_distribution)) == 1 &
          as.numeric(names(table(permutation_distribution))[1]) == actual) {
        p_actual <- 1
      }
    } else{
      rank <- rank(-c(actual, permutation_distribution))
      #p_actual<-1/(length(rank)-1)+1-(rank[1]/(length(rank)-1))
      p_actual <- rank[1] / (length(rank) - 1)
      p_permutation_distribution <- rank[-1]
      if (length(table(permutation_distribution)) == 1 &
          as.numeric(names(table(permutation_distribution))[1]) == actual) {
        p_actual <- 1
      }
    }

    p <- p_actual
    cat("/n Resolution limited to ", 1 / (length(rank) - 1))
    p_object$p <- p
    p_object$points <- NULL

  }

  ###############################################################################################################################################################################################

  if (type == "t") {
    p <-
      pt((actual - mean(permutation_distribution)) / sd(permutation_distribution),
         ##### pt gives the probability before the input point

         df = length(permutation_distribution) - 1
      )

    if (side == 'greater') {
      p <- 1 - p
    }    ####this is to solve which side p value is

    p_object$p <- p
    p_object$points <- NULL
  }


  if (type == "smooth") {
    # e = extend * diff(range(permutation_distribution))  ##c permutation distributioon actual
    #  if(actual>=max(permutation_distribution)){    ##

    #  from=min(permutation_distribution)-(actual-max(permutation_distribution))-e
    # to=actual+e
    #  }else if(actual<=min(permutation_distribution)){
    #    to=min(permutation_distribution)-actual+max(permutation_distribution)+e
    #    from=actual-e
    #  } else {
    #    from=min(permutation_distribution)
    #    to=max(permutation_distribution)
    #  }
    ran <- range(c(actual, permutation_distribution))
    from <-
      ran[1] - diff(ran) * extend  ## actual is always include in the from to
    to <- ran[2] + diff(ran) * extend

    dens <-
      density(
        permutation_distribution,
        ## get the density and value information of thi distribtion
        #adjust=0.01,    ##do hist(dens$x, dens$y). This is not wrong
        from = from,
        to = to,
        n = 100000
      )
    densy <- dens$y
    for (i in 1:length(dens$y)) {
      if (dens$y[i] == 0) {
        densy[i] <- min(dens$y[dens$y != 0]) * 1
      }
    }

    ## This value range is slightly broader than the original range
    #  value<-sample(x = dens$x,   ##This is where p value is calculate from. The value it should be saved
    #        prob = densy,
    #     size=100000000,
    #         #size = length(permutation_distribution),
    #         replace=TRUE
    #  )
    #if(max(value)>=)
    if (side == "smaller") {
      if (length(table(permutation_distribution)) == 1 &
          as.numeric(names(table(permutation_distribution))[1]) == actual) {
        p_actual <- 1
        p <- p_actual
      } else{
        sort_x <- sort(dens$x)
        bd <- (max(dens$x) - min(dens$x)) / length(dens$x)
        # sort_x<-sort(value)   ### The thing is if I use "value", the extreme max and min value cannot be selected because the chanceis low
        #fun_ecdf<-ecdf(sort_x)
        #ecdf_curve <- fun_ecdf(sort_x)
        #if(actual<=sort_x[1]){
        #  p_actual<-ecdf_curve[1]
        #}

        for (i in 1:(length(sort_x) - 1)) {
          if (actual > sort_x[i] & actual <= sort_x[i + 1]) {
            #############################################################################################################################

            p_actual <- bd * sum(dens$y[1:i])
            #############################################################################################################################

            #p_actual<-ecdf_curve[i]
          }
        }


        p <- p_actual
      }
    } else{
      if (length(table(permutation_distribution)) == 1 &
          as.numeric(names(table(permutation_distribution))[1]) == actual) {
        p_actual <- 1
        p <- p_actual
      } else{
        sort_x <- sort(dens$x)
        bd <- (max(dens$x) - min(dens$x)) / length(dens$x)
        #sort_x<-sort(value)
        #fun_ecdf<-ecdf(sort_x)
        #ecdf_curve <- fun_ecdf(sort_x)
        #if(actual>=sort_x[length(sort_x)]){    ## not possible to happen
        #  p_actual<-1-ecdf_curve[length(sort_x)]
        #}

        for (i in 1:(length(sort_x) - 1)) {
          if (actual >= sort_x[i] & actual < sort_x[i + 1]) {
            #############################################################################################################################
            #p_actual=1-bd*sum(dens$y[1:i])          ### do not use pseudocount
            p_actual <-
              bd * sum(dens$y[(i + 1):length(dens$y)])   ####### this side
            #############################################################################################################################
            #p_actual<- 1-ecdf_curve[i]
          }
        }


        p <- p_actual
      }
    }

    p_object$p <- p
    #p_object$points<-value
    p_object$dens <- dens
  }


  return(p_object)
}
