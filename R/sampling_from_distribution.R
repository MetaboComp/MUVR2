#' Sampling from the distribution of something
#'@param X a vector (numeric or factor) where the distribution/probabality will be generated
#'@param upperlimit if X is numeric, set upper limit
#'@param lowerlimit if X is numeric, set lower limit
#'@param extend If X is numeric, how much you want to extend from the lower and upper existing X.
##If not specified, it is 5% of the range
#'@param n How many you want to sample
#'@export
#'@return a resampled thing
#' @examples
#' \dontrun{
#' data("mosquito")
#' sampling_from_distribution(Yotu)
#' data("freelive2")
#' sampling_from_distribution(YR2,
#'                            upperlimit=200,
#'                            lowerlimit=0,
#'                            n=length(YR2)
#'                            )
#' }
sampling_from_distribution <- function(X,
                                       upperlimit,
                                       lowerlimit,
                                       extend,
                                       n) {
  if (missing(n)) {
    n <- length(X)
  }
  if (is.numeric(X)) {


    if(!missing(upperlimit)&!missing(extend)){
      warning("The upper boundary is the smallest value between upperlimit and the upper value generated from extend argument")
    }

    if(!missing(lowerlimit)&!missing(extend)){
      warning("The lower boundary is the largest value between lowerlimit and the lower value generated from extend argument")
    }

    if (missing(extend)) {
      extend <- 0.05 * (max(X) - min(X))
    }
    from <- min(X) - extend
    to <- max(X) + extend
    if (missing(lowerlimit)) {
      lowerlimit <- from
    }
    if (missing(upperlimit)) {
      upperlimit <- to
    }
    if (from < lowerlimit) {
      from <- lowerlimit
    }
    if (to > upperlimit) {
      to <- upperlimit
    }
    denss <- density(
      x = X,
      from = from,
      to = to,
      n = 100000
    )
    dataPerm <- sample(x = denss$x,
                       prob = denss$y,
                       size = n,
                       replace=T)
  }
  if (is.factor(X)) {
    dataPerm <- factor_samesequence(
      factor(
      sample(
      levels(factor_samesequence(X)),
      ##when ML is TRUE, DA =FALSE
      size = n,
      replace = TRUE,
      prob = table(factor_samesequence(X)) /length(X)
    )))
    dataPerm <- factor(dataPerm, levels = levels(X))
  }

  return(dataPerm)
}
