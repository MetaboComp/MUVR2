#' Sampling from the distribution of something
#'@param X a vector (numeric or factor) where the distribution/probabality will be generated
#'@param upperlimit if X is numeric, set upper limit
#'@param lowerlimit if X is numeric, set lower limit
#'@param extend If X is numeric, how much you want to extend from the lower and upper existing X.
##If not specified, it is 5% of the range
#'@param n How many you want to sample
#'@export
#'@return a resampled thing

sampling_from_distribution <- function(X,
                                       upperlimit,
                                       lowerlimit,
                                       extend,
                                       n) {
  if (missing(n)) {
    n <- length(X)
  }
  if (is.numeric(X)) {
    if (missing(extend)) {
      extend <- 0.05 * (max(X) - min(X))
    }
    from <- min(X) - extend
    to <- max(X) + extend
    if (missing(upperlimit)) {
      upperlimit <- to
    }
    if (missing(lowerlimit)) {
      lowerlimit <- from
    }
    if (upperlimit - lowerlimit <= 0) {
      stop("Dude, upperlimit should be higher than lower limit")
    }
    if (from < lowerlimit) {
      from <- lowerlimit
    }
    if (to > lowerlimit) {
      to <- upperlimit
    }
    denss <- density(
      x = X,
      from = from,
      to = to,
      n = 10000
    )
    dataPerm <- sample(x = denss$x,
                       prob = denss$y,
                       size = n,)
  }
  if (is.factor(X)) {
    dataPerm <- factor_samesequence(sample(
      levels(factor_samesequence(X)),
      ##when ML is TRUE, DA =FALSE
      size = n,
      replace = TRUE,
      prob = table(factor_samesequence(X)) /
        length(X)
    ))
    dataPerm <- factor(dataPerm, levels = levels(X))
  }

  return(dataPerm)
}
