#' Identify variables with near zero variance
#' Adapted and stripped down from mixOmics v 5.2.0 (https://cran.r-project.org/web/packages/mixOmics/)
#' @param x a numeric vector or matrix, or a data frame with all numeric data.
#' @param freqCut the cutoff for the ratio of the most common value to the second most common value.
#' @param uniqueCut the cutoff for the percentage of distinct values out of the number of total samples.
#' @return nzv object
#' @export
nearZeroVar <- function (x,
                         freqCut = 95 / 5,
                         uniqueCut = 10) {
  if (is.vector(x)) {
    x <- matrix(x, ncol = 1)
  }
  ###To apply the function on each column of the matrix
  freqRatio <- apply(x, 2, function(vectorX) {
    if (length(unique(vectorX)) == length(vectorX)) {
      # No duplicate
      return(1)
    } else if (length(unique(vectorX)) == 1) {
      # Same value
      return(0)
    } else {
      t  <- table(vectorX)
      return(max(t, na.rm = TRUE) / max(t[-which.max(t)], na.rm = TRUE))
      ###number of the most frequent one in the table divide the number of the second frequent one
    }
  })
  ##find the number of unique categories in each column of the matrix: lunique (a vector of ncol length)

  lunique  <- apply(x, 2, function(vectorX)
    length(unique(vectorX)))
  percentUnique  <- 100 * lunique / nrow(x)
  zeroVar  <-
    (lunique == 1) | apply(x, 2, function(vectorX)
      all(is.na(vectorX)))
  ###If all element are same value or if all element are NA, the variable is defined as zero variance

  out  <- list()
  ##the position number of the zero variance column
  out$Position  <-
    which((freqRatio > freqCut & percentUnique <= uniqueCut) | zeroVar)
  names(out$Position)  <- NULL
  ##Out metrics only record frequent ratio and percentage unique of the nzv variables
  out$Metrics  <-
    data.frame(freqRatio = freqRatio, percentUnique = percentUnique)
  out$Metrics  <- out$Metrics[out$Position,]
  return(out)
}
