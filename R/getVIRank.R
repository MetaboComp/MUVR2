#' Extract autoselected variables from MUVR model object
#' @param MUVRclassObject An object of MUVR class
#' @param model Which model to use ("min", "mid" (default), or "max")
#' @param n customize values
#' @param all logical, to get the ranks of all variable or not
#'
#' @return Data frame with order, name and average rank of variables (`order`, `name` & `rank`)
#' @export
#' @examples
#' \donttest{
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
#' getVIRank(regrModel, model="min")
#' }
getVIRank <- function(MUVRclassObject,
                      model = 'mid',
                      n,
                      all = FALSE) {
  nMod <- ifelse(model == 'min',
                 1,
                 ifelse(model == 'mid', 2, 3))

  if (class(MUVRclassObject)[3] == "rdCVnet") {
    VIRanks_vector <- rank(-as.vector(MUVRclassObject$varTable))
    names(VIRanks_vector) <- names(MUVRclassObject$varTable)
    if (!is.null(MUVRclassObject$keep)) {
      VIRanks_vector <- VIRanks_vector - 4
      VIRanks_vector[MUVRclassObject$keep] <- 0
    }
    if (all == TRUE) {
      n <- ncol(MUVRclassObject$inData$X)
    }
    if (!missing(n)) {
      if (n > ncol(MUVRclassObject$inData$X)) {
        stop("n bigger than total number of variables")
      }
      VIRanks <- data.frame(
        order = 1:n,
        name = names(MUVRclassObject$varTable)[1:n],
        rank = VIRanks_vector[1:n]
      )
    } else{
      VIRanks <- data.frame(
        order = 1:length(MUVRclassObject$Var[[nMod]]),
        name = names(MUVRclassObject$varTable)[1:length(MUVRclassObject$Var[[nMod]])],
        rank = VIRanks_vector[1:length(MUVRclassObject$Var[[nMod]])]
      )
    }
    VIRanks$name <- as.character(VIRanks$name)
    rownames(VIRanks) <- VIRanks$name

  } else{
    if (all == TRUE) {
      n <- ncol(MUVRclassObject$inData$X)
    }
    if (!missing(n)) {
      if (n > ncol(MUVRclassObject$inData$X)) {
        stop("n bigger than total number of variables")
      }
      nVar <- n
      VIRanks <-
        sort(MUVRclassObject$VIRank[, nMod])[1:nVar]       ###sequencing them and take the first few of them
      ###sort() do not return rank but the true value
      ##however it sort() rank, so the result is still VIRanks but sequenced from small to big
      VIRanks <- data.frame(order = 1:nVar,
                            name = names(VIRanks),
                            rank = VIRanks)
      VIRanks$name <- as.character(VIRanks$name)
    } else{
      nVar <- round(MUVRclassObject$nVar[nMod])
      VIRanks <-
        sort(MUVRclassObject$VIRank[, nMod])[1:nVar]       ###sequencing them and take the first few of them
      ###sort() do not return rank but the true value
      ##however it sort() rank, so the result is still VIRanks but sequenced from small to big
      VIRanks <- data.frame(order = 1:nVar,
                            name = names(VIRanks),
                            rank = VIRanks)
      VIRanks$name <- as.character(VIRanks$name)
    }
  }
  return(VIRanks)
}
