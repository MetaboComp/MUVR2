#' Convert folds in listformat to vectorformat. Specifcy for each ID which group they are in
#' @param foldList fold list from vectSamp or uniqDASamp
#' @param ID Vector of sampling unit identifier
#' @return a vector with folds
#' @examples
#' \donttest{
#' Y <- rep(LETTERS[1:2],10)
#' ID <- 1:20
#' folds <- 5
#' foldList <- MUVR2:::uniqDASamp(Y, ID, folds)
#' foldVect <- foldVector(foldList, ID)
#' foldVect
#' }
#' @noRd
foldVector <- function(foldList, ID) {
  fold <- numeric(length(ID)) ##longer than nfold
  nFold <- length(foldList)  ## number of groups
  for (i in 1:nFold) {
    fold[ID %in% foldList[[i]]] <- i
  }
  return(fold)
}
