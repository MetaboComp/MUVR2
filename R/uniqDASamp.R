#' Stratified sampling for classification and unique individuals
#' Even if it is stratified sampling, it may lead to some fold have more than 1 samples than others
#' @param Y Classes (Std Y variable)
#' @param ID Individual identifier (unique)
#' @param folds Number of folds
#' @return A master list of segments
#' @export
#' @examples
#' Y <- rep(LETTERS[1:2],10)
#' ID <- 1:20
#' folds <- 5
#' uniqDASamp(Y, ID, folds)
uniqDASamp <- function(Y,
                       ID,
                       folds) {
  Ynames <- sort(unique(Y))  # Find classes
  groups <- length(Ynames)   ## classes names
  groupList <- list()
  for (grp in 1:groups) {
    groupID <- ID[Y == Ynames[grp]]  # Find indices per group
    groupList[[grp]] <-
      vectSamp(groupID, n = folds)  # Draw random samples within group, smallest length in the beginning
  }
  masterList <-
    groupList[[1]] # Add 1st class samplingintofolds to 'Master' sample of all groups
  for (grp in 2:groups) {
    # Add subsequent groups
    masterList <- masterList[order(-sapply(masterList,
                                           length))]  ## the ones has the largest length goes to the beginning of list
    for (segment in 1:folds) {
      masterList[[segment]] <- sort(c(masterList[[segment]],
                                      groupList[[grp]][[segment]])) ## add the following class samplingintofolds
      ## sort them in the correct order

      ##note that grouplist has smallest lengthin the beginning and masterlist has the largest length in the beginning
    }
  }
  return(masterList)
}
