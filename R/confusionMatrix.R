#' Make a confusion matrix from a MUVR object
#' @param MVObj A MUVR object (classification analysis)
#' @param model min, mid or max model
#' @return A confusion matrix of actual vs predicted class
#' @export
#' @examples
#' nRep <- 2 # Number of MUVR2 repetitions
#' nOuter <- 3 # Number of outer cross-validation segments
#' varRatio <- 0.75 # Proportion of variables kept per iteration
#' method <- 'RF' # Selected core modeling algorithm
#' classModel <- MUVR2(
#'   X = Xotu,
#'   Y = Yotu,
#'   nRep = nRep,
#'   nOuter = nOuter,
#'   varRatio = varRatio,
#'   method = method
#' )
#' confusionMatrix(classModel)
#'
confusionMatrix <- function(MVObj,
                           model = 'mid') {
  if (!any(class(MVObj) == 'Classification')) {
    stop ('The MUVR object needs to be from a classification analysis')
  }
  nMod <- ifelse(model == 'min', 1, ifelse(model == 'mid', 2, 3))
  actual <- MVObj$inData$Y
  predicted <- MVObj$yClass[, nMod]
  confusion_matrix <-
    matrix(table(actual = actual,
                 predicted = predicted))
  return(confusion_matrix)
}
