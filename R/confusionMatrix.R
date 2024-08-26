#' Confusion matrix
#' 
#' Make a confusion matrix from a MUVR object.
#' @param MVObj A MUVR object (classification analysis)
#' @param model min, mid or max model
#' @return A confusion matrix of actual vs predicted class
#' @export
#' @examples
#' \donttest{
#' data("mosquito")
#' data("crisp")
#' nRep <- 2 # Number of MUVR2 repetitions
#' nOuter <- 4 # Number of outer cross-validation segments
#' varRatio <- 0.6 # Proportion of variables kept per iteration
#' classModel <- MUVR2_EN(X = Xotu,
#'                       Y = Yotu,
#'                       nRep = nRep,
#'                       nOuter = nOuter,
#'                       DA = TRUE,
#'                       modReturn = TRUE)
#' confusionMatrix(classModel)
#' MLModel <- MUVR2(X = crispEM,
#'                  ML = TRUE,
#'                  nRep = nRep,
#'                  nOuter = nOuter,
#'                  varRatio = varRatio,
#'                  method = "RF",
#'                  modReturn = TRUE)
#'  confusionMatrix(MLModel)
#' }
confusionMatrix <- function(MVObj,
                           model = 'mid') {
  if (!any(class(MVObj)%in% c('Classification',"Multilevel"))) {
    stop ('The MUVR object needs to be from a classification or multilevel analysis')
  }
  nMod <- ifelse(model == 'min', 1, ifelse(model == 'mid', 2, 3))
  actual <- MVObj$inData$Y
  if(class(MVObj)[3]!="rdCVnet"){
  predicted <- MVObj$yClass[, nMod]
  }else{
    predicted <- factor(MVObj$yClass)

    }
  confusion_matrix <-
    as.matrix(table(actual = actual,
                 predicted = predicted))
  return(confusion_matrix)
}
