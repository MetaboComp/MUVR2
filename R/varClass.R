#' Report variables belonging to different classes
#' Reports names and numbers of variables: all as well as Optimal (min model), redundant (from min up to max) and noisy (the rest)
#' @param MUVRclassObject A MUVR class object
#' @return A list with names and numbers of variables: all as well as Optimal (Corresponding to 'min' or minial-optimal model),
#' Redundant (from 'min' up to 'max' or all-relevant ) and Noisy (the rest)
#' @export
#' @examples
#' \dontrun{
#' data("mosquito")
#' nRep <- 2
#' nOuter <- 4
#' classModel <- MUVR2_EN(X = Xotu,
#'                        Y = Yotu,
#'                        nRep = nRep,
#'                        nOuter = nOuter,
#'                        DA = TRUE,
#'                        modReturn = TRUE)
#' classModel<-getVar(classModel,option="quantile")
#' varClass(classModel)
#' }
#'
############################################################################
#Change MVObject to MUVRclassObject
#
####################################################################
varClass <- function(MUVRclassObject) {
  if(length(MUVRclassObject$nVar))
  nVarO <- round(MUVRclassObject$nVar[1])    #####min
  nVarOR <- round(MUVRclassObject$nVar[3])  #####max
  O <-
    names(sort(MUVRclassObject$VIRank[, 1])[1:nVarO])    ###names of number of variables in min with highest VIrank
  OR <-
    names(sort(MUVRclassObject$VIRank[, 3])[1:nVarOR])  ###names of number of variables in max with highest VIrank
  R <-
    OR[!OR %in% O]                       ####names of variables in redundant (from min up to max)
  ALL <-
    rownames(MUVRclassObject$VIRank)           ####names of all variables
  if(class(MUVRclassObject)[3]=="rdCVnet"){
    O <-
      names(sort(MUVRclassObject$varTable)[1:nVarO])    ###names of number of variables in min with highest VIrank
    OR <-
      names(sort(MUVRclassObject$varTable)[1:nVarOR])  ###names of number of variables in max with highest VIrank
    R <-
      OR[!OR %in% O]                       ####names of variables in redundant (from min up to max)
    ALL<-names(MUVRclassObject$varTable)
  }

  N <-
    ALL[!ALL %in% c(O, R)]                ####names of variables of the Noisy (the rest)
  numbers <-
    c(length(ALL), length(O), length(R), length(N))  ##Number of variables in each category
  names(numbers) <- c('All', 'Optimal', 'Redundant', 'Noisy')
  return(list(
    ALL = ALL,
    Optimal = O,
    Redundant = R,
    Noisy = N,
    numbers = numbers
  ))
}
