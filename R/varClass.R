#' Report variables belonging to different classes
#' Reports names and numbers of variables: all as well as Optimal (min model), redundant (from min up to max) and noisy (the rest)
#' @param MUVRclassObject A MUVR class object
#' @return A list with names and numbers of variables: all as well as Optimal (Corresponding to 'min' or minial-optimal model),
#' Redundant (from 'min' up to 'max' or all-relevant ) and Noisy (the rest)
#' @export
#'
############################################################################
#Change MVObject to MUVRclassObject
#
####################################################################
varClass <- function(MUVRclassObject) {
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
