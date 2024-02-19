#'The original result MUVR model has the class of 'MVObject'.
#'This function is to transform its classname from 'MVObject' to 'MUVR'
#'This function also transform item of the output in MUVR():
#'VIP-->VIRank
#'VIPPerRep-->VIRankPerRep
#'@param MUVRclassObject  A 'MUVR' class object
#'@return A 'MUVR' class Object where all is transformed to


convertMUVR <- function(MUVRclassObject) {
  if (!any(class(MUVRclassObject) == 'MVObject'))
  {
    stop("It is not a MUVR class object")
  }
  class(MUVRclassObject)[1] <- 'MUVR'
  class(MUVRclassObject)[4] <- 'converted'
  for (i in 1:length(names(classModel)))
  {
    if (names(MUVRclassObject)[i] == "VIP") {
      names(MUVRclassObject)[i] <- "VIRank"
    }
    if (names(MUVRclassObject)[i] == "VIPPerRep") {
      names(MUVRclassObject)[i] <- "VIRankPerRep"
    }
  }
  return(MUVRclassObject)
}
