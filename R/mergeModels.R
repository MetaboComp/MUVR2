#' Merge two MUVR class object that use regression for PLS or RF methods
#' This 2 MUVR class object has the same indata except that nRep is different
#' @param MV1 a MUVR class Object
#' @param MV2 a MUVR class Object
#' @return A merged MURV class object
#'
#' @export
#' @examples
#' \dontrun{
#'
#' }
mergeModels <- function(MV1, MV2) {
  if (any(class(MV1) == 'Multilevel') |
      any(class(MV1) == 'Classification')) {
    cat('\nNot yet supported')
    stop()
  }

  if (any(class(MV2) == 'Multilevel') |
      any(class(MV2) == 'Classification')) {
    cat('\nNot yet supported')
    stop()
  }
  #####name in data
  in1 <- MV1$inData
  in2 <- MV2$inData
  ######same repetition numbers in both models
  nRep1 <- MV1$inData$nRep
  nRep2 <- MV2$inData$nRep
  nRep <- nRep1 + nRep2
  ####rename the repetion numbers in 2 models as null
  in1$nRep <- NULL
  in2$nRep <- NULL

  if (!identical(in1, in2)) {
    cat('\nIndata not identical between models')
    stop()
  }
  ####save everything of MV1 , whichs indata  is identical as MV2
  DA <- MV1$inData$DA
  PLS <-
    MV1$inData$method == 'PLS'      ###if used pls, PLS=TRUE , if RF.PLS=FALSE
  ####
  yP <- MV1$yPred
  yPPR <- MV1$yPredPerRep
  VIRank <- MV1$VIRank
  VIRankrep <- MV1$VIRankPerRep

  nV <- MV1$nVar
  nVPR <- MV1$nVarPerRep

  if (PLS) {
    nC <- MV1$nComp
    nCPR <- MV1$nCompPerRep
  }
  for (i in 1:3) {
    ###min mid max
    if (DA) {
      cat('\nNot yet implemented')
    } else {
      yPPR[[i]] <-
        cbind(yPPR[[i]], MV2$yPredPerRep[[i]])     ###combine 2 matrix
      yP[, i] <-
        apply(yPPR[[i]], 1, mean)                       ####mean of repetitions

      VIRankrep[[i]] <-
        cbind(VIRank[[i]], MV2$VIRankPerRep[[i]])   ###combine 2 matrix
      VIRank[, i] <- apply(VIRankrep[[i]], 1, mean)

      nVPR[[i]] <-
        c(nVPR[[i]], MV2$nVarPerRep[[i]])     #####Cobine 2 vectors
      nV[i] <- mean(nVPR[[i]])

      if (PLS) {
        nCPR[[i]] <- c(nCPR[[i]], MV2$nCompPerRep[[i]])   #####Cobine 2 vectors
        nC[i] <- mean(nCPR[[i]])
      }
    }
  }
  newMod <- list()
  newMod$inData <- in1     ###same inData in1=in2
  newMod$inData$nRep <- nRep

  newMod$yPred <- yP
  newMod$yPredPerRep <- yPPR

  newMod$VIRank <- VIRank
  newMod$VIRankPerRep <- VIRankrep

  newMod$nVar <- nV
  newMod$nVarPerRep <- nVPR

  if (PLS) {
    newMod$nComp <- nC
    newMod$nCompPerRep <- nCPR
  }

  class(newMod) <- c(class(MV1), 'Merged')
  return(newMod)
}
