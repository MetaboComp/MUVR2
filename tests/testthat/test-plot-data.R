## The internal data helpers shared by the base and ggplot2 plot functions.

test_that("modelIndex maps min/mid/max onto 1/2/3", {
  expect_equal(MUVR2:::modelIndex("min"), 1L)
  expect_equal(MUVR2:::modelIndex("Mid"), 2L)
  expect_equal(MUVR2:::modelIndex("MAX"), 3L)
  expect_error(MUVR2:::modelIndex("middle"), "min")
  expect_error(MUVR2:::modelIndex(c("min", "max")), "min")
})

test_that("isEN identifies elastic net models regardless of class position", {
  expect_true(MUVR2:::isEN(classModel))
  expect_false(MUVR2:::isEN(regrModel))

  ## The old class(x)[3] == "rdCVnet" idiom broke as soon as anything prepended
  ## to the class vector; inherits() does not.
  padded <- classModel
  class(padded) <- c("Merged", class(classModel))
  expect_true(MUVR2:::isEN(padded))
})

test_that("mvData returns consensus and per-repetition predictions", {
  d <- MUVR2:::mvData(regrModel, model = "min")
  expect_equal(d$type, "regression")
  expect_equal(nrow(d$overall), length(YR2))
  expect_equal(nrow(d$perRep), length(YR2) * regrModel$inData$nRep)
  expect_equal(d$Q2, regrModel$fitMetric$Q2[1])

  d <- MUVR2:::mvData(classModelPLS, model = "min")
  expect_equal(d$type, "classification")
  ## one row per sample per class
  expect_equal(nrow(d$overall), length(Yotu) * nlevels(Yotu))
  ## every misclassified sample is flagged exactly once
  expect_equal(nrow(d$wrong), sum(classModelPLS$yClass[, 1] != Yotu))

  d <- MUVR2:::mvData(MLModel, model = "min")
  expect_equal(d$type, "multilevel")
})

test_that("mvData rejects mismatched sample labels", {
  expect_error(MUVR2:::mvData(regrModel, sampLabels = c("a", "b")),
               "sampLabels")
})

test_that("valData splits by core method", {
  expect_equal(MUVR2:::valData(regrModel)$type, "rdCV")
  expect_equal(MUVR2:::valData(classModelVar)$type, "fitness")
  expect_equal(MUVR2:::valData(getVar(classModel, option = "quantile"))$type,
               "quantile")
  expect_error(MUVR2:::valData(classModel), "getVar")
})

test_that("valData's rdCV curves match the underlying VAL array", {
  d <- MUVR2:::valData(regrModel)
  VAL <- regrModel$VAL$VAL
  expect_equal(nrow(d$segments), length(VAL))
  expect_equal(d$overall$value, unname(apply(VAL, 2, mean)))
  expect_equal(nrow(d$repMeans), length(d$count) * dim(VAL)[3])
})

test_that("stabilityData produces one series pair per metric per repetition", {
  d <- MUVR2:::stabilityData(regrModel, model = "min")
  expect_equal(d$nRep, regrModel$inData$nRep)
  ## regression: variables, proportion, Q2 -- each with 2 series of nRep points
  expect_equal(nrow(d$long), 3 * 2 * d$nRep)
  ## proportions are proportions
  expect_true(all(d$VARep >= 0 & d$VARep <= 1))

  d <- MUVR2:::stabilityData(classModelPLS, model = "min")
  expect_equal(nrow(d$long), 4 * 2 * d$nRep)
  expect_true(all(d$berRep >= 0 & d$berRep <= 1))
})

test_that("viRankData ranks for PLS and maps selections for elastic net", {
  d <- MUVR2:::viRankData(regrModel, n = 10, model = "min")
  expect_equal(d$type, "rank")
  expect_equal(nrow(d$VIRankRep), 10)
  ## all 10 are inside the min selection, which is larger than 10
  expect_true(all(d$long$selected))

  d <- MUVR2:::viRankData(classModelVar, n = 10)
  expect_equal(d$type, "selection")
  expect_equal(ncol(d$subset), 10)
  expect_setequal(levels(d$long$selected), c("Selected", "Not selected"))
})

test_that("viRankData only clusters when asked to", {
  clustered <- MUVR2:::viRankData(classModelVar, n = 10, cluster = TRUE)
  plain <- MUVR2:::viRankData(classModelVar, n = 10, cluster = FALSE)
  ## unclustered keeps the variables in selection-ratio order, i.e. varTable order,
  ## and the calibration models in their original order
  expect_equal(colnames(plain$matrix_count), names(classModelVar$varTable))
  expect_equal(rownames(plain$matrix_count),
               as.character(seq_len(nrow(plain$matrix_count))))
  ## clustering reorders both, but neither adds nor drops anything
  expect_setequal(colnames(clustered$matrix_count),
                  colnames(plain$matrix_count))
  expect_setequal(rownames(clustered$matrix_count),
                  rownames(plain$matrix_count))
})

test_that("predData accepts vectors, matrices and data frames", {
  d <- MUVR2:::predData(YR2, regrModel$yPred[, 1])
  expect_equal(nrow(d$consensus), length(YR2))

  d <- MUVR2:::predData(YR2, regrModel$yPred)
  expect_equal(nrow(d$perPred), length(YR2) * 3)
  expect_equal(d$consensus$Ypred, unname(rowMeans(regrModel$yPred)))

  d <- MUVR2:::predData(YR2, as.data.frame(regrModel$yPred))
  expect_equal(nrow(d$perPred), length(YR2) * 3)

  expect_error(MUVR2:::predData(YR2, list(1, 2)), "vector")
  expect_error(MUVR2:::predData(YR2, 1:3), "same number")
})

test_that("biplotData puts loadings on the scores' scale", {
  d <- MUVR2:::biplotData(regrModel$Fit[[2]], comps = 1:2)
  expect_equal(nrow(d$scores), length(YR2))
  expect_true(d$scaleFactor > 0)
  ## scaled loadings fit inside the scores' range
  expect_lte(max(abs(d$loads$x * d$scaleFactor)), d$rSc)
})
