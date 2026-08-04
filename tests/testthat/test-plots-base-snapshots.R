# Visual regression tests for the base graphics plot functions.
#
# These snapshots exist to pin the appearance of the base plots while their
# internals are refactored onto the shared data helpers in R/plot-data.R.
# They are skipped on CI, where fonts and graphics devices differ enough to
# make base graphics SVGs unstable.

skip_if_not_installed("vdiffr")
skip_on_ci()
skip_on_cran()

test_that("plotMV snapshots are stable", {
  vdiffr::expect_doppelganger("plotMV regression min",
                              function() plotMV(regrModel, model = "min"))
  vdiffr::expect_doppelganger("plotMV regression max",
                              function() plotMV(regrModel, model = "max"))
  vdiffr::expect_doppelganger("plotMV classification PLS",
                              function() plotMV(classModelPLS, model = "min"))
  vdiffr::expect_doppelganger("plotMV classification EN",
                              function() plotMV(classModel))
  vdiffr::expect_doppelganger("plotMV multilevel",
                              function() plotMV(MLModel, model = "min"))
})

test_that("plotVAL snapshots are stable", {
  vdiffr::expect_doppelganger("plotVAL regression rdCV",
                              function() plotVAL(regrModel))
  vdiffr::expect_doppelganger("plotVAL classification PLS",
                              function() plotVAL(classModelPLS))
  vdiffr::expect_doppelganger("plotVAL elastic net fitness",
                              function() plotVAL(classModelVar))
  vdiffr::expect_doppelganger(
    "plotVAL elastic net quantile",
    function() plotVAL(getVar(classModel, option = "quantile"))
  )
  ## The gam curve used to hard-code ylab = "RMSEP" and draw no legend, even for
  ## a model whose fitness metric is BER. Pin the corrected version.
  vdiffr::expect_doppelganger(
    "plotVAL elastic net fitness gam",
    function() plotVAL(getVar(classModel, fit_curve = "gam"))
  )
})

test_that("plotVAL labels the y-axis with the model's own fitness metric", {
  ## Regression models are the only ones whose metric is RMSEP; a classification
  ## elastic net model reports BER, and both curve fits must say so.
  expect_equal(classModelVar$VAL$metric, "BER")

  loessFit <- MUVR2:::valData(getVar(classModel, fit_curve = "loess"))
  gamFit <- MUVR2:::valData(getVar(classModel, fit_curve = "gam"))

  expect_equal(loessFit$metric, "BER")
  expect_equal(gamFit$metric, "BER")
  expect_equal(gamFit$fit_curve, "gam")
})

test_that("plotVIRank snapshots are stable", {
  vdiffr::expect_doppelganger("plotVIRank regression",
                              function() plotVIRank(regrModel, n = 20))
  vdiffr::expect_doppelganger("plotVIRank regression n above nVar",
                              function() plotVIRank(regrModel, n = 60, model = "min"))
  vdiffr::expect_doppelganger("plotVIRank elastic net heatmap",
                              function() plotVIRank(classModelVar, n = 20))
  vdiffr::expect_doppelganger(
    "plotVIRank elastic net dotplot",
    function() plotVIRank(classModelVar, n = 20, maptype = "dotplot")
  )
})

test_that("plotStability snapshots are stable", {
  vdiffr::expect_doppelganger("plotStability regression",
                              function() plotStability(regrModel, model = "min"))
  vdiffr::expect_doppelganger("plotStability classification PLS",
                              function() plotStability(classModelPLS, model = "min"))
  vdiffr::expect_doppelganger("plotStability elastic net",
                              function() plotStability(classModelVar, model = "min"))
  vdiffr::expect_doppelganger("plotStability multilevel",
                              function() plotStability(MLModel, model = "min"))
})

test_that("plotPerm snapshots are stable", {
  set.seed(1)
  distribution <- rnorm(200, mean = 0.1, sd = 0.1)
  actual <- 0.45

  vdiffr::expect_doppelganger("plotPerm t",
                              function() plotPerm(actual, distribution, type = "t"))
  vdiffr::expect_doppelganger("plotPerm smooth",
                              function() plotPerm(actual, distribution, type = "smooth"))
  vdiffr::expect_doppelganger("plotPerm rank no curve",
                              function() plotPerm(actual, distribution, type = "rank",
                                                  curve = FALSE))
  vdiffr::expect_doppelganger(
    "plotPerm multiple p",
    function() plotPerm(actual, distribution, multiple_p_shown = c("t", "smooth"))
  )
  vdiffr::expect_doppelganger(
    "plotPerm median marker",
    function() plotPerm(actual, distribution, permutation_visual = "median")
  )
})

test_that("plotPCA snapshots are stable", {
  pca <- prcomp(XRVIP2)
  vdiffr::expect_doppelganger("plotPCA plain",
                              function() plotPCA(pca))
  vdiffr::expect_doppelganger("plotPCA coloured and symbols",
                              function() plotPCA(pca, colVar = YR2, symbVar = YR2 > median(YR2)))
})

test_that("plotPred snapshots are stable", {
  vdiffr::expect_doppelganger("plotPred vector",
                              function() plotPred(YR2, regrModel$yPred[, 1]))
  vdiffr::expect_doppelganger("plotPred matrix",
                              function() plotPred(YR2, regrModel$yPred))
})

test_that("biplotPLS snapshots are stable", {
  vdiffr::expect_doppelganger(
    "biplotPLS scores and loadings",
    function() biplotPLS(regrModel$Fit[[2]], comps = 1:2, xCol = YR2,
                         labPlSc = FALSE, labPlLo = FALSE)
  )
})

test_that("permutationPlot snapshots are stable", {
  permutation_result <- H0_test(regrModel, n = 10, nRep = 2, nOuter = 4)
  vdiffr::expect_doppelganger(
    "permutationPlot regression",
    function() permutationPlot(regrModel, permutation_result, model = "min")
  )
})
