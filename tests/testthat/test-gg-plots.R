## The ggplot2 plots. Two things are checked: that the object builds at all
## (ggplot is lazy, so a broken layer only errors on build), and that it still
## looks the way it did (vdiffr snapshots).

buildsOk <- function(p) {
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
}

test_that("ggplotMV builds for every model type", {
  buildsOk(ggplotMV(regrModel, model = "min"))
  buildsOk(ggplotMV(classModelPLS, model = "min"))
  buildsOk(ggplotMV(classModel))
  buildsOk(ggplotMV(MLModel, model = "min"))
})

test_that("ggplotMV rejects a bad factCols length", {
  expect_error(ggplotMV(classModelPLS, factCols = c("red", "blue")),
               "factCols")
})

test_that("ggplotMV consensusOnly drops the per-repetition layer", {
  countLayers <- function(p) length(p$layers)

  for (m in list(regrModel, classModelPLS, classModel, MLModel)) {
    full <- ggplotMV(m)
    consensus <- ggplotMV(m, consensusOnly = TRUE)
    buildsOk(consensus)
    ## exactly one fewer geom_point layer (the per-repetition dots) when consensusOnly
    expect_equal(countLayers(full) - countLayers(consensus), 1)
  }
})

test_that("ggplotVAL builds for every core method", {
  buildsOk(ggplotVAL(regrModel))
  buildsOk(ggplotVAL(classModelPLS))
  buildsOk(ggplotVAL(classModelVar))
  buildsOk(ggplotVAL(getVar(classModel, option = "quantile")))
  buildsOk(ggplotVAL(getVar(classModel, fit_curve = "gam")))
})

test_that("ggplotVAL tells you when an elastic net model has not seen getVar()", {
  expect_error(ggplotVAL(classModel), "getVar")
})

test_that("ggplotVIRank builds for ranks and for selection maps", {
  buildsOk(ggplotVIRank(regrModel, n = 20))
  buildsOk(ggplotVIRank(regrModel, n = 60, model = "min"))
  buildsOk(ggplotVIRank(classModelVar, n = 20))
  buildsOk(ggplotVIRank(classModelVar, n = 20, maptype = "dotplot"))
})

test_that("ggplotStability builds and has one panel per metric", {
  p <- ggplotStability(regrModel, model = "min")
  buildsOk(p)
  ## regression: variables, proportion, Q2
  expect_length(levels(p$data$metric), 3)

  p <- ggplotStability(classModelPLS, model = "min")
  buildsOk(p)
  ## classification: variables, proportion, misclassifications, BER
  expect_length(levels(p$data$metric), 4)

  buildsOk(ggplotStability(classModelVar, model = "min"))
  buildsOk(ggplotStability(MLModel, model = "min"))
})

test_that("ggplotPerm builds for every type", {
  set.seed(1)
  distribution <- rnorm(200, mean = 0.1, sd = 0.1)
  actual <- 0.45

  buildsOk(ggplotPerm(actual, distribution, type = "t"))
  buildsOk(ggplotPerm(actual, distribution, type = "smooth"))
  buildsOk(ggplotPerm(actual, distribution, type = "rank", curve = FALSE))
  buildsOk(ggplotPerm(actual, distribution, multiple_p_shown = c("t", "smooth")))
  buildsOk(ggplotPerm(actual, distribution, permutation_visual = "median"))
  expect_error(ggplotPerm(actual, distribution, type = "nonsense"), "type")
  expect_error(ggplotPerm(actual, distribution, permutation_visual = "nope"),
               "permutation_visual")
})

test_that("ggplotPCA, ggplotPred and ggbiplotPLS build", {
  pca <- prcomp(XRVIP2)
  buildsOk(ggplotPCA(pca))
  buildsOk(ggplotPCA(pca, colVar = YR2))
  buildsOk(ggplotPCA(pca, colVar = YR2, symbVar = YR2 > median(YR2)))

  buildsOk(ggplotPred(YR2, regrModel$yPred[, 1]))
  buildsOk(ggplotPred(YR2, regrModel$yPred))

  buildsOk(ggbiplotPLS(regrModel$Fit[[2]], comps = 1:2, xCol = YR2,
                       labPlSc = FALSE, labPlLo = FALSE))
})

test_that("ggpermutationPlot builds from an H0_test result", {
  permutation_result <- H0_test(regrModel, n = 10, nRep = 2, nOuter = 4)
  buildsOk(ggpermutationPlot(regrModel, permutation_result, model = "min"))
})

## ---------------------------------------------------------------------------
## Visual snapshots

test_that("ggplot2 plots look the way they did", {
  skip_if_not_installed("vdiffr")
  skip_on_ci()
  skip_on_cran()

  vdiffr::expect_doppelganger("ggplotMV regression",
                              ggplotMV(regrModel, model = "min"))
  vdiffr::expect_doppelganger("ggplotMV classification PLS",
                              ggplotMV(classModelPLS, model = "min"))
  vdiffr::expect_doppelganger("ggplotMV classification EN",
                              ggplotMV(classModel))
  vdiffr::expect_doppelganger("ggplotMV multilevel",
                              ggplotMV(MLModel, model = "min"))
  vdiffr::expect_doppelganger("ggplotVAL regression rdCV",
                              ggplotVAL(regrModel))
  vdiffr::expect_doppelganger("ggplotVAL elastic net fitness",
                              ggplotVAL(classModelVar))
  vdiffr::expect_doppelganger("ggplotVIRank regression",
                              ggplotVIRank(regrModel, n = 20))
  vdiffr::expect_doppelganger("ggplotVIRank elastic net heatmap",
                              ggplotVIRank(classModelVar, n = 20))
  vdiffr::expect_doppelganger("ggplotStability regression",
                              ggplotStability(regrModel, model = "min"))
  vdiffr::expect_doppelganger("ggplotStability classification PLS",
                              ggplotStability(classModelPLS, model = "min"))

  set.seed(1)
  vdiffr::expect_doppelganger(
    "ggplotPerm t",
    ggplotPerm(0.45, rnorm(200, mean = 0.1, sd = 0.1), type = "t")
  )
  vdiffr::expect_doppelganger("ggplotPCA coloured",
                              ggplotPCA(prcomp(XRVIP2), colVar = YR2))
  vdiffr::expect_doppelganger("ggplotPred matrix",
                              ggplotPred(YR2, regrModel$yPred))
  vdiffr::expect_doppelganger(
    "ggbiplotPLS scores and loadings",
    ggbiplotPLS(regrModel$Fit[[2]], comps = 1:2, xCol = YR2,
                labPlSc = FALSE, labPlLo = FALSE)
  )
})
