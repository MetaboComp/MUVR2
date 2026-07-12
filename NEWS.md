# MUVR2 (development version)

## New features

* Every plot now has a `ggplot2` version, returning a `ggplot` object rather than
  drawing to the graphics device: `ggplotMV()`, `ggplotVAL()`, `ggplotVIRank()`,
  `ggplotStability()`, `ggplotPerm()`, `ggpermutationPlot()`, `ggplotPred()`,
  `ggplotPCA()` and `ggbiplotPLS()`. They can be themed, composed, saved with
  `ggplot2::ggsave()`, and made interactive with `plotly::ggplotly()`. The base
  graphics functions are unchanged and are not deprecated.

* A pkgdown website at <https://metabocomp.github.io/MUVR2/>, with the tutorial
  rewritten as articles, and a plot gallery comparing the base, ggplot2 and plotly
  versions of every plot side by side.

## Improvements

* `ggplotStability()` facets its panels rather than stacking them with
  `par(mfrow=)`, so the panels stay aligned and share an x-axis.

* `ggplotPerm()` draws the histogram and the fitted curve on a common density
  scale. `plotPerm()` draws the Student's t curve on a second, hidden axis, which
  means the curve and the histogram cannot be compared directly.

* `plotVAL()` on an elastic net model fitted with `fit_curve = "gam"` now labels the
  y-axis with the model's own fitness metric and draws the min/mid/max legend, as
  the `loess` branch always did. The gam branch previously hard-coded `"RMSEP"`,
  which is only the metric for regression — on a classification model, whose
  fitness is BER by default, it plotted balanced error rates under an axis labelled
  RMSEP. The values were right; the label was wrong.

* `H0_reference()` now returns the reference distribution invisibly, in addition to
  drawing its histogram. Previously it returned the result of its final `text()`
  call, i.e. `NULL`, so there was no way to compare the reference distribution
  against the resampled distribution from `H0_test()` — which is the whole point of
  computing it.

* The plot functions now report a clear error when given an elastic net model that
  has not been through `getVar()`, rather than silently drawing nothing or failing
  obscurely.

* `plotVIRank()` now errors on a non-MUVR object instead of warning and then
  failing anyway.

## Internal

* The computation behind the plots was extracted into shared helpers
  (`R/plot-data.R`), so the base and ggplot2 versions of a plot cannot drift apart.

* Model type is now determined with `inherits(x, "rdCVnet")` rather than
  `class(x)[3] == "rdCVnet")`, which silently misidentified models whenever
  anything else touched the class vector (e.g. `mergeModels()`).

* Imports are declared per function in a package-level documentation file, rather
  than as a blanket `@import` block attached to `MUVR2_EN()`. Unused dependencies
  (`psych`, `splines`) were dropped and used-but-undeclared ones (`utils`, `grid`)
  added.

* `predict.plsMUVR()` and `predict.plsdaMUVR()` are registered as S3 methods.

* The plot tests, which were placeholders, now have visual regression snapshots
  (vdiffr) and the data helpers have unit tests.

# MUVR2 0.1.0

* Initial release.
