# MUVR2 0.2.0

## New features

* Every plot now has a `ggplot2` version, returning a `ggplot` object rather than
  drawing to the graphics device: `ggplotMV()`, `ggplotVAL()`, `ggplotVIRank()`,
  `ggplotStability()`, `ggplotPerm()`, `ggpermutationPlot()`, `ggplotPred()`,
  `ggplotPCA()` and `ggbiplotPLS()`. They can be themed, composed, saved with
  `ggplot2::ggsave()`, and made interactive with `plotly::ggplotly()` (pass
  `tooltip = "text"` for the hover labels). The base graphics functions are
  unchanged and are not deprecated.

* `plotMV()` and `ggplotMV()` gained a `consensusOnly` argument. With
  `consensusOnly = TRUE` they draw only the consensus predictions and omit the
  smaller per-repetition dots — useful for the classification swimlane, which
  gets crowded, though it works for every model type.

* A pkgdown website, with the tutorial rewritten as articles and a plot gallery
  comparing the base, ggplot2 and plotly versions of every plot side by side.

## Bug fixes

* `plotVAL()` on an elastic net model fitted with `fit_curve = "gam"` labelled the
  y-axis `"RMSEP"` and drew no cut-off legend. `"RMSEP"` is only the metric for
  regression, so on a classification model — whose fitness is BER by default — it
  plotted balanced error rates under an axis labelled RMSEP. It now reads the
  model's own fitness metric and draws the legend, matching the `loess` branch.

* `plotPerm()` returned a *visible* `NULL` — its last statement was an empty `if`
  — so knitr and the console printed `NULL` underneath every plot it drew. It now
  returns invisibly, like the other plot functions.

* `H0_reference()` now returns the reference distribution invisibly, in addition
  to drawing its histogram. Previously it returned the result of its final
  `text()` call, i.e. `NULL`, so there was no way to compare the reference
  distribution against the resampled distribution from `H0_test()`.

* Model type is now determined with `inherits(x, "rdCVnet")` rather than
  `class(x)[3] == "rdCVnet"`, which silently misidentified models whenever
  anything else touched the class vector.

* The plot functions report a clear error when given an elastic net model that
  has not been through `getVar()`, rather than silently drawing nothing or
  failing obscurely. `plotVIRank()` errors on a non-MUVR object instead of
  warning and then failing anyway.

## Internal

* The computation behind the plots was extracted into shared helpers
  (`R/plot-data.R`), so the base and ggplot2 versions of a plot cannot drift
  apart.

* Imports are declared per function in a package-level documentation file, rather
  than as a blanket `@import` block. Unused dependencies (`psych`, `splines`)
  were dropped and used-but-undeclared ones (`utils`, `grid`) added.

* `predict.plsMUVR()` and `predict.plsdaMUVR()` are registered as S3 methods.

* The plot tests, which were placeholders, now have visual regression snapshots
  (vdiffr) and the data helpers have unit tests.

# MUVR2 0.1.0

* Initial release.
