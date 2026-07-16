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

## Bug fixes

* `ggplotVAL()` drew the PLS/RF validation curves as a single scrambled zigzag
  rather than one line per segment. It used `geom_line()`, which re-sorts points by
  the x variable before joining them, destroying the per-segment grouping; it now
  uses `geom_path()`, which joins in data order.

* `plotPerm()` returned a *visible* `NULL` — its last statement is an empty `if` —
  so knitr and the console printed `NULL` underneath every plot it drew. It now
  returns invisibly, like the other plot functions.

## Improvements

* The ggplot2 plots now agree with their base counterparts on the things that are
  not a matter of style:
    * `ggplotVIRank()` computes its boxes with `boxplot.stats()`, i.e. Tukey's
      hinges, as `boxplot()` does. ggplot2's own `geom_boxplot()` uses type-7
      quantiles, which with few repetitions gives a narrower box, a tighter
      1.5*IQR fence, and hence whiskers cut short and points wrongly called
      outliers — the two flavours disagreed about the same data.
    * `ggplotVAL()` and `ggplotPerm()` bin with `hist()` and draw the bars it
      chose, rather than asking ggplot2 for the same *number* of bins, which put
      the bars somewhere else. `ggplotPerm()` takes the same `breaks` argument as
      `plotPerm()`.
    * `ggplotMV()` draws solid, opaque points, as `plotMV()` does.
    * `ggplotVAL()` marks the min/mid/max cut-offs in the base version's colours.

* Two things the ggplot2 versions do differently, on purpose:
    * `ggplotVIRank()`'s dot plot shows the *not selected* variables as faint grey
      dots. `plotVIRank()` draws them in white, i.e. invisibly, so an empty cell
      cannot be told from a missing one.
    * `ggplotPCA()` gives `colVar` a continuous colour bar reading in the
      variable's own units, rather than the base version's three-swatch "low /
      mid / high" key, and titles both legends with the name of the variable
      passed in (`colLab` / `symbLab` override it). The base legend implies a
      categorical variable and never says what is being coloured.

* `ggplotly()` on `ggplotVAL()` no longer labels the curves
  `(Validation segments,1)`. plotly names a trace after every discrete scale in
  the plot, so the cut-offs' linetype scale was leaking into the curves' names;
  the cut-offs now take their line types directly.

* The interactive versions gained the polish the review asked for:
    * `ggplotMV()` (regression, classification and multilevel) and `ggplotPCA()`
      carry a hover tooltip naming the sample, its prediction and — for the
      swimlane — its class. Pass `tooltip = "text"` to `ggplotly()`.
    * `ggplotVIRank()`'s boxplot survives `ggplotly()` (the previous
      `geom_boxplot(stat = "identity")` came out empty) and its whiskers now have
      end caps, as `boxplot()` draws them, because the box is built from rectangle
      and segment primitives. There is also more space between the boxes.
    * `ggplotPerm()`'s p-value and actual-value labels are `geom_text`, not
      `annotate()`, so they survive `ggplotly()`, which drops plot annotations.
    * `ggbiplotPLS()` titles its colour legend (`colLab`, defaulting to the name of
      the `xCol` expression), so the shading is no longer an unlabelled gradient.

* `ggplotStability()` facets its panels rather than stacking them with
  `par(mfrow=)`, so the panels stay aligned and share an x-axis. As in the base
  version, each panel's y-axis is anchored (proportions and balanced error rates
  run 0 to 1, misclassifications 0 to the number of samples) rather than fitted to
  its data, which would zoom into the noise and make a converged model look
  unstable.

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
