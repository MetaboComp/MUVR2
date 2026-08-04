## Shared setup for every article.
##
## The articles fit real models, but small ones: the tutorial's settings
## (nRep = 35, nOuter = 8) take minutes to hours, which is not something a
## website build can carry. Everything here is therefore run with a reduced
## nRep/nOuter under a fixed seed. The shape of the results is the same; the
## exact numbers are not, and where the tutorial quotes a number that depends on
## a full-scale model, the article says so.

knitr::opts_chunk$set(
  collapse = FALSE,
  comment = "#>",
  ## SVG rather than PNG, so the plots stay sharp when zoomed, and svglite rather
  ## than grDevices::svg, so that the text in them stays real text: selectable,
  ## searchable, and readable by a screen reader.
  ##
  ## This overrides the device set in _pkgdown.yml, which the reference pages use.
  ## They cannot use svglite: pkgdown renders their examples through fig_save(),
  ## which hardcodes bg = NA, and svglite rejects NA where it wants a colour
  ## string. Here in the article path, bg is ours to set, so svglite is fine.
  dev = "svglite",
  fig.ext = "svg",
  dev.args = list(bg = "transparent"),
  fig.width = 7,
  fig.height = 5,
  fig.align = "center",
  out.width = "100%",
  warning = FALSE,
  message = FALSE
)

set.seed(42)

## Article-scale settings. Compare with the tutorial's final-model settings,
## given in the text of each article.
nRep_article <- 4
nOuter_article <- 6
varRatio_article <- 0.75
