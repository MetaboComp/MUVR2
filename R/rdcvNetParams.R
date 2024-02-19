#' Make custom parameters for rdcvNet internal modelling
#' Custom parameters can be set in the function call or by manually setting "slots" in the resulting methParam object
#' @param robust Robustness (slack) criterion for determining min and max knees (defaults to 0.05)
#' @param family the options could be "gaussian", "binomial", "poisson", "multinomial", "cox", "mgaussian"
#' @param oneHot TRUE or FALSE using onehot endcoding or not
#' @param nRepInner how many nRepInner
#' @param NZV NZV
#' @return a `methParam` object
#' @export
#' @examples
#' # Standard parameters for rdcvNet
#' methParam <- rdcvNetParams()
rdcvNetParams <- function(robust = 0.05,
                          family = 'gaussian',
                          nRepInner = 1,
                          NZV = TRUE,
                          oneHot = TRUE) {
  if (!family %in% c("gaussian",
                     "binomial",
                     "poisson",
                     "multinomial",
                     "cox",
                     "mgaussian")) {
    stop("Wrong family")
  }
  methParam <- list(
    robust = robust,
    family = family,
    nRepInner = nRepInner,
    oneHot = oneHot,
    NZV = NZV
  )
  return(methParam)
}
