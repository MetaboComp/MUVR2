#' make reference distribution for resampling tests/permutation tests
#' @param Y the target variable
#' @param n number of permutations to run
#' @param fitness number of repetitions for each permutation (defaults to value of actual model)
#' @param ... some more argument for sampling frm distribution
#' @return  a histogram of reference distribution
#' @export
#' @examples
#' \dontrun{
#' data("freelive2")
#' H0_reference(YR2)
#' }
H0_reference <- function(Y,
                         n = 1000,
                         fitness = c("Q2", "BER", "MISS", "AUROC"),
                         ...) {
  if(is.atomic(Y)==FALSE){
    stop("The input must be an vector")
  }
  if (missing(fitness)) {
    if (is.factor(Y) | is.character(Y) | is.logical(Y)) {
      fitness <- "BER"
    } else if (is.integer(Y) | is.numeric(Y)) {
      fitness <- "Q2"
    }
  } else if (!missing(fitness)) {
    if (!fitness %in% c("Q2", "BER", "MISS", "AUROC")) {
      stop("This performance metric is not supported.")
    }
    if ((is.factor(Y) |
         is.character(Y) | is.logical(Y)) & (fitness == "Q2")) {
      stop("This performance metric does not match the clas of the target variable.")
    } else if (is.integer(Y) | is.numeric(Y) & (fitness != "Q2")) {
      stop("This performance metric does not match the clas of the target variable.")
    }
  }


  Ref <- numeric(n)
  for (p in 1:n) {
    Y_new <- sampling_from_distribution(Y) # More clever sampling ;)

    if (fitness == "Q2") {
      Ref[p] <- Q2_calculation(Y_new, Y)
    } else if (fitness == "BER") {
      Ref[p] <- getBER(Y, Y_new)
    } else if (fitness == "MISS") {
      Ref[p] <- getMISS(Y, Y_new)

    } else if (fitness == "AUROC") {
      cat("This method is not supported. The distribution will be plotted for BER")
      Ref[p] <- getBER(Y, Y_new)
      fitness <- "BER"
    }

  }
  if (fitness == "Q2") {
    lim <- c(min(Ref) * 1.1, max(Ref) * 1.1)
  } else if (fitness == "BER") {
    lim <- c(0, 1)
  } else if (fitness == "MISS") {
    lim <- c(0, length(Y))
  }


  h <- hist(
    Ref,
    breaks = 40,
    main = paste(fitness, "reference distribution"),
    xlim = lim,
    xlab = paste(fitness),
    ylab = "Frequency",
    yaxt = 'n'
  )

  text(
    mean(Ref),
    ###x position of the text
    max(h$density) * 0.003,
    ##y position of the text
    pos = 3,
    ##a position specifier for the text. If specified this overrides any adj value given. Values of 1, 2, 3 and 4,
    labels = paste0("mean=",
                    signif(mean(Ref), 2))
  )
}
