#' @keywords internal
#' @aliases MUVR2-package
#'
#' @importFrom foreach foreach registerDoSEQ %do% %dopar%
#' @importFrom doParallel registerDoParallel
#' @importFrom dplyr filter mutate select
#' @importFrom ggplot2 aes after_stat annotate coord_cartesian coord_flip element_blank element_text expansion facet_wrap geom_abline geom_blank geom_boxplot geom_histogram geom_hline geom_line geom_point geom_segment geom_text geom_tile geom_vline ggplot guides labs scale_alpha_manual scale_colour_gradient scale_colour_gradientn scale_colour_manual scale_fill_manual scale_linetype_manual scale_shape_manual scale_x_continuous scale_x_log10 scale_y_continuous scale_y_reverse sec_axis theme theme_bw
#' @importFrom glmnet glmnet cv.glmnet
#' @importFrom grDevices colorRampPalette dev.off palette png
#' @importFrom graphics abline arrows axis box boxplot hist legend lines matlines matplot matpoints mtext par plot.new plot.window points text
#' @importFrom grid arrow unit
#' @importFrom magrittr %>%
#' @importFrom mgcv gam predict.gam s
#' @importFrom pROC roc
#' @importFrom parallel detectCores
#' @importFrom randomForest randomForest
#' @importFrom ranger ranger
#' @importFrom rlang .data
#' @importFrom stats as.dist coef coefficients cor density dt ecdf hclust heatmap lm loess median predict pt quantile resid sd setNames
#' @importFrom utils globalVariables
"_PACKAGE"
