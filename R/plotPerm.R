#' Plot for comparison of actual model fitness vs permutation/resampling
#' Plots histogram of null hypothesis (permutation/resampling) distribution, actual model fitness and cumulative p-value.
#' Plot defaults to "greater than" or "smaller than" tests and cumulative probability in Student's t-distribution
#' @param actual Actual model fitness (e.g. Q2, AUROC or number of misclassifications)
#' @param distribution Null hypothesis (permutation) distribution of similar metric as `actual`
#' @param xlab Label for x-axis (e.g. 'Q2 using real value',"Q2 using distributions","BER" 'AUROC', or 'Misclassifications')
#' @param ylab label for y-axis
#' @param side Cumulative p either "greater" or "smaller" than H0 distribution (defaults to side of median(H0))
#' @param type c('t','non',"smooth","rank","ecdf")
#' @param xlim Choice of user-specified x-limits (if default is not adequate)
#' @param ylim Choice of user-specified y-limits (if default is not adequate)
#' @param breaks Choice of user-specified histogram breaks (if default is not adequate)
#' @param main Choice of user-specified plot title
#' @param permutation_visual choice of showing median or mean or none
#' @param pos Choice of position of p-value label (if default is not adequate)
#' @param curve if add curve or not base on the mid
#' @param extend how many percenrtage of the orignical range do we start
#' @param show_p if p value is added to the figure
#' @param show_actual_value show the actual value on the vertical line or not
#' @param multiple_p_shown show many p values
#' @param round_number How many digits does it keep
#' @return Plot
#' @export
#' @examples
#' \donttest{
#' data("freelive2")
#' actual <- sample(YR2, 1)
#' distribution <- YR2
#' plotPerm (actual, distribution)
#' }
plotPerm <- function(actual,
                     distribution,
                     ####a distribution
                     xlab = NULL,
                     side = c('greater', 'smaller'),
                     type = "t",
                     ylab = NULL,
                     #               type=c('t','non',"smooth","ecdf","rank"),
                     xlim,
                     ylim = NULL,
                     breaks = 'Sturges',
                     pos,
                     ####Choice of position of p-value label (if default is not adequate)
                     main = NULL,
                     permutation_visual = "none",
                     curve = TRUE,
                     extend = 0.1,
                     multiple_p_shown = NULL,
                     show_actual_value = TRUE,
                     show_p = TRUE,
                     round_number = 4) {

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  if (!is.null(multiple_p_shown)) {
    if (!any(multiple_p_shown %in% c('t', 'non', "smooth", "ecdf", "rank"))) {
      stop("This type can not be implemented")
    }
  }
  if (!permutation_visual %in% c("mean", "median", "none")) {
    stop("this type not supoorted")
  }
  if (!missing(type)) {
    if (!any(type %in% c('t', 'non', "smooth", "ecdf", "rank"))) {
      stop("This type can not be implemented")
    }
  }
  if (missing(type)) {
    type == 't'
  }

  if (!missing(side)) {
    if (side != "smaller" &
        side != "greater")
      stop("This side can not be implemented")
  }

  ######when it is Q2 or MISS
  if (!missing(pos)) {
    pos_rev <- 6 - pos
  }
  if (missing(side)) {
    side <- ifelse(actual < median(distribution), 'smaller', 'greater')
  }

  if (missing(pos)) {
    pos <- ifelse(side == 'smaller', 4, 2)
    pos_rev <- ifelse(side == 'smaller', 2, 4)
  }
  ##a position specifier for the text. If specified this overrides any adj value given. Values of 1, 2, 3 and 4,
  ##respectively indicate positions below, to the left of, above and to the right of the specified (x,y) coordinates.
  if (!length(multiple_p_shown) >= 2) {
    pP <- pPerm(actual,
                distribution,
                side,
                type = type,
                extend = extend)     ####calculate p value
    x_values <- distribution

    ran_pre <- range(c(actual, distribution))
    from_pre <- ran_pre[1] - diff(ran_pre) * extend
    to_pre <- ran_pre[2] + diff(ran_pre) * extend

if(sd(x_values)!=0){
    t_values_min <-
      min((from_pre - mean(x_values)) / sd(x_values),
          (to_pre - mean(x_values)) / sd(x_values))
    t_values_max <-
      max((from_pre - mean(x_values)) / sd(x_values),
          (to_pre - mean(x_values)) / sd(x_values))
    t_values_sim <-
      seq(t_values_min, t_values_max, length.out = 500)

    ### the simulated x values
    x_values_sim <- (t_values_sim * sd(x_values)) + mean(x_values)
  }else{
    t_values_sim <-rep(mean(x_values),500)
    ### the simulated x values
    x_values_sim <- (t_values_sim * sd(x_values)) + mean(x_values)

  }

    #  curve(dt((x-mean(x_values))/(sd(x_values)/sqrt(length(x_values))),
    #               df=length(x_values)-1))
    y_values_sim <- dt(t_values_sim, df = length(x_values) - 1)


    if (curve == TRUE) {
      if (type == "smooth") {
        ran <- range(c(actual, x_values, x_values_sim))
        from <- ran[1] - diff(ran) * extend
        to <- ran[2] + diff(ran) * extend

        if (missing(xlim)) {
          xlim <- c(from, to)
        }

        h <- hist(
          distribution,
          breaks,
          xlim = xlim,
          #ylim=ylim,
          axes = FALSE,
          ###remove both axes
          xlab = xlab,
          ylab = ylab,
          yaxt = 'n',
          freq = FALSE,
          ##### if FALSE, probability densities, component density, are plotted (so that the histogram has a total area of one).
          main = main
        )
        h2 <-
          max(h$density)  ######as estimated density values, This is to decide how high the vertical line will be drawn

        lines(pP$dens, lwd = 2, col = "red")
        abline(h = 0)
        axis(1, pos = 0)
        lines(rep(actual, 2),     ###x1,x2 for the line
              c(0, h2 * 0.75))          ##y1 ,y2 forthe line
      }

      if (type == "t") {
        ran <- range(c(actual, x_values, x_values_sim))
        from <- ran[1] - diff(ran) * extend
        to <- ran[2] + diff(ran) * extend

        if (missing(xlim)) {
          xlim <- c(from, to)
        }

        h <- hist(
          distribution,
          breaks,
          xlim = xlim,
          #ylim=ylim,
          axes = FALSE,
          ###remove both axes
          xlab = xlab,
          ylab = ylab,
          yaxt = 'n',
          # freq=FALSE,   ##### if FALSE, probability densities, component density, are plotted (so that the histogram has a total area of one).
          main = main
        )
        h2 <- max(h$density)
        axis(1, pos = 0)


        par(new = TRUE)
        plot.new()
        plot.window(
          xlim = xlim,
          ylim = c(0, max(y_values_sim)) ,
          xlab = '',
          ylab = '',
          main = ''
        )
        lines (
          x_values_sim ,
          y_values_sim ,
          #   type = "l",
          bty = "n",
          #  axes = FALSE,
          xlim = xlim,
          yaxt = 'n',
          col = "red",
          lwd = 2,
          ylim = c(0, max(y_values_sim))
        )
        abline(h = 0)

        lines(rep(actual, 2),     ###x1,x2 for the line
              c(0, max(y_values_sim) * 0.75))          ##y1 ,y2 forthe line

        # lines(x_values_sim, y_values,#*length(x_values_sim),
        #        lwd = 2,col = "red")

      }

      if (type == "rank") {
        ran <- range(c(actual, x_values, x_values_sim))
        from <- ran[1] - diff(ran) * extend
        to <- ran[2] + diff(ran) * extend

        if (missing(xlim)) {
          xlim <- c(from, to)
        }

        h <- hist(
          distribution,
          breaks,
          xlim = xlim,
          #ylim=ylim,
          axes = FALSE,
          ###remove both axes
          xlab = xlab,
          ylab = ylab,
          yaxt = 'n',
          freq = FALSE,
          ##### if FALSE, probability densities, component density, are plotted (so that the histogram has a total area of one).
          main = main
        )
        h2 <-
          max(h$density)  ######as estimated density values, This is to decide how high the vertical line will be drawn
        abline(h = 0)
        #lines(pP$dens,lwd = 2, col = "red")
        axis(1, pos = 0)
        lines(rep(actual, 2),     ###x1,x2 for the line
              c(0, h2 * 0.75))          ##y1 ,y2 forthe line


      }
    } else{
      if (type == "t") {
        ran <- range(c(actual, x_values, x_values_sim))
        from <- ran[1] - diff(ran) * extend
        to <- ran[2] + diff(ran) * extend

        if (missing(xlim)) {
          xlim <- c(from, to)
        }
      } else{
        ran <- range(c(actual, x_values, x_values_sim))
        from <- ran[1] - diff(ran) * extend
        to <- ran[2] + diff(ran) * extend

        if (missing(xlim)) {
          xlim <- c(from, to)
        }
      }
      h <- hist(
        distribution,
        breaks,
        xlim = xlim,
        #ylim=ylim,
        axes = FALSE,
        ###remove both axes
        xlab = xlab,
        ylab = ylab,
        yaxt = 'n',
        freq = FALSE,
        ##### if FALSE, probability densities, component density, are plotted (so that the histogram has a total area of one).
        main = main
      )
      h2 <-
        max(h$density)  ######as estimated density values, This is to decide how high the vertical line will be drawn
      abline(h = 0)
      #lines(pP$dens,lwd = 2, col = "red")
      axis(1, pos = 0)
      lines(rep(actual, 2),     ###x1,x2 for the line
            c(0, h2 * 0.75))


    }

    ###the coordinate at which the axis line is to be drawn: if not NA this overrides the value of line.

    ####comment out if you don't want y axis
    # if(side=='smaller') {axis(2,pos=0,las=1)   #### the style of axis labels. (0=parallel, 1=all horizontal, 2=all perpendicular to axis, 3=all vertical)
    #  }else {axis(2,pos=h$breaks[1],las=1)}

    if (type == "t" & curve == TRUE) {
      ymax <- max(y_values_sim) * 0.75
    } else{
      ymax <- h2 * 0.75
    }


    if (show_p == TRUE) {
      if (!is.nan(pP$p) & is.numeric(pP$p)) {
        text(actual,
             ###x position of the text
             ymax,
             ##y position of the text
             pos = pos,
             ##a position specifier for the text. If specified this overrides any adj value given. Values of 1, 2, 3 and 4,
             ##respectively indicate positions below, to the left of, above and to the right of the specified (x,y) coordinates.
             labels = paste('p=',
                            signif(pP$p, round_number),
                            sep = '')) ####what is the text
        ##For signif the recognized values of digits are 1...22, and non-missing values are rounded to the nearest integer in that range.
        ##Complex numbers are rounded to retain the specified number of digits in the larger of the components.
        ##Each element of the vector is rounded individually, unlike printing.
      }

      if (!is.nan(pP$p) & !is.numeric(pP$p)) {
        text(actual,
             ###x position of the text
             ymax,
             ##y position of the text
             pos = pos,
             ##a position specifier for the text. If specified this overrides any adj value given. Values of 1, 2, 3 and 4,
             ##respectively indicate positions below, to the left of, above and to the right of the specified (x,y) coordinates.
             labels = paste("p", pP$p, sep = '')) ####what is the text
        ##For signif the recognized values of digits are 1...22, and non-missing values are rounded to the nearest integer in that range.
        ##Complex numbers are rounded to retain the specified number of digits in the larger of the components.
        ##Each element of the vector is rounded individually, unlike printing.
      }

      if (!is.null(pP$points)) {
        ################################ add curves

        #lines(dens)
      }

    }
    if(show_actual_value==TRUE){
    text(actual,
         ###x position of the text
         max(h$density) * 0.003,
         ##y position of the text
         pos = 3,
         ##a position specifier for the text. If specified this overrides any adj value given. Values of 1, 2, 3 and 4,
         labels = paste0(signif(actual, round_number)))
    }
    if (permutation_visual == "mean") {
      text(
        median(distribution),
        ###x position of the text
        ymax * 0.003,
        ##y position of the text
        pos = 3,
        ##a position specifier for the text. If specified this overrides any adj value given. Values of 1, 2, 3 and 4,
        labels = paste0("mean=",
                        signif(mean(distribution), round_number))
      )
    }

    if (permutation_visual == "median") {
      text(
        median(distribution),
        ###x position of the text
        ymax * 0.003,
        ##y position of the text
        pos = 3,
        ##a position specifier for the text. If specified this overrides any adj value given. Values of 1, 2, 3 and 4,
        labels = paste0("median=",
                        signif(
                          median(distribution), round_number
                        ))
      )
    }

    if (permutation_visual == "none" | missing(permutation_visual)) {

    }
  } else {
    #############################################################################################
    x_values <- distribution
    ran_pre <- range(c(actual, distribution))
    from_pre <- ran_pre[1] - diff(ran_pre) * extend
    to_pre <- ran_pre[2] + diff(ran_pre) * extend


    if(sd(x_values)!=0){
    t_values_min <-
      min((from_pre - mean(x_values)) / sd(x_values),
          (to_pre - mean(x_values)) / sd(x_values))
    t_values_max <-
      max((from_pre - mean(x_values)) / sd(x_values),
          (to_pre - mean(x_values)) / sd(x_values))
    t_values_sim <-
      seq(t_values_min, t_values_max, length.out = 500)

    ### the simulated x values
    x_values_sim <- (t_values_sim * sd(x_values)) + mean(x_values)
    }else{
      t_values_sim <-rep(mean(x_values),500)
      ### the simulated x values
      x_values_sim <- (t_values_sim * sd(x_values)) + mean(x_values)

    }

    #  curve(dt((x-mean(x_values))/(sd(x_values)/sqrt(length(x_values))),
    #               df=length(x_values)-1))
    y_values_sim <- dt(t_values_sim, df = length(x_values) - 1)


    if ("t" %in% multiple_p_shown) {
      ran <- range(c(actual, x_values, x_values_sim))
    } else{
      ran <- range(c(actual, x_values, x_values_sim))
    }

    from <- ran[1] - diff(ran) * extend
    to <- ran[2] + diff(ran) * extend
    if (missing(xlim)) {
      xlim <- c(from, to)
    }


    h <- hist(
      distribution,
      breaks,
      xlim = xlim,
      # ylim=ylim,
      axes = FALSE,
      ###remove both axes
      xlab = xlab,
      ylab = ylab,
      yaxt = 'n',
      freq = FALSE,
      ##### if FALSE, probability densities, component density, are plotted (so that the histogram has a total area of one).
      main = main
    )
    h2 <-
      max(h$density) ######as estimated density values, This is to decide how high the vertical line will be drawn
    axis(1, pos = 0)


    ppp <- list()
    pP <- c()
    for (i in 1:length(multiple_p_shown)) {
      ppp[[i]] <- pPerm(actual,
                        distribution,
                        side,
                        type = multiple_p_shown[i],
                        extend = extend)     ####calculate p value
      pP[i] <- ppp[[i]]$p
    }

    if ("t" %in% multiple_p_shown & curve == TRUE) {
      ymax <- max(y_values_sim) * 0.75
    } else{
      ymax <- h2 * 0.75
    }

    if (curve == TRUE &
        "t" %in% multiple_p_shown & !"smooth" %in% multiple_p_shown) {
      abline(h = 0)



      par(new = TRUE)
      plot.new()
      plot.window(
        xlim = xlim,
        ylim = c(0, max(y_values_sim)) ,
        xlab = '',
        ylab = '',
        main = ''
      )
      lines (
        x_values_sim ,
        y_values_sim ,
        #  type = "l",
        #  bty="n",
        #   axes = FALSE,
        xlim = xlim,
        yaxt = 'n',
        col = "darkgreen",
        lwd = 2,
        ylim = c(0, max(y_values_sim))
      )

      legend(
        'topright',
        legend = c("t"),
        lty = 1,
        lwd = 2,
        cex = 0.5,
        trace = FALSE,
        ####line type
        col = "darkgreen",
        bty = 'n'
      )

      lines(rep(actual, 2),     ###x1,x2 for the line
            c(0, max(y_values_sim) * 0.75))          ##y1 ,y2 forthe line

    }


    if (curve == TRUE &
        "smooth" %in% multiple_p_shown & !"t" %in% multiple_p_shown) {
      lines(rep(actual, 2),     ###x1,x2 for the line
            c(0, ymax))          ##y1 ,y2 forthe line
      lines(ppp[[which(multiple_p_shown == "smooth")]]$dens, lwd = 2, col = "red")


      abline(h = 0)
      legend(
        'topright',
        legend = c("smooth"),
        lwd = 2,
        lty = 1,
        cex = 0.5,
        trace = FALSE,
        ####line type
        col = "red",
        bty = 'n'
      )

    }

    if (curve == TRUE &
        "smooth" %in% multiple_p_shown & "t" %in% multiple_p_shown) {
      abline(h = 0)


      lines(ppp[[which(multiple_p_shown == "smooth")]]$dens, lwd = 2, col = "red")



      par(new = TRUE)
      plot.new()
      plot.window(
        xlim = xlim,
        ylim = c(0, max(y_values_sim)) ,
        xlab = '',
        ylab = '',
        main = ''
      )
      lines (
        x_values_sim ,
        y_values_sim ,
        #  type = "l",
        #  bty="n",
        #  axes = FALSE,
        xlim = xlim,
        lty = 1,
        lwd = 2,
        yaxt = 'n',
        col = "darkgreen",
        ylim = c(0, max(y_values_sim))
      )

      lines(rep(actual, 2),     ###x1,x2 for the line
            c(0, max(y_values_sim)) * 0.75)          ##y1 ,y2 forthe line



      legend(
        'topright',
        legend = c("t", "smooth"),
        lty = 1,
        lwd = 2,
        cex = 0.5,
        trace = FALSE,
        ####line type
        col = c("darkgreen", "red"),
        bty = 'n'
      )


    }

    ####comment out if you do not eant axis
    #  if(side=='smaller') {axis(2,pos=0,las=1)}   #### the style of axis labels. (0=parallel, 1=all horizontal, 2=all perpendicular to axis, 3=all vertical)
    #  else {axis(2,
    #          pos=h$breaks[1],las=1)}



      lines(rep(actual, 2),     ###x1,x2 for the line
            c(0, ymax))        ##y1 ,y2 forthe line

    if (show_p == TRUE) {

      for (i in 1:length(ppp)) {
        if (!is.nan(ppp[[i]]$p) & is.numeric(ppp[[i]]$p)) {
          text(
            actual,
            ###x position of the text
            ymax - i * 0.1 * ymax,
            ##y position of the text
            pos = pos,
            ##a position specifier for the text. If specified this overrides any adj value given. Values of 1, 2, 3 and 4,
            ##respectively indicate positions below, to the left of, above and to the right of the specified (x,y) coordinates.
            labels = paste(
              multiple_p_shown[i],
              ' p=',
              signif(ppp[[i]]$p, round_number),
              sep = ''
            )
          )

        }
        if (!is.nan(ppp[[i]]$p) & !is.numeric(ppp[[i]]$p)) {
          text(
            actual,
            ###x position of the text
            ymax - i * 0.1 * ymax,
            ##y position of the text
            pos = pos,
            ##a position specifier for the text. If specified this overrides any adj value given. Values of 1, 2, 3 and 4,
            ##respectively indicate positions below, to the left of, above and to the right of the specified (x,y) coordinates.
            labels = paste(multiple_p_shown[i], " p", ppp[[i]]$p, sep =
                             '')
          ) ####what is the text
        }
      }
    }
    if(show_actual_value == TRUE){
    text(actual,
         ###x position of the text
         ymax * 0.003,
         ##y position of the text
         pos = 3,
         ##a position specifier for the text. If specified this overrides any adj value given. Values of 1, 2, 3 and 4,
         labels = paste0(signif(actual, round_number)))
    }

    if (permutation_visual == "mean") {
      text(
        median(distribution),
        ###x position of the text
        ymax * 0.003,
        ##y position of the text
        pos = 3,
        ##a position specifier for the text. If specified this overrides any adj value given. Values of 1, 2, 3 and 4,
        labels = paste0("mean=",
                        signif(mean(distribution), round_number))
      )
    }

    if (permutation_visual == "median") {
      text(
        median(distribution),
        ###x position of the text
        ymax * 0.003,
        ##y position of the text
        pos = 3,
        ##a position specifier for the text. If specified this overrides any adj value given. Values of 1, 2, 3 and 4,
        labels = paste0("median=",
                        signif(
                          median(distribution), round_number
                        ))
      )
    }

    if (permutation_visual == "none" | missing(permutation_visual)) {

    }
    ###################################################################


  }


}
