# Copyright 2012 Google Inc.
# All Rights Reserved.
#
# Author: xzhang@google.com (Tom Zhang)

# This plotting tool PlotIncrementalFit() makes disgnostic plots to visualize
# incremental contribution of explanatory variables in a regression.

# The general purpose plotting function can be used in two scenarios. The input
# data frame contains the response y and a number of component columns C_i.
# - When "incremental" is TRUE, C_i is the contribution of X_i in the fixed
#   regression model. So we plot y versus \sum_{1<=i<=k}{C_i} for each k.
# - When "incremental" is FALSE, each column C_i is the fitted.values of a
#   separate regression. So we plot y versus C_i.

# This plotting tool was originally made to visualize models fitted by Steve
# Scott's bsts object. There is a utility function to build the input for this
# plotting tool from a bsts object, which can be found in the bsts package.


# Default color scheme for the incremental fit plot.
# Note that we want to use transparent colors for the main plots.
kDefaultColorSpec <-
  list(response=rgb(49/255, 54/255, 149/255, alpha=0.9),
       component.major=rgb(165/255, 0, 38/255, alpha=0.9),
       component.minor=rgb(240/255, 128/255, 128/255, alpha=0.6),
       residual.positive=rgb(116/255, 173/255, 209/255),
       residual.negative=rgb(253/255, 174/255, 97/255),
       auxiliary=rgb(t(col2rgb("grey"))/255, alpha=0.4))

ColorToGrayscale <- function(color, method="luminosity") {
  # convert a color to grayscale, currently supporting three modes
  stopifnot(method %in% c("luminosity", "lightness", "average"))
  R <- col2rgb(color)["red", ] / 255
  G <- col2rgb(color)["green", ] / 255
  B <- col2rgb(color)["blue", ] / 255
  if (method == "luminosity") {
    grayness <- 0.21 * R + 0.71 * G + 0.07 * B
  } else if (method == "lightness") {
    grayness <- (max(R, G, B) + min(R, G, B)) / 2
  } else {
    grayness <- (R + G + B) / 3
  }
  return(rgb(grayness, grayness, grayness,
             alpha=col2rgb(color, alpha=T)["alpha", ]/255))
}

kGrayscaleColorSpec <-
  list(response=ColorToGrayscale(kDefaultColorSpec$response),
       component.major=ColorToGrayscale(kDefaultColorSpec$component.major),
       component.minor=ColorToGrayscale(kDefaultColorSpec$component.minor),
       residual.positive=ColorToGrayscale(kDefaultColorSpec$residual.positive),
       residual.negative=ColorToGrayscale(kDefaultColorSpec$residual.negative),
       auxiliary=ColorToGrayscale(kDefaultColorSpec$auxiliary))

RemoveCommonSuffix <- function(some.strings) {
  ## Helper function that remove common suffix, so to make date label succinct.
  ## Args:
  ##   some.strings: an array, vector or list of strings.
  ## Returns:
  ##   a vector of strings that has their common suffix removed.
  while (length(unique(substring(some.strings, nchar(some.strings)))) == 1) {
    some.strings <- substring(some.strings, 1, nchar(some.strings) - 1)
  }
  return(some.strings)
}


PrepareAxisSpec <- function(data,
                            response.colname,
                            component.colnames,
                            plot.type=c("together", "against"),
                            dates=NULL,
                            incremental,
                            plot.residuals) {
  ## Helper function to compute the xlim/ylim/ticks/labels for the plots.
  ## It is trivial when plot.residuals=F; otherwise we do the calculation.
  ## Args:
  ##   data:               a matrix or data frame.
  ##   response.colname:   name of the response column.
  ##   component.colnames: name of the component columns.
  ##   plot.type:          if "together", we plot the actual and predictions
  ##                       together against the date (or simply 1:nrow(data));
  ##                       if "against", we plot the predictions against the
  ##                       actual.
  ##   dates:              a vector of dates.
  ##   incremental:        When TRUE, the component columns are components of
  ##                       a fixed regression; otherwise they are fitted.values
  ##                       of different regression models.
  ##   plot.residuals:     boolean; when FALSE the function is trivial; when
  ##                       TRUE, we do the calculation.
  ## Returns:
  ##   a list with these following fields:
  ##     xlim ,ylim, yticks,
  ##   when plot.residuals=T, also
  ##     ylim.split, residual.ylim, residual.ticks, residual.units,
  ##   if dates is not NULL, also
  ##     date.ticks, date.labels.
  plot.type <- match.arg(plot.type)
  y <- data[, response.colname]
  spec <- list()
  if (!is.null(dates)) {
    date.labels <- pretty(dates)
    spec$date.ticks <- which(dates %in% date.labels)
    spec$date.labels <- RemoveCommonSuffix(as.character(dates))[spec$date.ticks]
  }
  if (!plot.residuals) {
    if (plot.type == "together") {
      spec$xlim <- c(1, length(y))
    } else if (plot.type == "against") {
      y.span <- max(y) - min(y)
      spec$xlim <- c(min(y) - .04 * y.span, max(y) + .04 * y.span)
    }
    spec$ylim <- range(y)
    spec$yticks <- pretty(y)
  } else {
    # prepare the coordinates to make room for residuals plot
    # xlim is .5 wider on both sides because we plot residuals as a barplot
    if (plot.type == "together") {
      spec$xlim <- c(.5, length(y) + .5)
    } # otherwise keep spec$xlim as the case above
    # residual plot's size is 1/5 of the main plot
    y.width <- max(y) - min(y)
    ylim.upper <- max(y)
    ylim.lower <- min(y) - .2 * y.width
    spec$ylim <- c(ylim.lower, ylim.upper)
    # calculate where to split the plot (main plot above residual plot below)
    ylim.upper.exact <- ylim.upper + .04 * (ylim.upper - ylim.lower)
    ylim.lower.exact <- ylim.lower - .04 * (ylim.upper - ylim.lower)
    spec$ylim.split <- min(y) - (ylim.upper.exact - max(y))
    # recalculate yticks so not to show in the residual subplot
    y.pretty <- pretty(y)
    spec$yticks <- y.pretty[y.pretty >= spec$ylim.split & y.pretty < ylim.upper]
    # calculate ylim for the residual plot, so it's properly scaled
    if (incremental) {
      preds.max <- apply(as.matrix(data[, component.colnames]),
                         MARGIN=1, FUN=function(x) {max(cumsum(x))})
      preds.min <- apply(as.matrix(data[, component.colnames]),
                         MARGIN=1, FUN=function(x) {min(cumsum(x))})
    } else {
      preds.max <- apply(as.matrix(data[, component.colnames]),
                         MARGIN=1, FUN=function(x) {max(x)})
      preds.min <- apply(as.matrix(data[, component.colnames]),
                         MARGIN=1, FUN=function(x) {min(x)})
    }
    residuals.max <- max(y - preds.min)
    residuals.min <- min(y - preds.max)
    bar.up <- max(0.1, residuals.max)
    bar.lo <- min(-0.1, residuals.min)
    scaling.a <- (spec$ylim.split - ylim.lower.exact) / (bar.up - bar.lo)
    scaling.b <- spec$ylim.split - scaling.a * bar.up
    bar.up.scaled <- (ylim.upper.exact - scaling.b) / scaling.a
    spec$residual.ylim <- c(bar.lo, bar.up.scaled)
    # ticks and labels for the barplot
    residual.ticks <- pretty(c(residuals.min, residuals.max))
    residual.units <- min(abs(residual.ticks[residual.ticks != 0]))
    spec$residual.units <- residual.ticks[abs(residual.ticks) == residual.units]
    spec$residual.ticks <- residual.ticks[2:(length(residual.ticks) - 1)]
  }
  return(spec)
}


MakeOneSubPlot <- function(y,
                           fit,
                           fit.prev=NULL,
                           plot.type=c("together", "against"),
                           plot.residuals=T,
                           axis.spec,
                           color.spec=kDefaultColorSpec,
                           main.str="",
                           mean.abs.error=T,
                           print.date.labels=F,
                           ...) {
  ## Make one subplot for the incremental fit plots.
  ## Args:
  ##   y:                 the response
  ##   fit:               the fit
  ##   fit.prev:          the previouse fit
  ##   plot.type:         if "together", we plot the actual and predictions
  ##                      together against the date (or simply 1:nrow(data));
  ##                      if "against", we plot the predictions against the
  ##                      actual.
  ##   plot.residuals:    whether to plot the residuals
  ##   axis.spec:         axis specification for the plot
  ##   color.spec:        color specification for the plot
  ##   main.str:          title string
  ##   mean.abs.error:    if TRUE, print m.a.e in plot title
  ##   print.date.labels: if TRUE, print date labels on x-axis
  ##   ...:               other args for the png()/pdf() call
  ## Returns:
  ##   NULL. The function produces a plot.
  plot.type <- match.arg(plot.type)
  if (mean.abs.error) {
    main.str <- sprintf("%s (mae=%s)", main.str, signif(mean(abs(y - fit)), 5))
  }
  if (plot.type == "together") {
    plot(y, type="p", col=color.spec$response, pch=20, main=main.str,
         xlab="", ylab="", xaxt="n", yaxt="n",
         xlim=axis.spec$xlim, ylim=axis.spec$ylim)
    if (!is.null(fit.prev)) {
      lines(fit.prev, col=color.spec$component.minor, lwd=2)
    }
    lines(fit, col=color.spec$component.major, lwd=2)
  } else if (plot.type == "against") {
    plot(y, fit, type="p", col=color.spec$component.major, pch=20,
         main=main.str, xlab="", ylab="", xaxt="n", yaxt="n",
         xlim=axis.spec$xlim, ylim=axis.spec$ylim)
    if (!is.null(fit.prev)) {
      points(y, fit.prev, col=color.spec$component.minor)
    }
    points(y, fit, col=color.spec$component.major) # do this again to stay on top
    abline(a=0, b=1, col="grey")
  }
  axis(side=2, at=axis.spec$yticks, padj=1, tck=-0.01)
  # add date ticks; note we must call this before plotting residuals
  # because the residuals are a barplot which need to have a .5 offset.
  if (!is.null(axis.spec$date.ticks)) {
    if (print.date.labels) {
      print.date.labels <- axis.spec$date.labels
    }
    axis(side=1, at=axis.spec$date.ticks, tck=1, fg=color.spec$auxiliary,
         labels=print.date.labels)
  }
  # plot the residuals
  if (plot.residuals) {
    abline(h=axis.spec$ylim.split)
    residuals <- y - fit
    res.colors <- rep(color.spec$residual.positive, length(residuals))
    res.colors[residuals < 0] <- color.spec$residual.negative
    par(new=T)
    barplot(residuals, space=0, col=res.colors, border=NA, axes=F,
            ylim=axis.spec$residual.ylim)
    abline(h=0, col=color.spec$auxiliary)
    axis(side=4, at=axis.spec$residual.ticks, padj=-1, tck=-0.01, label=F)
    axis(side=4, at=axis.spec$residual.units, padj=-1, tck=-0.01)
  }
  return(invisible(NULL))
}


PlotIncrementalFit <- function(data,
                               response.colname,
                               component.colnames,
                               date.colname=NULL,
                               incremental=TRUE,
                               shift.components=TRUE,
                               plot.type=c("together", "against"),
                               main.title=NULL,
                               subplot.titles=NULL,
                               ncol.plot=2,
                               plot.residuals=TRUE,
                               color.spec=kDefaultColorSpec,
                               plot.fname=NULL,
                               plot.fname.series=NULL,
                               plot.ftype=c("png", "pdf"),
                               ...) {
  ## Make incremental fit plots.
  ## Args:
  ##   data:               a matrix or data frame that contains the response and
  ##                       components of a regression model.
  ##   response.colname:   name of the response column.
  ##   component.colnames: name of the component columns.
  ##   date.colname:       name of the date column; if NULL (default) we try to
  ##                       find column whose name is "date" (case insensitive),
  ##                       if not found, we simply use 1:nrow(data) for x-axis.
  ##   incremental:        when TRUE, the component columns are components of
  ##                       a fixed regression; otherwise they are fitted.values
  ##                       of different regression models.
  ##   shift.components:   when TRUE, we perform mean removal for all
  ##                       component.columns (except the first) and adjust the
  ##                       first column to maintain the sum. This can prevent
  ##                       certain components from throwing off the subplot in
  ##                       some cases. (Only legit when incremental is TRUE.)
  ##   plot.type:          if "together", we plot the actual and predictions
  ##                       together against the dates (or simply 1:nrow(data));
  ##                       if "against", we plot the predictions against the
  ##                       actual.
  ##   main.title:         main title for the entire plot at the top; if NULL
  ##                       (default), use paste("Incremental Plot for Fitting",
  ##                       response.colname); to disable, use main.title="".
  ##   subplot.titles:     titles for the individual subplots; if NULL (default)
  ##                       we derive from the component.colnames.
  ##   ncol.plot:          number of columns in the plot.
  ##   plot.residuals:     whether to plot the residuals or not.
  ##   color.spec:         a list of colors for plotting, it should have the
  ##                       form like kDefaultColorSpec above.
  ##   plot.fname:         filename for the output; now only support png;
  ##                       if NULL (default), only plot, not writing to file.
  ##                       if need other format, handle outside this function.
  ##   plot.fname.series:  if this is set, generate one png for each subplot,
  ##                       name the files sequentially; it trumps plot.fname.
  ##   plot.ftype:         now we support png and pdf.
  ##   ...:                other args for the png()/pdf() call.
  ## Returns:
  ##   NULL. The function produces a plot.
  plot.type <- match.arg(plot.type)
  plot.ftype <- match.arg(plot.ftype)
  # shift.components only makes sense when incremental is TRUE.
  stopifnot(incremental || !shift.components)
  y <- data[, response.colname]
  num.components <- length(component.colnames)
  # handle date axis and label
  if (is.null(date.colname)) {
    dategrep <- grep("^date$", names(data), ignore.case=T)
    stopifnot(length(dategrep) <= 1)
    if (length(dategrep) == 1) {
      date.colname <- names(data)[dategrep]
    }
  }
  # if shift.components, perform mean removal
  if (shift.components) {
    component.means <- colMeans(data[, component.colnames])
    data[, component.colnames[2:num.components]] <- apply(
        data[, component.colnames[2:num.components]],
        MARGIN=2, FUN=function(x){x-mean(x)})
    data[, component.colnames[1]] <- (data[, component.colnames[1]] +
                                      sum(component.means[2:num.components]))
  }
  dates <- NULL
  if (!is.null(date.colname)) {
    dates <- as.Date(data[, date.colname])
  }
  axis.spec <- PrepareAxisSpec(data=data,
                               response.colname=response.colname,
                               component.colnames=component.colnames,
                               plot.type=plot.type,
                               dates=dates,
                               incremental=incremental,
                               plot.residuals=plot.residuals)

  if (!is.null(plot.fname.series) || !is.null(plot.fname)) {
    if (!is.null(plot.fname)) {
      filename <- plot.fname
    } else {
      filename <- paste0(plot.fname.series, "%02d.", plot.ftype)
    }
    if (plot.ftype == "png") {
      png(filename=filename, ...)
    } else if (plot.ftype == "pdf") {
      pdf(file=filename, onefile=is.null(plot.fname.series), ...)
    }
  }

  # if plot.fname.series is NULL, we generate plots as individual files
  # Otherwise we use layout for nice looking plot and margins
  if (is.null(plot.fname.series)) {
    nrow.plot <- ceiling(num.components / ncol.plot)
    layout.mat <- matrix(2:((nrow.plot + 1) * ncol.plot + 1), ncol=ncol.plot,
                         byrow=T)
    layout.mat <- cbind(0, rbind(1, layout.mat), 0)
    layout(layout.mat, widths=c(.1, rep(1 / ncol.plot, ncol.plot), .05),
           heights=c(0.05, rep(1 / nrow.plot, nrow.plot), 0.10))
    if (is.null(main.title)) {
      main.title <- paste("Incremental Plot for Fitting", response.colname)
    }
    par(mar=c(0, 1, 1, 1))
    plot.new()
    text(x=0.5, y=0.5, labels=main.title, cex=2)
  }
  if (is.null(subplot.titles)) {
    subplot.titles <- component.colnames
    subplot.titles[2:num.components] <-
        paste("add", component.colnames[2:num.components])
    subplot.titles <- paste0(1:num.components, ". ", subplot.titles)
  }
  fit <- rep(0, nrow(data))
  for (i in 1:length(component.colnames)) {
    if (i == 1) {
      fit.prev <- NULL
    } else {
      fit.prev <- fit
    }
    if (incremental) {
      fit <- fit + data[, component.colnames[i]]
    } else {
      fit <- data[, component.colnames[i]]
    }
    if (!is.null(plot.fname.series)) {
      par(mar=c(5, 4, 4, 2))
    } else {
      par(mar=c(0, 1, 3, 1))
    }
    print.date.labels <- !is.null(plot.fname.series) && (plot.type == "together")
    MakeOneSubPlot(y=y,
                   fit=fit,
                   fit.prev=fit.prev,
                   plot.type=plot.type,
                   plot.residuals=plot.residuals,
                   axis.spec=axis.spec,
                   color.spec=color.spec,
                   main.str=subplot.titles[i],
                   mean.abs.error=T,
                   print.date.labels=print.date.labels)
  }
  # Now print the date labels
  if (is.null(plot.fname.series) && (plot.type == "together")) {
    par(mar=c(0, 1, 2.5, 1))
    for (i in 1:ncol.plot) {
      plot(0, xlim=c(0.5, length(y)+0.5), type="n", axes=F, ylab="", xlab="")
      axis(side=3, at=axis.spec$date.ticks, tick=F,
           labels=axis.spec$date.labels)
    }
  }

  if (!is.null(plot.fname.series) || !is.null(plot.fname)) {
    dev.off()
  }
  return(invisible(NULL))
}
