# ******************************************************************************
# Created: 14-Aug-2019
# Author:  J. DuBois
# Contact: jason.dubois@wildlife.ca.gov
# Purpose: This file contains functions or methods to create custom base plots
# ******************************************************************************

#' Customize Axis Tick Labels
#'
#' @description \code{AxisFormat} (for now the numeric method) customizes axis
#'   tick labels by reducing tick labels with many characters to the bare
#'   minimum. For example, 0.015 becomes 15 or 1500 becomes 1.5. The returned
#'   function \code{AxisTitle} allows for customizing the axis label with `x
#'   10^power`, where `power` is either negative or positive depending upon
#'   input.
#'
#' @param x Vector (for now numeric only) typically supplied from
#'   \code{axTicks()}.
#' @param ... Currently not used
#'
#' @return A list with new re-formatted labels and function \code{AxisTitle} for
#'   customizing axis label.
#' @export
#'
#' @examples
#' AxisFormat(c(0, 0.015, 0.020, 0.025, 0.030))
AxisFormat <- function(x, ...) {
  UseMethod(generic = "AxisFormat")
}

#' @describeIn AxisFormat method for object class `numeric`.
#' @export
AxisFormat.numeric <- function(x, ...) {

  # to decide if numeric x contains a decimal; for now assumes if yes then x is
  # less than 1, may add functionality to mitigate assumption
  is_dec <- grepl(pattern = "\\.", x = x)

  # for string split below
  splt <- "^[1-9]{1, }"
  sng <- 1

  # only if `x` contains decimal, splt & sng change, so power becomes negative
  if (any(is_dec)) {
    splt <- "\\."
    sng <- -1
  }

  # split incoming `x` accordingly: if non-decimal, then how many 0s if decimal,
  # then how many digits to the right
  # s <- strsplit(as.character(x), split = splt)
  s <- strsplit(format(x, scientific = FALSE), split = splt)
  r <- vapply(s, FUN = `[`, FUN.VALUE = character(1L), 2)
  n <- nchar(r)

  # for the power of base 10
  # pwr <- max(n, na.rm = TRUE) * sng
  pwr <- min(n, na.rm = TRUE) * sng

  # e.g., 0.015 becomes 15 | 1500 becomes 1.5
  out <- x / 10^pwr

  # output
  list(
    # Labels = out,
    Labels = format(out),
    Pwr = pwr,
    Base = 10,
    AxisTitle = function(var) {
      if (pwr == 1) return(bquote(.(var) ~~ 'x' ~ 10))
      bquote(.(var) ~~ 'x' ~ 10^.(pwr))
    }
  )
}
# end AxisFormat.numeric

# added 07-Jan-2020 for custom base plots ---------------------------------

#' Create Custom Empty Base R Plot.
#'
#' @description \code{Plot} creates an empty plot with axes of suitable limits
#'    so as to include all data points. Plot created is then ready for points
#'    or lines or bars (etc.) overlay.
#'
#' @param data A dataframe with at least `x` & `y` data fields.
#' @param x A vector if `data` is NULL or field name in `data` for the x axis.
#' @param y A vector if `data` is NULL or field name in `data` for the y axis.
#' @param ... Passed to other methods. Currently not used.
#' @param yerr A vector if `data` is NULL or field name in `data` for the
#'    y axis error bars. Currently not such parameter for x error bars.
#' @param subset Logical expression to be evaluated in `data` or for `x` & `y`.
#' @param x0 Logical. True if x-axis minimum value should be 0. Default False.
#' @param y0 Logical. True if y-axis minimum value should be 0. Default False.
#' @param adjUsr Numeric. User to ajust \code{par("usr")} mostly to smaller
#'    size, as the default is 0.90 (thus a 10\% reduction).
#' @param xAsFactor Logical. True if x-axis should be a factor. Defaul = FALSE.
#'
#' @return A list (to be detailed later).
#' @export
#'
#' @examples
#' # coming soon.
Plot <- function(
  data = NULL, x, y, ..., yerr = NULL,
  subset = NULL, x0 = FALSE, y0 = FALSE, adjUsr = 0.90, xAsFactor = FALSE) {

  # for eventual subsetting of data or x&y
  s <- substitute(subset)

  # creates data from x&y if data is null
  if (is.null(data)) {

    b <- if (is.null(s)) TRUE else eval(s)

    if (is.null(yerr)) yerr <- rep(0L, length = length(b))

    xrng <- range(x[b], na.rm = TRUE)

    # if (xAsFactor) xrng <- factor(xrng, levels = unique(x[b]))

    n <- length(unique(x[b]))
    n <- Filter(f = Negate(is.na), x = n)

    # lvls <- sort(unique(x[b]), na.last = NA)

    yrng <- range(c(y[b] - yerr[b], y[b] + yerr[b]), na.rm = TRUE)
    d <- list(x = x[b], y = y[b], yerr = yerr[b])

  } else {

    # subset data according to `s`
    if (!is.null(s)) data <- data[eval(expr = s, envir = data), ]

    # to get x&y and error bars (if supplied) from data
    x <- as.character(substitute(x))
    y <- as.character(substitute(y))
    ye <- substitute(yerr)

    # so plot height takes in to account error on y-axis
    ye <- if (is.null(ye)) 0 else data[[as.character(ye)]]

    # ranges for plotting empty plot
    xrng <- range(data[[x]], na.rm = TRUE)

    # if (xAsFactor) xrng <- factor(xrng, levels = unique(data[[x]]))

    n <- length(unique(data[[x]]))
    n <- Filter(f = Negate(is.na), x = n)

    # lvls <- sort(unique(data[[x]]), na.last = NA)

    yrng <- range(c(data[[y]] - ye, data[[y]] + ye), na.rm = TRUE)
    d <- list(x = data[[x]], y = data[[y]], yerr = ye)
  }

  # for anticipation of bar plot starting at 0
  if (x0) xrng[1] <- 0
  if (y0) yrng[1] <- 0

  # for par("usr") default
  usr_default <- par("usr")

  # display an empty plot
  plot(
    x = if (xAsFactor) gl(n = n, k = 1, length = 2) else xrng,
    # x = if (xAsFactor) factor(xrng, levels = lvls) else xrng,
    # x = if (xAsFactor) factor(xrng, levels = seq_len(n)) else xrng,
    # x = if (xAsFactor) factor(c(n, n), levels = seq_len(n)) else xrng,
    y = yrng,
    type = "n",
    xaxt = "n",
    yaxt = "n",
    bty = "n",
    lty = 0, # needed when x is factor
    xlab = NA,
    ylab = NA
  )

  # to get plotting region after plot has been set
  pr <- par("usr")

  # for adjusting plotting region
  coords <- c(xrng, yrng)

  # sets new plotting region (smaller | larger) via `adjUsr` argument
  par(usr = ((pr - coords) * adjUsr) + coords)

  # for use in setting plot background to grey90 if desired
  u <- par("usr")

  out <- list(
    xrng = xrng,
    yrng = yrng,
    xticks = function() axTicks(side = 1),
    yticks = function() axTicks(side = 2),
    ticks = function(side, interval = NULL, offset = 0) {
      tk <- axTicks(side = side)
      if (is.null(interval)) return(tk)
      interval <- interval  %/% 1
      if (interval <= 1)
        stop("`interval` must be integer > 1.", call. = FALSE)
      g <- as.numeric(as.factor(tk))
      b <- (g %% interval) == (1 + offset)
      tk[b]
    },
    data = function() d,
    grid = function(xRng = FALSE, yRng = FALSE, nx = 1, ny = 1) {
      # p <- par(c("xaxp", "yaxp"))
      if (xRng) par(xaxp = c(xrng, diff(xrng) * nx))
      if (yRng) par(yaxp = c(yrng, diff(yrng) * ny))
      # grid with lwd = 1000 old way of setting grey background
      # now I prefer to use rect()
      # grid(col = "grey90", lwd = 1000, lty = 1)
      rect(
        xleft = u[[1]],
        ybottom = u[[3]],
        xright = u[[2]],
        ytop = u[[4]],
        col = "grey90",
        border = NA,
        lty = 1,
        lwd = 1
      )
      # sets grid lines according to xaxp & yaxp
      grid(col = "white", lwd = 1, lty = 1)
    },
    usr = list(
      default = usr_default,
      orig = pr,
      adjusted = u
    ),
    subetted = s
  )

  # for method development
  class(out) <- "plot_empty"
  out
}
# end PlotEmpty

#' Add Data to \code{Plot}.
#'
#' @description Add data points (lines, etc.) to empty plot with or without
#'    background plus grid lines.
#'
#' @param x An object of class `plot_empty`.
#' @param type Scalar character of either `p` (points), `l` (lines), or `h`
#'    (bars); see `help("plot.default")` for more.
#' @param ... Other parameters passed to \code{points}.
#' @param grid Logical. True (default) will display grid on `Plot`.
#' @param xRng Logical. False (default) keeps default `xaxp`. True uses
#'    c(range(x), diff(range(x))).
#' @param yRng Logical. False (default) keeps default `yaxp`. True uses
#'    c(range(y), diff(range(y))).
#' @param nx Integer (default 1) affecting number of vertical lines in grid.
#' @param ny Integer (default 1) affecting number of horizontal lines in grid.
#'
#' @return Nothing at the momement.
#' @export
#'
#' @examples
#' # coming soon.
points.plot_empty <- function(
  x, type = "p", ..., grid = TRUE, xRng = FALSE, yRng = FALSE, nx = 1, ny = 1) {

  if (grid) x$grid(xRng = xRng, yRng = yRng, nx = nx, ny = ny)

  points(
    x = x$data()[["x"]],
    y = x$data()[["y"]],
    type = type,
    lend = 1,
    ...
  )
}
# end points.plot_empty

#' Display Custom Axis for \code{Plot}.
#'
#' @description Creates custom axis tick labels for \code{Plot}.
#'
#' @param x An object of class `plot_empty`.
#' @param ... Curently not used.
#' @param side Integer either 1, 2, 3, or 4 (bottom, left, top, right).
#' @param cexAxis Numeric denoting size of axis tick labels.
#' @param labelAdj Numeric denoting placement of label to tick. Is passed to
#'    second value in par("mgp").
#' @param interval Integer > 1. Sets interval at which axis tick labels will
#'    be displayed.
#' @param offset Integer (positive) to offset where tick labels are placed.
#' @param format Logical. True to employ \code{AxisFormat} to reduce 0s before
#'    or after decimal when axis tick lables are large or small numbers.
#'    Default is False.
#'
#' @return Null (if format `FALSE`) otherwise \code{AxisFormat} output.
#' @export
#'
#' @examples
#' # coming soon.
Axis.plot_empty <- function(
  x, ..., side, cexAxis = 1, labelAdj = 1,
  interval = NULL, offset = 0, format = FALSE) {

  # for the 4 sides on a plot
  side <- match.arg(
    arg = as.character(side),
    # choices = as.character(1:4),
    choices = 1:4,
    several.ok = FALSE
  )

  # # get the function that will define x | y ticks
  # f <- x[["xticks"]]
  # if (side %in% c(2, 4)) f <- x[["yticks"]]

  frmt <- NULL

  tk <- x$ticks(side = side, interval = interval, offset = offset)

  if (format) frmt <- AxisFormat(tk)

  # for distance of tick label from tick
  par(mgp = c(3, labelAdj, 0))

  # display the axis
  axis(
    side = side,
    at = tk,
    # labels = tk,
    labels = if (format) frmt[["Labels"]] else format(tk),
    col = "transparent",
    col.ticks = "grey50",
    col.axis = "grey50",
    cex.axis = cexAxis,
    las = 1,
    tcl = -0.3
    # padj = -0.75,
    # gap.axis = -2,
    # tck = -0.04
  )

  invisible(frmt)
}
# end axis.plot_empty

#' @export
lines.plot_empty <- function(
  x, ..., grid = TRUE, xRng = FALSE, yRng = FALSE, nx = 1, ny = 1) {

  if (grid) x$grid(xRng = xRng, yRng = yRng, nx = nx, ny = ny)

  y0 <- x$data()[["y"]] - x$data()[["yerr"]]
  y1 <- x$data()[["y"]] + x$data()[["yerr"]]

  segments(x0 = x$data()[["x"]], y0 = y0, y1 = y1, ...)
  #
  # lines(
  #   x = rep(x$data()[["x"]], times = 2),
  #   y = c(y0, y1),
  #   type = "h",
  #   ...
  # )

  # ybar <- list(
  #   y0 = x$data()[["y"]] - x$data()[["yerr"]],
  #   y1 = x$data()[["y"]] + x$data()[["yerr"]]
  # )

  # lapply(ybar, FUN = function(y) {
  #
  #   lines(
  #     x = x$data()[["x"]],
  #     y = y,
  #     type = "l"
  #   )
  #
  # })

}
