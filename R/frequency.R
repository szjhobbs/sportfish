# ******************************************************************************
# Created: 21-Aug-2018
# Author:  J. DuBois
# Contact: jason.dubois@wildlife.ca.gov
# Purpose: This file contains functions or methods to get frequency
#          distributions of select variables (e.g., age; length)
# ******************************************************************************

#' @keywords internal
#'
BinWidth <- function(w) {

  function(x) {
    r <- range(x)
    seq(from = r[1], to = r[2] + w, by = w)
  }

}
# end BinWidth

#' Create a frequency distribution of \code{x} by \code{binWidth}.
#'
#' @description \code{Frequency} is a custom wrapper (mostly) for
#'   \code{\link[graphics]{hist}}. It simplifies input to only data and bin
#'   width. Bins are always left included but not right, as in \code{`[a, b)`}.
#'   \code{Frequency} removes \code{NA} values (with warning) prior to binning.
#'
#' @param x A postive numeric vector to be binned according to \code{binWidth}.
#' @param binWidth A numeric scalar (integer) defining the bin size.
#' @param xRange A numeric vector of length 2, supplying 'from' and 'to' for
#'   customized x-axis range. Default NULL. Recommended for consistent
#'   comparison of multiple histograms.
#'
#' @return A list (of class \code{Frequency} & class
#'   \code{\link[graphics]{hist}}) containing proportion & count frequencies.
#'
#' @section Values:
#'
#'   \describe{ \item{others}{see \code{\link{hist}}}. \item{xna}{number of NA
#'   values removed}. \item{xname}{deparsed name of \code{x}, replaces
#'   \code{hist} \code{xname}} \item{binw}{value passed to \code{binWidth}}
#'   \item{xstats}{ function when run will yield descriptive statistics of
#'   \code{x} } }
#'
#' @note Method \code{summary} written for class Frequency. Returns dataframe
#'   with fields Bin, Count, Density, Proportion. Print method also writted for
#'   class Frequency.
#'
#' @export
#'
#' @examples
#' #Frequency(trammel_catch[["FL"]], binWidth = 5)
Frequency <- function(x, binWidth, xRange = NULL) {

  xna <- sum(is.na(x))

  if (xna != 0) {
    w <- sprintf("Removed %s value(s) due to NA.", xna)
    x <- x[!is.na(x)]
    warning(w, call. = FALSE, noBreaks. = FALSE)
  }

  if (is.null(xRange)) {
    brks <- BinWidth(binWidth)
  } else {
    stopifnot(length(xRange) == 2)
    brks <- seq(from = xRange[1], to = xRange[2] + binWidth, by = binWidth)
  }

  # plot FALSE for custom plotting later
  h <- hist(x, breaks = brks, right = FALSE, plot = FALSE)

  # for more information
  h$xna <- xna
  h$xname <- deparse(substitute(x))
  h$binw <- binWidth
  # h$xstats <- DescStat(x)
  h$xstats <- function() DescStat(x)

  # return list of class histogram
  # class(h) <- c(class(h), "Frequency")
  class(h) <- c("Frequency", class(h))
  h
}
# end Frequency

#' @describeIn Frequency \code{summary} method (See notes).
#' @param object Object of class \code{Frequency}.
#' @param ... Passed on to other methods.
#' @export
summary.Frequency <- function(object, ...) {

  n <- length(object[["breaks"]])

  data.frame(
    Bin = object[["breaks"]][-n],
    Count = object[["counts"]],
    Density = object[["density"]],
    Proportion = object[["density"]] * object[["binw"]]
  )
}
# end summary.Frequency

#' @describeIn Frequency \code{print} method (See notes).
#' @export
print.Frequency <- function(x, ...) {

  n <- length(x[["breaks"]])

  cat("Bins by", x[["binw"]], "| Count per bin:\n", sep = " ")
  print(setNames(object = x[["counts"]], nm = x[["breaks"]][-n]))
}
# end print.Frequency

#' Generate base R histogram from object of class \code{\link{Frequency}}.
#'
#' @description Plot a histogram using \code{hist} (esentially calling next
#'   method on \code{plot}, which calls \code{plot.histogram}). Display contains
#'   sample size along with some descriptive statistics.
#'
#' @param x An object of class Frequency (& histogram).
#' @param ... Passed on to other methods.
#' @param med Logical. If true (default) plot will display vertical line
#'   denoting median value.
#' @param xTL Logical. If true (default) plot will include x-axis tick labels.
#' @param yTL Logical. If true (default) plot will include y-axis tick labels.
#' @param xTitle Logical. If true (default) plot will include x-axis title.
#' @param yTitle Logical. If true (default) plot will include y-axis title.
#' @param addN Logical. If true (default) plot will include `N` value outside
#'   plot area (top left).
#' @param maxY Numeric. If null (default) plot uses \code{max(x[["density"]])}.
#'
#' @return Displays a histogram using frequency item in \code{x}.
#'
#' @export
#'
#' @examples
#' #freq <- Frequency(trammel_catch[["FL"]], binWidth = 5)
#' #plot(freq)
plot.Frequency <- function(
  x, ..., med = TRUE, xTL = TRUE, yTL = TRUE,
  xTitle = TRUE, yTitle = TRUE, addN = TRUE, maxY = NULL) {

  # TODO: allow for custom x axis title
  # TODO: allow for selecting either freq or density
  # TODO: adjust y-axis tick labels accordingly (freq or density)
  # TODO: add background color and (or) gridlines (like ggplot)
  # TODO: control xaxis tick labels to every other one
  # TODO: maybe draw plot first, then add lines using lines.histogram
  # TODO: (big one) color bars based on some group (e.g., slot limit)
  # TODO: add more to mtext if necessary

  # m <- par()$mar
  # # a <- par()$yaxs
  #
  # # c(bottom, left, top, right)
  # par(mar = c(5, 4, 1.7, 2) + 0.1)
  #
  # on.exit(par(mar = m))

  # variables here for convenience, used throughout function
  xbrks <- x[["breaks"]]
  rngb <- range(xbrks)
  ds <- x$xstats()

  if (is.null(maxY)) maxY <- max(x[["density"]])

  # empty plot for layering
  plot(
    x = rngb,
    # y = range(x[["density"]]),
    y = range(0, maxY),
    type = "n",
    xaxt = "n",
    yaxt = "n",
    ylab = NA,
    xlab = NA
    # xlab = noquote(x[["xname"]]),
    # ylab = "Density"
  )

  # for custom axes & grid lines (white lines based on xaxp & yaxp)
  par(xaxp = c(rngb, length(xbrks) - 1))

  y_axis_ticks <- axTicks(side = 2)
  custom_y <- AxisFormat(y_axis_ticks)

  grid(lwd = 1000, col = "grey90")
  grid(lty = 1, col = "white", lwd = 1)

  # option to add median (as vertical line)
  if (med) abline(v = ds[["Med"]], col = "orange2", lty = 2)

  # add histogram to plot; bars filled with some transparency for ease of
  # viewing over grid lines & future coloration denoting particular bins (e.g.,
  # slot-limit sized White Sturgeon)
  fill <- rgb(red = 0, green = 0, blue = 0, alpha = 0.5)
  lines(x = x, freq = FALSE, col = fill, border = NA)

  # for possible use in custom axes
  col_ax <- "transparent"
  col_tcks <- "grey30"
  tck_size <- -0.3

  # custom x-axis ticks and labels ----------------------------------------

  # for ticks labels
  if (xTL) {
    axis(
      side = 1,
      at = xbrks,
      col = col_ax,
      col.ticks = col_tcks,
      tcl = tck_size
    )
  }

  # for axis title
  if (xTitle) {
    mtext(
      text = noquote(x[["xname"]]),
      side = 1,
      line = 2
    )
  }

  # custom y-axis ticks and labels ----------------------------------------

  if (yTL) {
    axis(
      side = 2,
      at = y_axis_ticks,
      labels = custom_y[["Labels"]],
      las = 1,
      col = col_ax,
      col.ticks = col_tcks,
      tcl = tck_size
    )
  }

  if (yTitle) {
    mtext(
      text = custom_y$AxisTitle("Density"),
      side = 2,
      line = 2
    )
  }

  # adding N to plot ------------------------------------------------------

  if (addN) {
    txt <- sprintf("total count: %s", ds[["N"]])
    mtext(text = txt, adj = 0)
  }

  invisible(custom_y)
}
# end plot.Frequency
