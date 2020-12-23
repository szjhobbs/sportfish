# ******************************************************************************
# Created: 20-Ju1-2018
# Author:  J. DuBois
# Contact: jason.dubois@wildlife.ca.gov
# Purpose: This file contains descriptive statistic functions (e.g., standard
#          error; mean, sd, min, max)
# ******************************************************************************

#' Calculates descriptive statistics of \code{x}.
#'
#' @description \code{DescStat} provides basic descriptive statistics of
#'    numeric input. Will remove all NA values prior to calculations &
#'    will issue warning when doing so.
#'
#' @param x A numeric vector, preferably of length > 1.
#'
#' @return A list containing n (with & without NA values; NAll & N),
#'    min, max, mean, median, and variance.
#'
#' @note To get standard error run \code{sqrt(Var / N)}.
#'
#' @export
#'
#' @examples
#' DescStat(1:10)
#' DescStat(c(runif(10), NA))
DescStat <- function(x) {

  if (!is.numeric(x)) stop("x must be numeric.", call. = FALSE)

  # get n with NAs
  n_all <- length(x)

  # remove NAs for stats
  if (any(is.na(x))) {
    n <- sum(is.na(x))
    x <- x[!is.na(x)]
    warning("Removed ", n, " NA value(s).", call. = FALSE)
  }

  # standard error = sqrt(Var / N), if desired
  list(
    NAll = n_all,
    N = length(x),
    Min = min(x),
    Max = max(x),
    Avg = mean(x),
    Med = median(x),
    Var = var(x)
  )
}
# end DescStat

#' Cumulative Sum For Numeric Vectors With NA.
#'
#' @description \code{\link[base]{cumsum}} returns NA for all values at and
#'    after first NA. \code{CumsumNA} 'ignores' NA values.
#'
#' @param x Numeric vector.
#'
#' @return Cumulative sum of `x`, where any NA values remain NA.
#' @export
#'
#' @examples
#' CumsumNA(c(1:4, NA, 5:10))
CumsumNA <- function(x) {
  b <- is.na(x)
  if (all(b)) return(x)
  if (!any(b)) return(cumsum(x))
  x[b] <- 0
  res <- cumsum(x)
  res <- `is.na<-`(res, which(b))
  res
}
# end CumsumNA
