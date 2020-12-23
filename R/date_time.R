# ******************************************************************************
# Created: 06-Dec-2018
# Author:  J. DuBois
# Contact: jason.dubois@wildlife.ca.gov
# Purpose: This file contains functions to assist with a variety of
#          procedures & data clean up
# ******************************************************************************

#' Extract 4-digit Year Based on Starting Month.
#'
#' @description \code{GroupYear} groups a year according to a starting month.
#'   This function is useful for grouping values in year x & year x + 1 crossing
#'   over calendar year (e.g., months Oct-Sep).
#'
#' @param x Object of class `Date` or `data.frame`.
#' @param ... Currently not used.
#' @param year Field name in `x` when `x` is `data.frame` containing numeric
#'   4-digit year.
#' @param mon Field name in `x` when `x` is `data.frame` containing numeric 1-
#'   to 2-digit month (i.e., 1-12).
#' @param startMon Integer scalar value between 1 and 12.
#' @param direction Character of either "forward" or "backward" denoting whether
#'   year is moved forward (default; `+`) or backward (`-`).
#'
#' @return Numeric 4-digit year based on starting month.
#' @export
#'
#' @note \code{GroupYear} not yet equipped to control `end` month, so result is
#'   full 12 months based on starting month.
#'
#' @examples
#' # coming soon
GroupYear <- function(x, ...) {
  UseMethod(generic = "GroupYear")
}
# end GroupYear

#' @describeIn GroupYear Method for object class `Date`.
#' @export
GroupYear.Date <- function(x, ..., startMon) {

  if (!(startMon %in% 1:12)) {
    stop("`startMon` must be integer between 1 & 12", call. = FALSE)
  }

  # get year & month from date value
  y <- as.numeric(format(x, format = "%Y"))
  m <- as.numeric(format(x, format = "%m"))

  # how much off is startMon from 1, then how much is that different from the
  # month, if 0 or below group within desired year (i.e., y - 1)
  if (m - (startMon - 1) <= 0) return(y - 1)

  # return y if m - (startMon - 1) > 0
  y
}
# end GroupYear.Date

#' @describeIn GroupYear Method for object class `data.frame`.
#' @export
GroupYear.data.frame <- function(
  x, year, mon, ..., startMon, direction = c("forward", "backward")) {

  if (!(startMon %in% 1:12)) {
    stop("`startMon` must be integer between 1 & 12.", call. = FALSE)
  }

  direction <- match.arg(
    arg = direction,
    choices = c("forward", "backward"),
    several.ok = FALSE
  )

  # get year & month from data
  y <- x[[as.character(substitute(year))]]
  m <- x[[as.character(substitute(mon))]]

  if (!is.numeric(y) || !is.numeric(m)) {
    stop("`year` & `mon` must be numeric.", call. = FALSE)
  }

  # how much off is startMon from 1, then how much is that different from the
  # month, if 0 or below group within desired year (i.e., y - 1)
  # if (m - (startMon - 1) <= 0) return(y - 1)
  b <- m - (startMon - 1) <= 0

  if (any(is.na(b))) {
    n <- sum(is.na(b))
    msg <- sprintf("Month NA for %s records. Returning `year` as is.", n)
    warning(msg, call. = FALSE)
    b[is.na(b)] <- FALSE
  }

  switch(
    EXPR = direction,
    "forward" = y[b] <- y[b] + 1,
    "backward" = y[b] <- y[b] - 1
  )

  # return y
  y
}
# end GroupYear.data.frame
