# ******************************************************************************
# Created: 17-Jan-2019
# Author:  J. DuBois
# Contact: jason.dubois@wildlife.ca.gov
# Purpose: This file contains functions to calculate tag return year based
#          on angler capture date & CDFW release date
# ******************************************************************************

#' Calculate Return Year for Angler Tag Returns.
#'
#' @description Angler tag returns are classified based on the difference (in
#'   days) between the reported capture date and recorded release (tagging)
#'   date. This difference is converted to years (by dividing by 365.25 [0.25
#'   accounts for leap year]) and is assigned as the `return year` for the
#'   returned tag. A tag returned within 365.25 days post released is classified
#'   as return year = 1, and so on. Return year is used (for example) in
#'   exploitation estimates, where only tags with return year = 1 are used.
#'
#' @param data Dataframe containg angler tag return information (at the very
#'   least tag, date captured, and date released).
#' @param dcap Field name (unquoted) in `data` containing capture date.
#' @param drel Field name (unquoted) in `data` containing release date.
#'
#' @note Date fields should be of class `Date` or `POSIX`.
#'
#' @return List. See `values`.
#'
#' @section values:
#'    \itemize{
#'       \item DAL Numeric vector. Difference in days between dates.
#'             (`DAL` = days at large)
#'       \item RetYear Numeric vector. Calculated return year.
#'       \item CountNAsDAL Numeric scalar. Count of `NA` values.
#'             (i.e., where RetYear or DAL could not be calculated.)
#'       \item CountNegativeDAL Numeric scalar. Count where capture date
#'             before release date (illogical).
#'       \item RecordNumsNegDAL Numeric vector. If `CountNegativeDAL` not 0,
#'             then herein are record numbers where capture date
#'             before release date.
#'    }
#'
#' @export
#'
#' @examples
#' # coming soon
TagRetYear <- function(data, dcap, drel) {

  dc <- as.character(substitute(dcap))
  dr <- as.character(substitute(drel))

  # to get difference in days between dates
  d <- difftime(time1 = data[[dc]], time2 = data[[dr]], units = "days")

  # for desired output
  class(d) <- "numeric"

  # negative days (d) not logical (must be entry or transcription error)
  # dlt0 <- sum(d[!is.na(d)] < 0)
  # recs <- which(d[!is.na(d)] < 0)

  flagged <- d < 0
  dlt0 <- sum(flagged, na.rm = TRUE)
  recs <- which(flagged)

  if (dlt0 > 0) {
    msg <- paste(
      "N = %d record(s) where %s before %s.",
      "Setting difference to NA for `RetYear` calculation."
    )

    warning(sprintf(msg, dlt0, dc, dr), call. = FALSE)
    d[flagged] <- NA
  }

  dNA <- sum(is.na(d))
  attr(dNA, which = "comment") <- "Post setting to NA any negative DAL."

  # to hold days at large (dal) for output before changing `d` below
  dal <- d
  attr(dal, which = "comment") <- "Negative values, if any, set to NA."

  # 0 days at large set to 0.1 for ceiling function so return year is not 0
  d[d %in% 0] <- 0.1

  # get return year (365.25 considers leap year and ceiling gives to nearest
  # whole number rounding up)
  ry <- ceiling(d / 365.25)

  attr(ry, which = "units") <- "years"

  out <- list(
    DAL = dal,
    RetYear = ry,
    CountNAsDAL = dNA,
    CountNegativeDAL = dlt0,
    RecordNumsNegDAL = recs,
    FlaggedRecs = flagged
  )

  class(out) <- "return_year"

  out
}
# end TagRetYear

#' Print Method for \code{TagRetYear}.
#'
#' @param x Object of class `return_year`.
#' @param ... Currently not used.
#'
#' @return Count of NA values and count of negative values, if any.
#' @export
#'
#' @examples
#' # coming soon
print.return_year <- function(x, ...) {
  cat(
    "Return year (or days at large) `NA`: ", x[["CountNAsDAL"]],
    "\nNegative days at large (set to `NA`): ", x[["CountNegativeDAL"]],
    sep = ""
  )
}
# end print.return_year
