# ******************************************************************************
# Created: 18-May-2020
# Author:  J. DuBois
# Contact: jason.dubois@wildlife.ca.gov
# Purpose: this file contains functions to handle Striped Bass creel census
#          data
# ******************************************************************************

#' Assign River Creel or Bay Creel Type Based on Census Date.
#'
#' @description Striped Bass creel census is bifurcated. River creel from
#'    Dec 16 - Jun 15 and Bay Creel from Jun 16 - Dec 15. This function
#'    assigns one or the other based on `date` argument.
#'
#' @param date Class Date or POSIX; sampling date (YYYY-MM-DD)
#'
#' @return List with Year, Month, and Type items, where Year is +1 if
#'    Month = 12 and Type = 'rvr'.
#' @export
#'
#' @examples
#' # coming soon.
CreelType <- function(date) {

  # assumes for now date is class POSIXct, will create methods at later date

  y <- as.numeric(format(date, format = "%Y"))
  m <- as.numeric(format(date, format = "%m"))

  # start & end dates for bay creel
  ss <- as.POSIXct(paste0(y, "-06-16"), tz = "UTC")
  ee <- as.POSIXct(paste0(y, "-12-15"), tz = "UTC")

  # Boolean (TRUE if between dates)
  b <- date >= ss & date <= ee

  # river creel (rvr) & bay creel (bay) if TRUE
  r <- rep("rvr", times = length(date))
  r[b] <- "bay"

  y[!b & m %in% 12] <- y[!b & m %in% 12] + 1

  # CYear = creel year to distinguish from calendar year
  list(
    CYear = y,
    Month = m,
    Type = r
  )
}
# end CreelType
