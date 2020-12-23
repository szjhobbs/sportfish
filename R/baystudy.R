# ******************************************************************************
# Created: 20-Dec-2018
# Author:  J. DuBois
# Contact: jason.dubois@wildlife.ca.gov
# Purpose: This file contains functions associated with Bay Study data clean-up
#          & analytics
# ******************************************************************************

#' Calculate Bay Study Tow Volume or Tow Area.
#'
#' @description \code{TowMetric} calculates tow volume if mid-water trawl or
#'   other and tow area if otter trawl. Does so by multiplying constants by
#'   (flow) meter value or tow distance.
#'
#' @param data Dataframe of Bay Study data with at least net type, meter value,
#'   and distance towed.
#' @param net Name of field in data holding net type info.
#' @param meter Name of field in data holding meter info.
#' @param distance Name of field in data holding distance.
#'
#' @return Numeric vector of tow area or tow volume (depending upon net type).
#' @export
#'
#' @references Bay Study metadata (insert link).
#'
#' @examples
#' # coming soon.
TowMetric <- function(data, net, meter, distance) {

  # field names from data
  n <- as.character(substitute(net))
  m <- as.character(substitute(meter))
  d <- as.character(substitute(distance))

  # boolean values for "filtering"
  b1 <- data[[n]] %in% 1 # net = 1 (mid-water)
  b2 <- data[[n]] %in% 2 # net = 2 (otter)
  b0 <- !b1 & !b2        # net = other

  # to hold function output
  val <- rep(NA, times = nrow(data))

  # where applicable replace NAs with values
  val[b1] <- data[[m]][b1] * 0.2875
  val[b2] <- data[[d]][b2] * 6338
  val[b0] <- data[[m]][b0] * 0.0103

  val
}
# end TowMetric

#' A "Where Clause" or Filter for Bay Study Tows.
#'
#' @description Flags "valid" tows per select criteria.  Typically, excluded are
#'   tows 52, 53, and 58; station #141 (only sampled a few months, then
#'   dropped); and stations #317 and #319 in 1980 and 1981-01 (split surveys,
#'   used stations #324 and #326, which were the same locations, sampled 1-2
#'   weeks later).
#'
#' @param data Dataframe of Bay Study data with at least station, year, survey,
#'   and tow.
#' @param station Name of field in data holding station info.
#' @param year Name of field in data holding year (i.e., extracted from date).
#' @param survey Name of field in data holding survey number.
#' @param tow Name of field in data holding tow nubmer.
#'
#' @return List containing flagged records (-1 = flagged; O = OK to use) and
#'   number of records flagged.
#' @export
#'
#' @references Bay Study metadata (insert link).
#'
#' @examples
#' # coming soon.
FlagTows <- function(data, station, year, survey, tow) {

  st <- as.character(substitute(station))
  yr <- as.character(substitute(year))
  sy <- as.character(substitute(survey))
  tw <- as.character(substitute(tow))

  # boolean variables for ease of subsetting
  bool_stat_317_319 <- data[[st]] %in% c(317, 319)
  bool_stat_141 <- data[[st]] %in% 141
  bool_1980 <- data[[yr]] %in% 1980
  bool_1981 <- data[[yr]] %in% 1981
  bool_gt1981 <- data[[yr]] > 1981
  bool_surv_1 <- data[[sy]] %in% 1
  bool_tows <- data[[tw]] %in% c(52, 53, 58)

  # variables to hold final output
  flag <- vector(mode = "integer", length = nrow(data))

  # checks for stations 317 & 319 sampled in 1980
  # datsub <- rbind(datsub, dat[bool_stat_317_319 & bool_1980, ])
  flag[bool_stat_317_319 & bool_1980] <- -1L

  # checks for stations 317 & 319 sampled in 1981 on survey 1
  # datsub <- rbind(datsub, dat[bool_stat_317_319 & bool_1981 & bool_surv_1, ])
  flag[bool_stat_317_319 & bool_1981 & bool_surv_1] <- -1L

  # checks for station 141 sampled after 1981
  # datsub <- rbind(datsub, dat[bool_stat_141 & bool_gt1981, ])
  flag[bool_stat_141 & bool_gt1981] <- -1L

  # checks for tow numbers 52, 53, & 58 - flags irrespective of station or year
  # datsub <- rbind(datsub, dat[bool_tows, ])
  flag[bool_tows] <- -1L

  # function output
  list(
    # DataFlagged = data[flag == -1L, ],
    Flag = flag,
    NumberFlagged = abs(sum(flag))
  )
}
# end FlagTows
