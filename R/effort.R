# ******************************************************************************
# Created: 12-Dec-2018
# Author:  J. DuBois
# Contact: jason.dubois@wildlife.ca.gov
# Purpose: This file contains functions to calculate fishing effort given such
#          inputs as time start, time end, and gear information
# ******************************************************************************

# Sturgeon effort functions -----------------------------------------------

#' Calculates Fishing Times of Sturgeon Trammel Net.
#'
#' @description The CDFW Sturgeon mark-recapture study uses trammel nets to
#'   catch sturgeon. Times are recorded four times: twice during deployment and
#'   twice during retrieval. \code{TimeDiffStu} calculates time differences
#'   between retrieval and deployment to ultimately calculate fishing effort.
#'   See 'Details' for further information.
#'
#' @param sStart date-time object. Time first bit of webbing entered water.
#' @param sEnd date-time object. Time last bit of webbing entered water.
#' @param rStart date-time object. Time first bit of webbing pulled from water.
#' @param rEnd date-time object. Time last bit of webbing pulled from water.
#'
#' @return list. See 'Details' for desription of list elements.
#'
#' @details Calculated differences are in hours. The returned list contains 5
#'   such calculations as explained below.
#'
#'   \itemize{
#'      \item TFT (total fishing time)
#'      \item ST (soak time)
#'      \item TST (true soak time)
#'      \item ETST (estimated true soak time)
#'      \item TOEST (true or estimated soak time)
#'   }
#'
#'   \deqn{TFT = \code{rEnd} - \code{sStart}}
#'   \deqn{ST = \code{rEnd} - \code{sEnd}}
#'   \deqn{TST = (\code{rStart} - \code{sEnd}) +
#'         0.5 * (\code{rEnd} - \code{rStart}) +
#'         0.5 * (\code{sStart} - \code{sEnd})}
#'   \deqn{ETST = 0.138 + (0.687 * \code{ST})}
#'
#'   If \code{ST} is NA, then \code{ETST} defaults to 1.067. If \code{TST} is
#'   NA, then \code{ETST} is used.
#'
#'   Return list also includes unit of time (currently hours) and function to
#'   calculcate effort using \code{TOEST}.
#'
#'   Time difference calculations created by M. Donnellan. (<some reference
#'   to metadata here for further understanding of calcs.>)
#'
#' @export
#'
#' @examples
#' # coming soon.
TimeDiffStu <- function(sStart, sEnd, rStart, rEnd) {

  # units for difftime() output
  u <- "hours"

  # calculate total fishing time (tft)
  tft <- difftime(time1 = rEnd, time2 = sStart, units = u)

  # calculate soak time (st)
  st <- difftime(time1 = rEnd, time2 = sEnd, units = u)

  # calculate true soak time (tst)
  tst <- difftime(time1 = rStart, time2 = sEnd, units = u) +
    0.5 * difftime(time1 = rEnd, time2 = rStart, units = u) +
    0.5 * difftime(time1 = sEnd, time2 = sStart, units = u)

  # calculate estimated true soak time (etst) (if st is NA, set etst to 1.067)
  etst <- 0.138 + (0.687 * as.numeric(st))
  etst[is.na(etst)] <- 1.067

  # Calculate true or estimated soak time (toest) (if toest is NA, set to etst)
  toest <- as.numeric(tst)
  toest[is.na(toest)] <- etst[is.na(toest)]

  out <- list(
    TFT = as.numeric(tft),
    ST = as.numeric(st),
    TST = as.numeric(tst),
    ETST = etst,
    TOEST = toest,
    Unit = u,
    NetEffort = function(netLength) toest * netLength
  )

  # class(out) <- "TimeDiffStu"
  out
}
# end TimeDiffStu

#' Calculates Fishing Effort for Sturgeon Trammel Nets.
#'
#' @description \code{EffortStu} will calculate fishing effort for each net set
#'    given fishing times and (trammel) net length. Fishing effort is the
#'    denominator when calculating CPUE (or catch per unit effort).
#'
#' @param data Dataframe containing (at least) net set & net retrieval time and
#'    net length (typically in fathoms).
#' @param sStart date-time object. Time first bit of webbing entered water.
#' @param sEnd date-time object. Time last bit of webbing entered water.
#' @param rStart date-time object. Time first bit of webbing pulled from water.
#' @param rEnd date-time object. Time last bit of webbing pulled from water.
#' @param netLength Numeric value given length of net fishing
#'    (e.g., 100 fathoms).
#'
#' @return list with the following elements:
#'    \itemize{
#'       \item Effort (fishing effort)
#'       \item TimeFished (in hours; total fishing hours)
#'       \item SoakTime (in hours)
#'       \item TimeUnit (characater)
#'       \item NetLength (the variable in \code{data} containing net length)
#'    }
#'
#' @export
#'
#' @examples
#' # coming soon.
EffortStu <- function(data, sStart, sEnd, rStart, rEnd, netLength) {

  # capture arguments from within data
  ss <- as.character(substitute(sStart))
  se <- as.character(substitute(sEnd))
  rs <- as.character(substitute(rStart))
  re <- as.character(substitute(rEnd))
  nl <- as.character(substitute(netLength))

  td <- TimeDiffStu(
    sStart = data[[ss]],
    sEnd = data[[se]],
    rStart = data[[rs]],
    rEnd = data[[re]]
  )

  # calculate effort based on time fished & net length
  e <- td$NetEffort(data[[nl]])

  # attr(e, which = "metadata") <- sprintf("Time unit: ", td[["Unit"]])

  out <- list(
    Effort = e,
    TimeFished = td[["TFT"]],
    SoakTime = td[["ST"]],
    TimeUnit = td[["Unit"]],
    NetLength = nl
  )

  class(out) <- "EffortStu"
  out
}
# end EffortStu

summary.EffortStu <- function(object, ...) {

}

# Striped Bass effort functions -------------------------------------------

#' Calculates Fishing Time in Hours.
#'
#' @description A simple function using \code{base::difftime} to calculate in
#'   hours time fished for fyke traps or gill nets. For convenience, output is
#'   converted to numeric rather than class "difftime."
#'
#' @param sTime A date-time object denoting fishing start time.
#' @param eTime A date-time object denoting fishing end time.
#'
#' @return Value(s) of class numeric given eTime - sTime.
#' @export
#'
#' @examples
#' # coming soon
HoursSb <- function(sTime, eTime) {
  u <- "hours"
  h <- difftime(time1 = eTime, time2 = sTime, units = u)

  # to make hours numeric only, not difftime
  class(h) <- "numeric"
  h
}
# end HoursSb

#' Counts Fyke Traps or Gill Nets Fished During Desired Period.
#'
#' @description A simple function to count fyke traps or gill nets fished during
#'   specified period. Period is specified (created) by user, not within this
#'   function.
#'
#' @param netNum Numeric or character value given net set or (fyke) trap number.
#'
#' @return A list with the following values:
#' \itemize{
#'   \item RecordCount (number of values)
#'   \item NetCount (record count for unique net numbers)
#'   \item NetCountDup (count of traps or nets fished more than once in a day)
#'   \item UniqueNets (trap or net numbers - unique)
#'   \item DupNets (trap or net numbers - non-unique)
#' }
#'
#' @export
#'
#' @examples
#' # coming soon
CountNets <- function(netNum) {

  nets <- unique(netNum)
  dup_nets <- duplicated(netNum)

  list(
    RecordCount = length(netNum),
    NetCount = length(nets),
    NetCountDup = sum(dup_nets),
    UniqueNets = nets,
    DupNets = netNum[dup_nets]
  )
}
# end CountNets

#' Calculates Fishing Effort for Striped Bass Fyke Traps & Gill Nets.
#'
#' @description Using start time and end time values, \code{EffortSb} calculates
#'   in hours the time fished for either fyke traps or gill nets. The output is
#'   then paired with catch data to calculate CPUE (or catch per unit effort).
#'
#' @param data A dataframe containing at least start time, end time, and net
#'   number.
#' @param sTime A datetime variable within \code{data}. Start time fishing net
#'   or trap.
#' @param eTime A datetime variable within \code{data}. End time fishing net or
#'   trap.
#' @param netNum Numeric or charater value denoting number of net or trap.
#'
#' @return A list of class \code{EffortSb} with the following values:
#' \itemize{
#'   \item Hours (fishing effort in hours; total)
#'   \item HoursAvg (average fishing hours)
#'   \item HoursVar (variance fishing hours)
#'   \item Days (count of days fished)
#'   \item NetSets (count of nets or traps fished)
#'   \item NetCount (<add explanation>)
#'   \item NetCountDup (counts of traps
#'         [mostly] fished more than once per)
#' }
#'
#' @export
#'
#' @examples
#' # coming soon
EffortSb <- function(data, sTime, eTime, netNum) {

  s <- as.character(substitute(sTime))
  e <- as.character(substitute(eTime))
  n <- as.character(substitute(netNum))

  hr <- HoursSb(sTime = data[[s]], eTime = data[[e]])
  hr <- Filter(f = Negate(is.na), x = hr)

  ct <- CountNets(netNum = data[[n]])

  days <- length(unique(as.Date(data[[e]])))

  out <- list(
    Hours = sum(hr),
    HoursAvg = mean(hr),
    HoursVar = var(hr),
    Days = days,
    NetSets = ct[["RecordCount"]],
    NetCount = ct[["NetCount"]],
    NetCountDup = ct[["NetCountDup"]]
  )

  # set class & return list output
  class(out) <- "EffortSb"
  out
}
# end EffortSb
