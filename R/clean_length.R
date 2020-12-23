# ******************************************************************************
# Created: 06-Dec-2018
# Author:  J. DuBois
# Contact: jason.dubois@wildlife.ca.gov
# Purpose: This file contains functions to assist with cleaning up (prior to
#          analytics) CDFW sturgeon data stored on tier-3 server
# ******************************************************************************

MaxYearIndex <- function(year, maxYear) {
  match(TRUE, table = year <= maxYear)
}
# end MaxYearIndex

#' Assign Each Fish to Length Group.
#'
#' @description Assigns for each fish 'sub' (sub-legal), 'leg' (legal), 'ovr'
#'   (over-legal) based on fishing regulations and measured fork length or total
#'   length (cm). If length is NA, then 'unk' (unknown) is used instead. Special
#'   case for Green Sturgeon using 'not' post 2006, when it be came illegal to
#'   fish for (and keep) said species.
#'
#' @param data Dataframe containing (at least) fish length & catch date (year).
#' @param year Field name (not quoted) in \code{data} that holds capture
#'   (collection) year.
#' @param species Field name (not quoted) in \code{data} that holds species.
#' @param tl Field name (not quoted) in \code{data} that holds total length.
#' @param fl Field name (not quoted) in \code{data} that holds fork length.
#' @param regTable Dataframe of regulation changes with at least the following
#'   fields: "MaxYear"; "Species"; "MinLen"; "MaxLen"; and "LenType". Default
#'   is `su` and gets internal `RegsSturgeon`. Other choice is `sb`
#'   and will get internal `RegsStriper`.
#'
#' @return A character vector with length = \code{nrow(data)} & possible values
#'   of 'sub', 'leg', 'ovr', 'unk', or 'not'.
#' @export
#'
#' @examples
#' # coming soon
LengthGroup <- function(
  data, year, species = NULL, tl = NULL, fl = NULL, regTable = c("su", "sb")) {

  # TODO: ensure tl and (or) fl are numeric along with year too
  # TODO: (16-Jan-2019) for now using RegsSturgeon will work but
  #       what about when using for Striped Bass

  # get year from data for comparison with regTable
  y <- eval(expr = substitute(year), envir = data)

  rt <- match.arg(arg = regTable, choices = c("su", "sb"), several.ok = FALSE)

  # get internal regulation data table
  switch(
    EXPR = rt,
    "su" = regTable <- RegsSturgeon,
    "sb" = regTable <- RegsStriper
  )

  mxy <- regTable[["MaxYear"]]

  # get index matching year (y) to max year in regTable
  i <- vapply(y, FUN = MaxYearIndex, FUN.VALUE = numeric(1L), mxy)

  # for when regs changed to fork length measurements (sturgeon only)
  b_fork <- regTable[["LenType"]][i] == "fork"

  # to hold function output (unk = unknown, cannot group length)
  out <- rep("unk", times = length(i))

  # for NULL checking & if not NULL data extraction
  sp <- substitute(expr = species)
  tl <- substitute(expr = tl)
  fl <- substitute(expr = fl)

  if (is.null(tl) & is.null(fl))
    stop("Must supply either `fl` or `tl` or both.", call. = FALSE)

  if (!is.null(tl) && is.null(fl)) l <- eval(expr = tl, envir = data)
  if (is.null(tl) && !is.null(fl)) l <- eval(expr = fl, envir = data)
  # for when both are NOT NULL
  if (!is.null(tl) && !is.null(fl)) {
    l <- eval(expr = tl, envir = data)
    fl <- eval(expr = fl, envir = data)
    l[b_fork] <- fl[b_fork]
  }

  # booleans for slot size limits
  b_lt_min <- l < regTable[["MinLen"]][i]
  b_gt_max <- l > regTable[["MaxLen"]][i]

  # set sub, leg, & ovr accordingly
  out[b_lt_min] <- "sub"
  out[!b_lt_min & !b_gt_max] <- "leg"
  out[b_gt_max] <- "ovr"

  if (!is.null(sp)) {
    sp <- eval(expr = sp, envir = data)
    # if y > 2006 and species green out = "not"
    # (i.e., can't take greens y > 2006)
    bool_gst <- grepl(pattern = "^G", x = sp, ignore.case = TRUE) & y > 2006
    out[bool_gst] <- "not"
  }

  # function output
  out
}
# end LengthGroup

#' Converts Total Length to Fork Length or Fork Length to Total Length.
#'
#' @description Mostly this function applies to California-based sturgeon and
#'   mostly because in 2013 regulations changed from total length to fork length
#'   (cm). The CDFW now measures fork length, so this function facilitates
#'   conversion between years allowing for more consistent analytics.
#'
#' @param data Dataframe containing (at least) length (fork & total) & species.
#' @param tl Field name (not quoted) in \code{data} that holds total length.
#' @param fl Field name (not quoted) in \code{data} that holds fork length.
#' @param sp Field name (not quoted) in \code{data} that holds species.
#' @param convertTo Character scalar option of either "TL" or "FL", the length
#'   type converting to.
#'
#' @return A numeric vector with length = \code{nrow(data)} where data are total
#'   length or fork length per choice in \code{convertTo}. Where converting to
#'   length already exists, that length it used rather than converting. If both
#'   TL & FL are NA, then returned length is also NA.
#'
#' @note Conversions use linear regression models (one for White Sturgeon
#'   another for Green Sturgeon). See References for source of models.
#'
#'   \deqn{FL_{WST}=-1.2162 + (0.9036 * TL_{WST})}
#'   \deqn{TL_{GST}=-4.6131 + (1.1374 * FL_{GST})}
#'
#' @references coming soon for equations
#'
#' @export
#'
#' @examples
#' # coming soon
ConvertLength <- function(data, tl, fl, sp, convertTo = c("TL", "FL")) {

  # TODO: rounding of final output ?
  # TODO: what happens if species is NA ?
  # TODO: attributes for which values converted ?

  convertTo <- match.arg(arg = convertTo, choices = c("TL", "FL"))

  # constants for conversion (slopes[SLP] & intercepts[INT] from linear
  # regression) for White Sturgeon (W) and Green Sturgeon (G)
  SLPW <- 0.9036
  INTW <- -1.2162
  SLPG <- 1.1374
  INTG <- -4.6131

  # get values from data
  # could do: tl <- substitute(expr = tl, env = data)
  tl <- data[[as.character(substitute(tl))]]
  fl <- data[[as.character(substitute(fl))]]
  sp <- data[[as.character(substitute(sp))]]

  # check lengths for NA
  tl_na <- is.na(tl)
  fl_na <- is.na(fl)

  # check species for White Sturgeon; assume if not White, species is Green (a
  # safe assumption as we'll run this function after species clean up)
  sp_wst <- grepl(pattern = "^W", x = sp, ignore.case = TRUE)

  # function output depending upon "convertTo" option
  switch (convertTo,
    "TL" = {
      bwst <- tl_na & sp_wst
      bgst <- tl_na & !sp_wst
      tl[bwst] <- (fl[bwst] - INTW) / SLPW
      tl[bgst] <- INTG + (SLPG * fl[bgst])
      return(tl)
    },
    "FL" = {
      bwst <- fl_na & sp_wst
      bgst <- fl_na & !sp_wst
      fl[bwst] <- INTW + (SLPW * tl[bwst])
      fl[bgst] <- (tl[bgst] - INTG) / SLPG
      return(fl)
    }
  ) # end switch
}
# end ConvertLength
