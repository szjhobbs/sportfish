# ******************************************************************************
# Created: 31-Oct-2019
# Author:  J. DuBois
# Contact: jason.dubois@wildlife.ca.gov
# Purpose: This file contains functions or methods to get tag recapture info
#          from Striped Bass or Sturgeon datasets. Functions here in work on
#          recaptured data to get release & recapture years, among other infi
# ******************************************************************************

#' Get Sturgeon Recapture Summary Data.
#'
#' @description The CDFW Sturgeon Mark-Recapture Study annually affixes a single
#'   disc tag (mark) to many sturgeon. Sometimes marks from within season or
#'   previous seasons are recaptured and the disc number recorded (along with
#'   other pertinent data). \code{StuRecap()} makes easier the process of
#'   getting (for each tag) release year, recapture year, & days-at-large (DAL),
#'   along with other metrics. Summary data are then used to construct a
#'   frequency table of sorts with the count of tag recaptures (by recapture
#'   year) for each release year.
#'
#' @param x Object of class `RecapData` or class `StuRecap`.
#' @param ... Passed to other methods or functions. Currently not used.
#' @param species Scalar characater either 'w' (default) or 'g'. Defines
#'    species either White Sturgeon or Green Sturgeon.
#' @param lencat Scalar character with choices 's', 'l', or 'o'. Defines
#'    length category for desired subsetting. Sub-legal (s), legal (l), or
#'    over-legal (o) are based on length (and existing regulations) at time
#'    fish was tagged. Default (`NULL`) includes all length categories.
#' @param minDAL Integer. Defines minimum days-at-large post tag & release.
#'    Oftentimes it is best (for minimizing biases) when tagged fish mixes
#'    with non-tagged fish. Limiting to at least ~30 days-at-large to define
#'    a recapture may mitigate biases. Default value (-1L) does not subset
#'    for DAL.
#'
#' @export
#'
#' @examples
#' # coming soon.
StuRecap <- function(x, ...) {
  UseMethod(generic = "StuRecap")
}

#' @describeIn StuRecap for object class `RecapData`.
#' @return List with nine items.
#'  \describe{
#'    \item{RelYr}{4-digit release year}
#'    \item{RecYr}{4-digit recapture year(s)}
#'    \item{RelDate}{release date (YYYY-MM-DD)}
#'    \item{RecDate}{recapture date(s) (YYYY-MM-DD)}
#'    \item{DAL}{days at large; RecDate - RelDate}
#'    \item{LenCat}{length category at tagging: sub; leg; ovr}
#'    \item{Species}{species (either white or green)}
#'    \item{DupTag}{see note}
#'    \item{NRecs}{number of records in recapture data}
#'  }
#'
#' @note In 2015, we inadvertently released duplicate tag numbers released in
#'    2011 & 2012. To date, duplicate tags `HH2141` & `HH2145` are the only two
#'    that have been recaptured. These are handled specially within
#'    \code{StuRecap.RecapData()} to account for two separate release dates.
#' @export
StuRecap.RecapData <- function(x, ...) {

  n <- nrow(x)
  spc <- unique(x[["Species"]])

  if (n <= 1)
    stop("`Data` must contain more than one record.", call. = FALSE)

  if (length(spc) > 1)
    stop("`Data` must contain only one species.", call. = FALSE)

  out <- list(
    RelYr = NA_integer_,
    RecYr = NA_integer_,
    RelDate = NA,
    RecDate = NA,
    DAL = NA_integer_,
    LenCat = NA_character_,
    Species = spc,
    DupTag = FALSE,
    NRecs = n
  )

  x <- x[order(x[["RelDate"]], x[["NetSet"]]), ]
  x$RelDate <- as.Date(x[["RelDate"]])

  if (n == 2) {
    # complete the list for output
    out$RelYr <- x[["RelYear"]][[1]]
    out$RecYr <- x[["RelYear"]][[2]]
    out$RelDate <- x[["RelDate"]][[1]]
    out$RecDate <- x[["RelDate"]][[2]]
    out$DAL <- as.integer(diff(x[["RelDate"]]))
    out$LenCat <- x[[1, "LenCat"]]
  }

  if (n > 2) {
    itype <- which(x[["StuType"]] %in% "Tag")
    cls <- c("TagNum", "ShedTag", "WasRetag")
    dups <- any(duplicated(x[itype, cls]))

    if (dups) {
      # return("I am a dup.")
      if (n > 3) return("dup records > 3. cannot process.")

      retag <- which(x[["WasRetag"]] %in% "Yes")

      if (length(retag) > 0) x$StuType[retag] <- "Recap"

      rec <- which(x[["StuType"]] %in% "Recap")
      tag <- rec - 1

      # complete the list for output
      out$RelYr <- x[["RelYear"]][[tag]]
      out$RecYr <- x[["RelYear"]][rec]
      out$RelDate <- x[["RelDate"]][[tag]]
      out$RecDate <- x[["RelDate"]][rec]
      out$DAL <- as.integer(diff(x[["RelDate"]][c(tag, rec)]))
      out$LenCat <- x[[tag, "LenCat"]]
      out$DupTag <- TRUE
    } else {
      # complete the list for output
      out$RelYr <- x[["RelYear"]][[1]]
      out$RecYr <- x[["RelYear"]][-1]
      out$RelDate <- x[["RelDate"]][[1]]
      out$RecDate <- x[["RelDate"]][-1]
      out$DAL <- Reduce(
        f = `+`,
        x = diff(x[["RelDate"]]),
        accumulate = TRUE
      )
      out$LenCat <- x[[1, "LenCat"]]
    }
  }

  # function output
  # class(out) <- c(class(out), "StuRecap")
  class(out) <- "StuRecap"
  out
}
# end StuRecap.RecapData

#' @describeIn StuRecap for object class `StuRecap`.
#' @export
StuRecap.StuRecap <- function(
  x, ..., species = 'w', lencat = NULL, minDAL = -1L) {

  # TODO: explain paramters
  # TODO: consider assigning class to this fun's output for summary method
  # TODO: add comments to certain code lines below for explanation

  species <- match.arg(
    arg = species,
    choices = c(White = 'w', Green = 'g'),
    several.ok = FALSE
  )

  spc <- x[["Species"]] %in% names(species)
  dal <- x[["DAL"]] > minDAL & !duplicated(x[["RecDate"]])

  if (!is.null(lencat)) {
    lencat <- match.arg(
      arg = lencat,
      choices = c(sub = 's', leg = 'l', ovr = 'o'),
      several.ok = FALSE
    )

    if (!(x[["LenCat"]] %in% names(lencat))) return(NULL)
  }

  if (!spc) return(NULL)
  if (sum(dal) == 0) return(NULL)

  rly <- x[["RelYr"]]
  rcy <- x[["RecYr"]][which(dal)]
  n <- length(rcy)

  if (n > 1) rly <- rep(rly, length = n)

  # output
  out <- matrix(
    data = c(rly, rcy),
    nrow = n,
    ncol = 2,
    dimnames = list(NULL, c("RelY", "RecY"))
  )

  # class(out) <- "StuRelRecYear"
  out
}
# end StuRecap.StuRecap

#' Conveniently Apply \code{StuRecap} Methods.
#'
#' @description \code{ApplyStuRecap()} is convenient wrapper for
#'    \code{StuRecap} methods. It works with the list outputs of such
#'    methods so user does not have to run for loops or nested
#'    \code{lapply} functions. See \code{\link{StuRecap}}.
#'
#' @param x Object of class `list` with each element of class `RecapData`
#'   (e.g., \code{TagRecaptures[["Data"]]}).
#' @param ... Passed on to other methods. See details.
#'
#' @return List with the following items;
#'    \describe{
#'       \item{GetRecapInfo}{function to get output of \code{StuRecap.RecapData}}
#'       \item{GetMatrixData}{function to get 2-column matrix - column 1
#'             is release year & column 2 is recapture year}
#'       \item{CrossTab}{cross tab table made from matrix data
#'             release x recapture}
#'    }
#'
#' @details \code{StuRecap.StuRecap()} accepts three arguments: `species`;
#'    `lencat`; & `minDAL`. Each are described below.
#'    \describe{
#'       \item{species}{default to 'w' (White Sturgeon)}
#'       \item{lencat}{default to `NULL`; choices: s; l; o. defines the
#'             length category at tagging s = sub-legal, l = legal, and
#'             o = over-legal}
#'       \item{minDAL}{minimum days-at-large, default -1 to include all but
#'             useful for limiting fish caught within a day or week of
#'             being tagged, if so desired}
#'    }
#'
#'    Best to keep \code{species} as default as 'g' (Green Sturgeon;
#'    the other choice have but ~3 recaptures in entire dataset)
#'
#' @export
#'
#' @examples
#' # coming soon.
ApplyStuRecap <- function(x, ...) {

  d <- lapply(x, FUN = StuRecap)

  l <- lapply(d, FUN = StuRecap, ...)
  out <- do.call(what = rbind, args = l)
  list(
    GetRecapInfo = function() get("d"),
    GetMatrixData = function() get("out"),
    CrossTab = xtabs(data = out)
  )
}
# end ApplyStuRecap

# methods for counting recaptured tags by release year --------------------

#' Count Tag Recaptures For Each Release Year.
#'
#' @description Count of tag recaptures for each release year is required to
#'    estimate absolute abundance (i.e., the `R` in MC/R). \code{CountRecap}
#'    methods provide a convenient way to get such counts along with the
#'    cutoff recapture year and count (n) of how many recapture years were
#'    included.
#'
#' @param x Object of either class `numeric` or class `xtabs`.
#' @param ... Passed to other methods. Currently not used.
#' @param n Integer giving the number in years for how long to count recaptures.
#' @param recYears Numeric vector providing 4-digit recapture years.
#' @param nRecaps Numeric vector providing number of recaptures per recapture
#'    year.
#'
#' @return Numeric vector with the following items.
#'    \describe{
#'       \item{NRecYears}{count of recapture years inlcuded in summary}
#'       \item{CutoffYear}{year in which counting recaptures stopped}
#'       \item{NRecaps}{number of recaptured tags for release year}
#'    }
#' @export
#'
#' @examples
#' # coming soon.
CountRecap <- function(x, ...) {
  UseMethod(generic = "CountRecap")
}
# end CountRecap

#' @describeIn CountRecap for class numeric.
#' @export
CountRecap.numeric <- function(x, ..., n, recYears, nRecaps) {

  b <- (x + n) > recYears
  bb <- x > recYears
  b[bb] <- NA[bb]

  i <- as.character(x)
  j <- which(b)

  nrec <- NA
  nrec <- if (is.matrix(nRecaps)) nRecaps[i, j] else nRecaps[j]

  c(
    NRecYears = sum(b, na.rm = TRUE),
    CutoffYear = recYears[max(j)],
    NRecaps = sum(nrec)
  )
}
# end CountRecap.numeric

#' @describeIn CountRecap for class xtabs.
#' @export
CountRecap.xtabs <- function(x, ..., n) {

  dn <- lapply(dimnames(x), FUN = as.numeric)
  yrs <- setNames(object = dn[[1]], nm = dn[[1]])

  out <- vapply(
    yrs,
    FUN = CountRecap,
    FUN.VALUE = numeric(3L),
    n = n,
    recYears = dn[[2]],
    nRecaps = x
  )

  # output
  t(out)
}
# end CountRecap.xtabs
