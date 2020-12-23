# ******************************************************************************
# Created: 17-Jan-2019
# Author:  J. DuBois
# Contact: jason.dubois@wildlife.ca.gov
# Purpose: This file contains functions & methods for counting tags released
#          or tags returned typically by tag reward value.
# ******************************************************************************

#' Get Tag Count.
#'
#' @description \code{TagCount} in reality is not specific to tags, rather this
#'   function will yield record count by some variable `v` for which `.subset`
#'   yields true. Ideally we use \code{TagCount} to count tags (released or
#'   returned) by tag value (i.e, non-reward and [or] dollar values).
#'
#' @param data Dataframe with tag (return or release) data.
#' @param .by Single field name (non-quoted) in `data` on which to aggregate
#'   record (tag) count.
#' @param .subset Logical. Booleans on which to count. Any grouping of Booleans
#'   evaluating to `TRUE` count as 1.
#' @param total Logical. Option to include `total` count. Default `FALSE`.
#'
#' @return Dataframe with fields `Val` (as in tag value) & `N` (count).
#' @export
#'
#' @examples
#' # coming soon
TagCount <- function(data, .by, .subset, total = FALSE) {

  e <- substitute(.subset)
  v <- as.character(substitute(.by))
  b <- eval(expr = e, envir = data, enclos = parent.frame())

  if (!is.logical(b))
    stop("`.subset` must be logical (T or F).", call. = FALSE)

  if (length(v) != 1)
    stop("`.by` must contain only one variable.", call. = FALSE)

  # handle potential NA values
  b[is.na(b)] <- FALSE

  # if all FALSE return NA (nothing to aggregate)
  if (all(!b)) return(NA) # or NULL need to decide

  # using aggregate() defaults: simplify = TRUE, drop = TRUE
  r <- aggregate(b[b], by = data[b, v, drop = FALSE], FUN = sum)
  colnames(r) <- c("Val", "N")

  # option to add total count
  if (total)
    r <- rbind(r, c(Val = "Total", N = sum(b)))

  # output dataframe
  r
}
# end TagCount

#' Generate Sequence and Count of Tags (marks).
#'
#' @description Marks (or tags) used by the Sportfish Unit are sequential.
#'   Oftentimes, though, a tag is lost (for example), and the sequence is
#'   broken. \code{TagSequence} makes easy the task of piecing together the
#'   sequence (in spite of missing tags) and allows us to easily report tags
#'   released for a particular season.
#'
#' @param data Dataframe holding tag numbers. Assumes (for now) data have been
#'   split according to tag value.
#' @param tags Field name in `data` that holds tag numbers.
#'
#' @return A matrix with three columns (From, To, N), where N is total count of
#'   tags between From and To.
#' @export
#'
#' @examples
#' # coming soon.
TagSequence <- function(data, tags) {

  # to get field name in data that holds tag numbers
  tags <- as.character(substitute(expr = tags))

  # to get numeric for sorting
  tn <- gsub(
    pattern = "[[:alpha:]]",
    replacement = "",
    x = data[[tags]]
  )

  # to eventually get first & last 6-digit tag number after sorting
  tn <- setNames(
    object = as.numeric(tn),
    nm = data[[tags]]
  )

  # to get sequence
  tn <- sort(tn)

  # to generate difference between sorted (ascending) numeric "tag" numbers;
  # because results of diff() are 1 less than length(tn) we include a value of 1
  # as the first value in the vector of differences
  d <- c(1, diff(tn))

  # to output number of tags in each group
  n <- length(tn)

  # for when the sequence is not broken
  if (all(d == 1)) {
    nm <- names(tn[c(1, n)])
    out <- t(list(From = nm[1], To = nm[2], N = n))
    return(out)
  }

  # section below for when tag sequence has been broken (due to lost tag, etc.)

  # i denotes beginning of new sequence
  # i <- which(d != 1)
  i <- unname(which(d != 1))

  # fm = from & assumes i does not contain value 1 as d[1] == 1
  fm <- c(1, i)
  to <- c(i - 1, n)

  # cycle through from & to
  out <- Map(f = function(x, y) {
    list(From = names(tn[x]), To = names(tn[y]), N = length(x:y))
  }, fm, to)

  # for preferred output
  do.call(what = rbind, args = out)
}
# end TagSequence
