# ******************************************************************************
# Created: 06-Dec-2018
# Author:  J. DuBois
# Contact: jason.dubois@wildlife.ca.gov
# Purpose: This file contains functions to assist with a variety of
#          procedures & data clean up
# ******************************************************************************

#' Read All `.rds` Files from a Single Directory.
#'
#' @description \code{ReadRDSFiles} is basically a wrapper for
#'   \code{base::readRDS} and makes convenient the loading at onces of multiple
#'   files.
#'
#' @param fileDir Character. Name of directory (with path) containing `.rds`
#'   files.
#' @param envir Character. Environment in which to save loaded .`rds` files.
#'   Default is global environment.
#'
#' @return Names of `.rds` files loaded as dataframes to `envir`.
#' @export
#'
#' @examples
#' # none at this time.
ReadRDSFiles <- function(fileDir, envir = .GlobalEnv) {

  # pattern for file searching
  p <- ".rds$"

  rds <- list.files(path = fileDir, pattern = p)

  out <- vapply(rds, FUN = function(.x) {

    nm <- sub(pattern = p, replacement = "", x = .x)

    # read data in
    # out <- readRDS(file = x)

    # load in global env
    assign(nm, value = readRDS(file = file.path(fileDir, .x)), envir = envir)

    if (!exists(nm, envir = envir)) return(FALSE)

    TRUE

  }, FUN.VALUE = logical(1L), USE.NAMES = FALSE)

  if (!all(out)) warning("Some `.rds` files not loaded.", call. = FALSE)

  spc <- paste0(rep('*', times = nchar(fileDir) + 1), collapse = "")

  cat("RDS Files loaded from:", fileDir, spc, rds[out], sep = "\n ", spc)
}
# end ReadRDSFiles

#' Create List Column for Analytics.
#'
#' @description \code{Split} creates a list-column dataframe for ease of
#'   analytics. The `Data` column contains the results of
#'   \code{\link[base]{split}}.
#'
#' @param data A dataframe to be subsetted (optional) & split accordingly.
#' @param subset A Boolean expression evaluated in `data`.
#' @param vars Desired field names (not quoted) in `data`.
#' @param splitVars Field names (or names) in `data` on which to split.
#' @param drop Logical. Default FALSE (should non-ocurring levels be dropped).
#'
#' @return A data.frame with `Data` as a list column along with fields in
#'   `splitVars`. Row count based on `splitVars`.
#' @export
#'
#' @examples
#' # coming soon
Split <- function(data, subset = NULL, vars = NULL, splitVars, drop = FALSE) {
  # TODO: add option to use `drop =` in split()
  # TODO: add functionality to carry classes from splitVars to `out`

  # subsetting *******************************************************
  # based on R's subset.data.frame()
  r <- rep_len(TRUE, length.out = nrow(data))
  v <- TRUE

  nl <- as.list(seq_along(data))
  names(nl) <- names(data)

  # for use in if statements
  esub <- substitute(expr = subset)
  evar <- substitute(expr = vars)

  if (!is.null(esub)) {
    r <- eval(expr = esub, envir = data, enclos = parent.frame())
    if (!is.logical(r))
      stop("`subset` must be logical.", call. = FALSE)
    r & !is.na(r)
  }

  if (!is.null(evar)) {
    v <- eval(expr = evar, envir = nl, enclos = parent.frame())
  }

  sv <- substitute(expr = splitVars)
  sv <- eval(expr = sv, envir = nl, enclos = parent.frame())

  data <- data[r, , drop = FALSE]
  # ******************************************************************

  # split subsetted data according to splitVars
  s <- split(data[v], f = data[sv], drop = drop)

  # variable to hold dataframe output
  out <- NULL

  # for field or fields in dataframe representing splitVars
  if (length(sv) == 1) {
    out <- data.frame(names(s), stringsAsFactors = FALSE)
  } else {
    nms <- strsplit(names(s), split = "\\.")
    nms <- do.call(what = rbind, args = nms)
    out <- data.frame(nms, stringsAsFactors = FALSE)
  }

  # for consistent field naming
  colnames(out) <- colnames(data[sv])

  # split data as a list column in `out`
  out$Data <- s
  out
}
# end Split

#' @keywords internal
CreateLenBins <- function(
  len, lenBreaks, breakLabs = NULL, numericBin = FALSE, ...) {

  if (!is.numeric(len))
    stop("`len` must be numeric.", call. = FALSE)

  b <- cut(
    len,
    breaks = lenBreaks,
    labels = breakLabs,
    include.lowest = FALSE,
    right = FALSE,
    ...
  )

  if (is.null(breakLabs) && numericBin) {
    # assures replacement of (for example) "[210,215)" with coercible numeric
    # (i.e., "210") using min value only (value left of comma)

    # removes L/R brackets & L/R parentheses from e.g., [100,105)
    b <- gsub(pattern = "\\[|\\]|\\(|\\)", replacement = "", x = b)

    b <- strsplit(b, split = ",")

    # get min value from each bin (i.e., left value in example [100,105))
    b <- vapply(b, FUN = '[', 1, FUN.VALUE = character(1L))

    # convert to numeric for output
    b <- as.numeric(b)
  }

  if (any(is.na(b[!is.na(len)])))
    warning("Some bins are NA. Suggest different breaks.", call. = FALSE)

  attr(b, which = "metadata") <- "bins closed right"
  b
}
# end CreateLenBins

#' @keywords internal
CheckBin <- function(x) {
  # x really could be any vector but using this funtion to check length bins
  # after employing split(); so likely use CheckBin within vapply()

  # check if empty (i.e., length = 0)
  if (length(x) == 0) return("empty")

  # check if NA
  if (all(is.na(x))) return("all NA")

  # otherwise return NA
  NA_character_
}
# end CheckBin

LookUp <- function(x, lkp, asFactor = FALSE) {

  if (any(is.na(x))) {
    warning("Some values NA in variable `x`.", call. = FALSE)
  }

  out <- lkp[x]

  # hold off on factor conversion for now
  # if (asFactor) {
  #   return(factor(out, levels = c(WST = "WST", GST = "GST")))
  # }

  out
}
# end LookUp
