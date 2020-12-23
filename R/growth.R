# ******************************************************************************
# Created: 23-Ju1-2018
# Author:  J. DuBois
# Contact: jason.dubois@wildlife.ca.gov
# Purpose: This file contains functions or methods to calculate growth given
#          age and length and (or) mean length at age.
# Source:  most functions herein use von Bertalanffy growth model as described
#          in Ricker 1975, eqn. 9.9; we use VBGM to get length given age
# ******************************************************************************

#' @keywords internal
#' @rdname spopmodel-internals
ApplyCLC <- function(mat) {

  # some input validation ********************************************
  if (!is.matrix(mat) && dim(mat)[[2]] != 2)
    stop("`mat` must be a matrix with only 2 columns.", call. = FALSE)
  # ******************************************************************

  # for cycling through each row of mat
  # n <- dim(mat)[[1]]
  dn <- dimnames(mat)[[1]]

  # the function to be returned, where l = fish lengths
  function(l) {

    if (!is.numeric(l)) stop("`l` must be numeric.", call. = FALSE)

    # for convenience of removing NAs
    l <- Filter(f = Negate(is.na), x = l)

    out <- vapply(seq_along(dn), FUN = function(i) {

      # for comparison in if statements below
      na <- is.na(mat[i, ])

      if (all(na)) return(NA)
      if (!na["L"] & na["U"]) return(sum(l >= mat[i, "L"]))
      sum(l >= mat[i, "L"] & l <= mat[i, "U"])

    }, FUN.VALUE = numeric(1L))

    names(out) <- dn
    out
  }
  # end output function
}
# end ApplyCLC

#' Create Function to Apply von Bertalanffy Growth Model (VBGM).
#'
#' @description Function returns a function which is then used to get length at
#'   age or age at length given VBGM parameters. The VBGM parameters are
#'   returned with the second function for use in other applications.
#'
#' @param Linf Numeric. Asymptotic length parameter from VBGM.
#' @param K Numeric. Estimated growth rate parameter from VBGM.
#' @param t0 Numeric. Age at zero size paramater from VBGM.
#'
#' @return Returns a function to supply length (default) or age (`x`) which then
#'   returns age (default) or length.
#'
#' @note Paramater `output`: Character. Paramter from returned function accepts
#'   either `laa` or `aal` (length-at-age or age-at-length; default `laa`).
#' @export
#'
#' @examples
#' # coming soon
GetVBGM <- function(Linf, K, t0) {

  #  t = age or ages
  fn <- function(x, output = c("laa", "aal")) {

    if (!is.numeric(x)) stop("`x` must be numeric.", call. = FALSE)

    output <- match.arg(
      output,
      choices = c("laa", "aal"),
      several.ok = FALSE
    )

    # calculate length at age (laa)
    # laa <- Linf * (1 - exp(-K * (x - t0)))

    switch(
      EXPR = output,
      "laa" = out <- Linf * (1 - exp(-K * (x - t0))),

      # get age at length (idea borrowed from ALKr::age_slicing)
      "aal" = out <- log(1 - (x / Linf)) / -K + t0
    )

    # output laa with parameters
    list(
      Linf = Linf,
      K = K,
      t0 = t0,
      Vals = out
    )
  }
  # end fn

  # return function
  fn
}
# end GetVBGM

#' Create Length Cutoff for White Sturgeon Catch.
#'
#' @description For White Sturgeon, the `C` in MC/R is collected over at least
#'   several years. To ensure consistency with length (size) of the estimate
#'   year, we limit `C` lengths using VBGM. (A better explanation is needed.)
#'
#' @param x Currently object of class `CatchCutoffMatrix`.
#' @param ... Passed on to \code{link{[base]{round}}} `digits` parameter.
#' @param year Numeric. Year in which length data was collected (likely
#'   considered a recapture year).
#'
#' @return List with two items. `CLC` or catch length cutoff matrix &
#'   `CountCatch()`, a function that accepts length data and returns counts
#'   given criteria (lower and upper bounds) set forth in `CLC`.
#' @export
#'
#' @examples
#' # coming soon.
CatchLengthCutoff <- function(x, ...) {
  UseMethod(generic = "CatchLengthCutoff")
}
# end CatchLengthCutoff

#' @describeIn CatchLengthCutoff for object class `CatchCutoffMatrix`.
#' @export
CatchLengthCutoff.CatchCutoffMatrix <- function(x, ..., year) {

  yr_diff <- year - x[, "RelYear"]

  yr_diff[yr_diff <= 0] <- NA

  adj_ages <- yr_diff + x[, c("MinAge", "MaxAge")]

  v <- attr(x, which = "vbParams")

  vb <- GetVBGM(Linf = v["Linf"], K = v["K"], t0 = v["t0"])

  # dig <- c(...)

  out <- vb(adj_ages)[["Vals"]]

  if (!is.null(c(...))) out <- round(out, ...)

  dimnames(out) <- list(x[, "RelYear"], c("L", "U"))

  # list output for inclusion of function to check lengths
  list(
    CLC = out,
    CountCatch = ApplyCLC(mat = out)
  )
}
# end CatchLengthCutoff.CatchCutoffMatrix

#' Create Annual (or Sampling Year) Length Cutoff Matrix.
#'
#' @description Because we cannot easily age sturgeon, length acts as a
#'   surrogate. Here, given min length and max length (usually from annual
#'   extant regulations) and VBGM, we calculate corresponding min age & max age.
#'   These ages then in turn are used to create length cutoff matrices.
#'
#' @param relYear Numeric. Release (tagging) or sampling 4-digit year.
#' @param minLen Numeric. Minimum length, typically per regulations in place
#'   during `relYear`.
#' @param maxLen Numeric. Maximum length, typically per regulations in place
#'   during `relYear`.
#' @param vb Function. Output of \code{GetVBGM()}.
#'
#' @return Matrix with five columns (RelYear, MinLen, MaxLen, MinAge, MaxAge),
#'   where MinAge & MaxAge calculated using length inputs & `vb` function.
#' @export
#'
#' @examples
#' # coming soon.
CatchCutoffMatrix <- function(relYear, minLen, maxLen, vb) {

  check <- vapply(
    c(relYear, minLen, maxLen),
    FUN = length,
    FUN.VALUE = numeric(1L)
  )

  if (!all(diff(check) == 0)) {
    stop("`numeric` inputs must be of equal length.", call. = FALSE)
  }

  # gets age from user-supplied lengths
  aal <- vb(c(minLen, maxLen), output = "aal")

  # for column names in matrix output
  col_names <- c("RelYear", "MinLen", "MaxLen", "MinAge", "MaxAge")

  # output as matrix for convenience & use in other functions
  out <- matrix(
    data = c(relYear, minLen, maxLen, aal[["Vals"]]),
    nrow = length(relYear),
    dimnames = list(NULL, col_names)
  )

  # just for convenience & FIO
  attr(out, which = "vbParams") <- unlist(aal[c("Linf", "K", "t0")])

  class(out) <- "CatchCutoffMatrix"

  # display matrix
  out
}
# end CatchCutoffMatrix

#' Fits von Bertalanffy growth model (VBGM) using \code{stats::nls}.
#'
#' @description \code{FitVBGM} using Ricker 1975, eqn. 9.9 to calculate
#'    length at age.\eqn{l_t = L_{inf}(1 - e^{-K(t-t_0)})}
#'
#' @param data Dataframe with length and age fields.
#' @param len Numeric length field within data.
#' @param age Numeric age field within data.
#' @param start Starting parameters passed to \code{stats::nls}. Must be
#'    a named list with values for "Linf", "K", and "t0".
#' @param ... Further arguments passed to \code{\link{nls}}.
#'
#' @return Model fit object of type \code{\link{nls}}.
#' @export
#'
#' @references Ricker, W.E. (1975). Computation and Interpretation of
#'    Biological Statistics of Fish Populations. Bulletin of the Fisheries
#'    Research Board of Canada, Bulletin 191, Ottawa.
#'
#' @examples
#' # start_vals from Cal Fish and Game, Kohlhorst et al 1980
#' start_vals <- list(Linf = 261.2, K = 0.04027, t0 = 3.638)
#' # catch_data <- subset(trammel_catch, !is.na(Age))
#' # below example giving error...need to troubleshoot
#' \dontrun{
#' FitVBGM(
#'   data = subset(trammel_catch, !is.na(Age)),
#'   len = FL,
#'   age = Age,
#'   start = start_vals
#' )
#' }
FitVBGM <- function(data, len, age, start = list(), ...) {

  y <- as.character(substitute(len))
  x <- as.character(substitute(age))

  # exception handling ***********************************************
  if (!all(c(x, y) %in% colnames(data)))
    stop(x, "|", y, " fields not found in data.", call. = FALSE)

  if (!is.list(start))
    stop("`start` must be a list.", call. = FALSE)

  params <- c("Linf", "K", "t0")

  if (is.null(names(start)) || !all(names(start) %in% params)) {
    msg <- "`start` must be a named list, use names: "
    stop(msg, paste0(params, collapse = "; "), call. = FALSE)
    rm(msg)
  }

  if (!is.numeric(unlist(start, use.names = FALSE)))
    stop("all `start` values must be numeric.", call. = FALSE)
  # ******************************************************************

  rhs <- paste0("Linf * (1 - exp(-K * (", x, " - t0)))")
  frmla <- as.formula(paste(y, rhs, sep = " ~ "))

  fit <- stats::nls(formula = frmla, data = data, start = start, ...)

  fit
}
# end FitVBGM

#' Uses Dahl-Lea method to back-calculate length at age.
#'
#' @description \code{BackCalcLength} employs the Dahl-Lea algorithm to
#'    back-calculate length at age given specific data from the analysis
#'    of hard parts (e.g., scales). Assumes for now first age is 1.
#'
#' @param data Dataframe where each row is a single fish & contains
#'    fields below.
#' @param lCap Field name in data containing (numeric) length at capture.
#' @param rCap Field name in data containing (numeric) scale radius at capture.
#' @param rAtAge Field (column) names or column numbers (easier) in \code{data}
#'    containing scale radius at each age \emph{i}.
#'
#' @return A dataframe with two numeric fields: \code{len} & \code{age}. NA
#'    values have been removed.
#' @export
#'
#' @references Lea, E. 1910. On the methods used in the herring-investigations.
#'             Publ. Circonst. Cons. perm. int. Explor. Mer 108(1):14–22.
#'
#'             Ogle, D.H. 2016. Introductory Fisheries Analyses with R. Chapman
#'             & Hall/CRC, Boca Raton, FL.
#'
#' @examples
#' # code here
BackCalcLength <- function(data, lCap, rCap, rAtAge) {

  l <- as.character(substitute(lCap))
  r <- as.character(substitute(rCap))

  # assumes for now first rAtAge is age 1
  d <- data[rAtAge]
  colnames(d) <- 1:ncol(d)

  dahl_lea <- (d * data[[l]]) / data[[r]]

  out <- reshape2::melt(
    data = dahl_lea,
    # id.vars = ,
    measure.vars = 1:ncol(dahl_lea),
    variable.name = "age",
    na.rm = TRUE,
    value.name = "len"
  )

  # need age as numeric, not factor
  out$age <- as.numeric(as.character(out[["age"]]))
  rownames(out) <- NULL
  out
}
# end BackCalcLength
