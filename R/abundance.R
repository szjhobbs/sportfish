# ******************************************************************************
# Created: 19-Sep-2019
# Author:  J. DuBois
# Contact: jason.dubois@wildlife.ca.gov
# Purpose: This file contains functions or methods to calculate Petersen
#          abundance estimates using equations found in Ricker 1975
# ******************************************************************************

# absolute estimates ------------------------------------------------------

#' Calculate Petersen Abundance Estimates.
#'
#' @description \code{PetersenEst} calculates abundance estimates using
#'   mark-recapture data. Function is designed following equations detailed in
#'   Ricker 1975 (see References). Currently, we use Bailey's modified
#'   equation (see eqn 3.9, page 79 in Ricker 1975).
#'
#' @param x Numeric. Number of fish marked (tagged).
#' @param ... Currently not used.
#' @param CC Numeric. Number of fish caught or sampled.
#' @param RR Numeric. Number of recaptured fish (tags) in the sample.
#' @param alpha Numeric scalar used to calculated \code{z-value} for defining
#'   confidence intervals (e.g., 95\%). Default at 0.05 (or 95\%).
#'
#' @return A list (of class \code{PetersenEst}) containing items in
#'   \code{Values} section.
#'
#' @section Values:
#'
#'   \describe{
#'     \item{N}{absolute abundance estimate}.
#'     \item{Var}{variance of N}.
#'     \item{CI}{confidence interval as SE * Z}.
#'     \item{SE}{standard error as \code{sqrt(Var)}}.
#'     \item{Level}{CI level as a result of Z; e.g., 95\%}.
#'     \item{Z}{given as \code{qnorm(1 - alpha / 2)},
#'              where alpha = 0.05 for 95\% CI}.
#'    }
#'
#' @references Ricker, W.E. (1975). Computation and interpretation of biological
#'   statistics of fish populations. Bulletin of the Fisheries Research Board
#'   of Canada, Bulletin 191, Ottawa. http://www.dfo-mpo.gc.ca/Library/1485.pdf
#'
#' @note Method \code{print} written for class PetersenEst returns estimate
#'   with confidence interval.
#'
#' @export
#'
#' @examples
#' # coming soon
PetersenEst <- function(x, ...) {
  UseMethod(generic = "PetersenEst")
}
# end PetersenEst

#' @describeIn PetersenEst for object class `numeric`.
#' @export
PetersenEst.numeric <- function(x, CC, RR, ..., alpha = 0.05) {

  n <- (x * (CC + 1)) / (RR + 1)

  v <- VarPetersenEst(NN = n, CC = CC, RR = RR)

  z <- qnorm(1 - alpha / 2)

  s <- sqrt(v)

  l <- sprintf(fmt = "%.0f%%", (1 - alpha) * 100)

  out <- list(N = n, Var = v, CI = s * z, SE = s, Level = l, Z = z)

  class(out) <- "PetersenEst"

  out
}
# end PetersenEst.numeric

#' @keywords internal
VarPetersenEst <- function(NN, CC, RR) {

  num <- CC - RR
  den <- (CC + 1) * (RR + 2)

  NN^2 * num / den
}
# end VarPetersenEst

#' @describeIn PetersenEst \code{print} method (See notes).
#' @export
print.PetersenEst <- function(x, ...) {
  msg <- sprintf(
    fmt = "Est (%s CI): %.1f %s %.1f",
    x[["Level"]],
    x[["N"]],
    "\U00B1",
    x[["CI"]]
  )
  cat(msg, sep = "\n")
}
# end print.PetersenEst
