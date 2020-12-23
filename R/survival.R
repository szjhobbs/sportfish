# ******************************************************************************
# Created: 20-Ju1-2018
# Author:  J. DuBois
# Contact: jason.dubois@wildlife.ca.gov
# Purpose: This file contains functions or methods to calculate survival rate
#          and (or) mortality rate given species age compisition
# ******************************************************************************

#' Calculates survival rate given age frequency.
#'
#' \code{ChapmanRobson} adapted from Ricker 1975, eqn. 2.5 (2.6 for variance).
#'    Method is similar to using catch curve with age frequency.
#'
#' @param ages Numeric vector of ages.
#' @param n Numeric vector (length of ages) containing count (n) at each age.
#' @param N0 Single value denoting starting age.
#'
#' @return Survival rate estimate \eqn{\hat{S}} with variance
#'    Var(\eqn{\hat{S}}). Also returns sum{ages used * count} and
#'    sum{frequency of all ages used}
#'
#' @export
#'
#' @references Ricker, W.E. (1975). Computation and Interpretation of
#'    Biological Statistics of Fish Populations. Bulletin of the Fisheries
#'    Research Board of Canada, Bulletin 191, Ottawa.
#'
#' @examples
#' # ChapmanRobson(ages = , n = , N0 = 3)
ChapmanRobson <- function(ages, n, N0 = 2) {

  if (!is.numeric(c(ages, n)))
    stop("`ages` & `n` must be numeric.", call. = FALSE)

  coded_age <- rep(NA, times = length(ages))
  coded_age[ages >= N0] <- seq_along(ages[ages >= N0]) - 1

  tt <- sum(coded_age * n, na.rm = TRUE)
  nn <- sum(n[ages >= N0])

  surv_est <- tt / (nn + (tt - 1))
  surv_var <- surv_est * (surv_est - ((tt - 1) / (nn + tt - 2)))

  # A = 1 - S
  # Z = -log(S)
  # F = uZ/A
  # M = Z-F

  list(
    TT = tt,
    N = nn,
    EstS = surv_est,
    VarS = surv_var
  )
}
# end ChapmanRobson

FishingParams <- function(S, mu) {

  Z <- abs(log(S))
  A <- 1 - S

  # µZ/A

  F. <- (mu * Z) / A

  M <- Z - F.

  cm <- 1 - exp(-M)

  list(Z = Z, A = A, F. = F., M = M, cm = cm)
}
