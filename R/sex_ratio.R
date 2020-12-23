# ******************************************************************************
# Created: 09-Oct-2019
# Author:  J. DuBois
# Contact: jason.dubois@wildlife.ca.gov
# Purpose: This file contains functions or methods to simplify calculating sex
#          ratio (mostly for striped bass); further we inlclude functionality
#          to employ a "look-up" table for sex (e.g., 1 = male, 2 = female)
# ******************************************************************************

#' Calculate Count and Proportion of Male to Female
#'
#' @description Simplifies calculation of Striped Bass sex ratio. For now, only
#'   available for object class `sex_striper`.
#'
#' @param x Object of class `sex_striper`.
#' @param ... Currently not used.
#' @param digits Integer for rounding proportion.
#'
#' @return List with two elements (frequency & proportion).
#' @export
#'
#' @examples
#' # coming soon.
SexRatio <- function(x, ...) {
  UseMethod(generic = "SexRatio")
}

#' @describeIn SexRatio for object class `sex_striper`.
#' @export
SexRatio.sex_striper <- function(x, ..., digits = NULL) {
  # if (inherits(x, what = "factor")) res <- summary(x)
  f <- table(x, useNA = "ifany", dnn = NULL)
  p <- prop.table(f)
  if (!is.null(digits)) p <- round(p, digits = digits)
  list(Freq = f, Prop = p)
}
# end SexRatio.sex_striper

# SetStriperSex <- function() {
#
#   lkp <- c(`1` = 'M', `2` = 'F')
#
#   function(s, asFactor = FALSE) {
#     out <- lkp[s]
#     out[is.na(out)] <- 'U'
#
#     if (asFactor) {
#       out <- factor(
#         out,
#         levels = c('M', 'F', 'U')#,
#         # labels = c()
#       )
#       class(out) <- "factor"
#     }
#
#     class(out) <- c("StriperSex", class(out))
#     out
#   }
# }
# # end SetStriperSex

#' Apportion Sex (as m or f) for Striped Bass Dataset.
#'
#' @description Striped Bass data analyses are bifurcated by sex. Not all
#'    fish are or can be sexed in field. So, this function apportions sex
#'    based on proportion of sexed fish. No fractions, this function assigns
#'    the 'leftover' 1 to females.
#'
#' @param data Dataframe containing sex data.
#' @param sex Character (quoted or not). The field name in `data`.
#'
#' @return `data` with un-sexed fish now either as 'm' or 'f'.
#' @export
#'
#' @examples
#' # coming soon.
ApportionSex <- function(data, sex) {

  # to quote argument assigned to `sex`
  s <- as.character(substitute(sex))

  # denotes male & female
  sym <- c('m', 'f')

  # to get frequency by sex, including 'u' (unknown)
  st <- table(data[[s]])

  # proportion of male & female for sampling below
  p <- prop.table(st[sym])

  # for later subsetting
  b <- substitute(expr = sex %in% 'u')

  # to sample either 'm' or 'f' based on sex ratio (proportion); may require a
  # set.seed() but will implement at later date or before calling this function
  sx <- sample(sym, size = st[['u']], replace = TRUE, prob = p)

  # replace all unknowns with known (sampled) sex
  data[[s]][eval(b, envir = data)] <- sx

  # to remove unknown level (if any)
  if (is.factor(data[[s]]))
    data[[s]] <- factor(data[[s]], exclude = 'u')

  # return data
  data
}
# end ApportionSex
