# ******************************************************************************
# Created: 15-Feb-2019
# Author:  J. DuBois
# Contact: jason.dubois@wildlife.ca.gov
# Purpose: This file contains functions to customize panels in R's
#          graphics::pairs()
# ******************************************************************************

SetPval <- function(x) {

  if (x < 0.001) return("< 0.001")
  if (x < 0.01) return("< 0.01")
  if (x < 0.05) return("< 0.05")

  paste("=", round(x, digits = 3))
}
# end SetPVal

LinReg <- function(x, y) {

  mod <- lm(y ~ x)
  mod_sum <- summary(mod)

  slp <- mod$coefficients[[2]]
  int <- mod$coefficients[[1]]

  mse <- mean(mod$residuals^2)

  ar2 <- mod_sum[["adj.r.squared"]]
  pvl <- mod_sum$coef[2, 4]

  list(
    Mod = mod,
    Slp = slp,
    Int = int,
    MSE = mse,
    AR2 = ar2,
    Pvl = pvl
  )
}
# end LinReg

Corr <- function(x, y) {
  mod <- cor.test(x = x, y = y)

  list(
    r = mod[["estimate"]],
    p = mod[["p.value"]]
  )
}
# end Corr

#' Add Correlation and Smoother to `graphics::pairs()` Plot.
#'
#' @description Allows one to easiliy display smoother curve & correlation (with
#'   p-value) on `graphics::pairs()` plot.
#'
#' @param out Character. Either "points" (default) or "text allowing for either
#'   points with smoother or text (as *r* & p-value). Pass this function to
#'   either `lower.panel` or `upper.panel` in `graphics::pairs()`.
#'
#' @return Function suited for use as described above.
#' @export
#'
#' @examples
#' # coming soon
PanelCorr <- function(out = c("points", "text")) {

  out <- match.arg(arg = out)

  switch (EXPR = out,
    "points" = function(x, y) {
      # points(x = x, y = y, col = "grey40", cex = 1.5)
      panel.smooth(
        x = x,
        y = y,
        col = "grey40",
        cex = 1.5,
        col.smooth = "darkorange",
        span = 3/4,
        iter = 5
      )
    },
    "text" = function(x, y) {
      r <- Corr(x = x, y = y)
      p <- SetPval(r[["p"]])

      lbls <- sprintf("r = %.4f \np-val %s", r[["r"]], p)
      ptx <- median(x, na.rm = TRUE) #* 1.5
      # pty <- max(y, na.rm = TRUE) #* 0.5
      pty <- median(y, na.rm = TRUE)
      # pty <- diff(range(y, na.rm = TRUE)) / 2

      # text(x = ptx, y = pty, labels = lbls, adj = c(0, -0.5), cex = 1.5)
      text(x = ptx, y = pty, labels = lbls, cex = 1.5, adj = c(0, 0))
    }
  )
}
# end PanelCorr

#' Add Linear Regression (Line & Ouput) to `graphics::pairs()` Plot.
#'
#' @description Allows one to easiliy display linear regression & some output
#'   (e.g., slope; intercept; p-value) on `graphics::pairs()` plot.
#'
#' @param out Character. Either "points" (default) or "text allowing for either
#'   points with linear regression line or text (e.g., slope; intercept). Pass
#'   this function to either `lower.panel` or `upper.panel` in
#'   `graphics::pairs()`.
#'
#' @return Function suited for use as described above.
#' @export
#'
#' @examples
#' # coming soon
PanelLM <- function(out = c("points", "text")) {

  out <- match.arg(arg = out)

  switch (EXPR = out,
    "points" = function(x, y) {
      l <- LinReg(x = x, y = y)
      abline(l[["Mod"]], col = "darkorange", cex = 1.25)
      points(x = x, y = y, col = "grey40", cex = 1.5)
    },
    "text" = function(x, y) {
      l <- LinReg(x = x, y = y)
      text(x = min(x), y = max(y), labels = l[["MSE"]])
    }
  )
}
# end PanelLM
