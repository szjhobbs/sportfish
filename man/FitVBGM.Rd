% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/growth.R
\name{FitVBGM}
\alias{FitVBGM}
\title{Fits von Bertalanffy growth model (VBGM) using \code{stats::nls}.}
\usage{
FitVBGM(data, len, age, start = list(), ...)
}
\arguments{
\item{data}{Dataframe with length and age fields.}

\item{len}{Numeric length field within data.}

\item{age}{Numeric age field within data.}

\item{start}{Starting parameters passed to \code{stats::nls}. Must be
a named list with values for "Linf", "K", and "t0".}

\item{...}{Further arguments passed to \code{\link{nls}}.}
}
\value{
Model fit object of type \code{\link{nls}}.
}
\description{
\code{FitVBGM} using Ricker 1975, eqn. 9.9 to calculate
   length at age.\eqn{l_t = L_{inf}(1 - e^{-K(t-t_0)})}
}
\examples{
# start_vals from Cal Fish and Game, Kohlhorst et al 1980
start_vals <- list(Linf = 261.2, K = 0.04027, t0 = 3.638)
# catch_data <- subset(trammel_catch, !is.na(Age))
# below example giving error...need to troubleshoot
\dontrun{
FitVBGM(
  data = subset(trammel_catch, !is.na(Age)),
  len = FL,
  age = Age,
  start = start_vals
)
}
}
\references{
Ricker, W.E. (1975). Computation and Interpretation of
   Biological Statistics of Fish Populations. Bulletin of the Fisheries
   Research Board of Canada, Bulletin 191, Ottawa.
}
