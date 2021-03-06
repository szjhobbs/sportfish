% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/age_length.R
\name{ALKPrecision}
\alias{ALKPrecision}
\title{Assess Precison of Age-Length Key Using Different Sampling Methods.}
\usage{
ALKPrecision(data, age, len, ...)
}
\arguments{
\item{data}{Dataframe containing age & length variables.}

\item{age}{Field name in `data` containing age variable.}

\item{len}{Field name in `data` containing length variable.}

\item{...}{Passed to internal \code{Size()} function. Supply `n` for fixed or
proportional sampling type. `n` is total sample size of ages (default is
1500). Supply `type` as "fixed", "prop" (proportional), or "random." See
Notes section.}
}
\value{
A list of class \code{ALKPrecision}
}
\description{
Coming soon.
}
\note{
Coming soon.
}
\section{Values}{


\describe{
  \item{VarTot}{}.
  \item{MeanVT}{}.
  \item{VarVT}{}.
  \item{Reps}{}.
  \item{SizeArgs}{list}.
}
}

\examples{
# coming soon.
}
\references{
Coming soon.
}
