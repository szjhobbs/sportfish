% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/effort.R
\name{EffortSb}
\alias{EffortSb}
\title{Calculates Fishing Effort for Striped Bass Fyke Traps & Gill Nets.}
\usage{
EffortSb(data, sTime, eTime, netNum)
}
\arguments{
\item{data}{A dataframe containing at least start time, end time, and net
number.}

\item{sTime}{A datetime variable within \code{data}. Start time fishing net
or trap.}

\item{eTime}{A datetime variable within \code{data}. End time fishing net or
trap.}

\item{netNum}{Numeric or charater value denoting number of net or trap.}
}
\value{
A list of class \code{EffortSb} with the following values:
\itemize{
  \item Hours (fishing effort in hours; total)
  \item HoursAvg (average fishing hours)
  \item HoursVar (variance fishing hours)
  \item Days (count of days fished)
  \item NetSets (count of nets or traps fished)
  \item NetCount (<add explanation>)
  \item NetCountDup (counts of traps
        [mostly] fished more than once per)
}
}
\description{
Using start time and end time values, \code{EffortSb} calculates
  in hours the time fished for either fyke traps or gill nets. The output is
  then paired with catch data to calculate CPUE (or catch per unit effort).
}
\examples{
# coming soon
}
