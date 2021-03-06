% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/age_length.R
\name{MakeALKey}
\alias{MakeALKey}
\title{Make an age-length key with lengths as rows and ages as columns.}
\usage{
MakeALKey(data, len, age, lenBreaks, breakLabs = NULL)
}
\arguments{
\item{data}{A dataframe with length and age fields (columns).}

\item{len}{Field name in data containing numeric lengths.}

\item{age}{Field name in data containing nummeric ages.}

\item{lenBreaks}{How lengths are binned. See \code{\link{cut}}
for more information. (Supplied to \code{breaks} in \code{cut}.)}

\item{breakLabs}{NULL (default), currently not used.}
}
\value{
Age-length key (length[rows] x age[columns]) with proportion
   or 0 in each cell. Return object is of class "table" with
\code{dimnames(<ALK>)} providing \code{Bin} and \code{Age} list items.
}
\description{
\code{MakeALKey} creates an age-length key according
   to length frequency per age. Resulting frequency converted to
   proportion for application to entire length dataset.
}
\note{
In the example below, some length bins contain no aged sample.
   One can adjust the "by" argument in \code{seq} as a possible fix.
   Or one needs to age fish within those length bins.
}
\examples{
# check range(trammel_catch[["FL"]])
len_breaks <- seq(from = 50, to = 220, by = 5)
#MakeALKey(data = trammel_catch, len = FL, age = Age, lenBreaks = len_breaks)
}
