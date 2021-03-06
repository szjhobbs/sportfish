% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tag_count.R
\name{TagSequence}
\alias{TagSequence}
\title{Generate Sequence and Count of Tags (marks).}
\usage{
TagSequence(data, tags)
}
\arguments{
\item{data}{Dataframe holding tag numbers. Assumes (for now) data have been
split according to tag value.}

\item{tags}{Field name in `data` that holds tag numbers.}
}
\value{
A matrix with three columns (From, To, N), where N is total count of
  tags between From and To.
}
\description{
Marks (or tags) used by the Sportfish Unit are sequential.
  Oftentimes, though, a tag is lost (for example), and the sequence is
  broken. \code{TagSequence} makes easy the task of piecing together the
  sequence (in spite of missing tags) and allows us to easily report tags
  released for a particular season.
}
\examples{
# coming soon.
}
