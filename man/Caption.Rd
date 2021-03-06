% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/captions.R
\name{Caption}
\alias{Caption}
\title{Auto-number Table or Figure Captions.}
\usage{
Caption(label, sep = ":")
}
\arguments{
\item{label}{Character. No restrictions yet but likely should be either
Figure or Table.}

\item{sep}{Character. The separator between `label` and number.}
}
\value{
List. `Num` returns current caption number. `Show` displays the
   caption as (example) `Figure 1: This is a caption.`
}
\description{
Perform auto-numbering within text and for caption of figures
   tables, and (or) appendices. Function creates an internal counter for
   eliminate use of global variable counter.
}
\examples{
# coming soon
}
