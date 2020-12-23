# ******************************************************************************
# Created: 20-Apr-2020
# Author:  J. DuBois
# Contact: jason.dubois@wildlife.ca.gov
# Purpose: This file contains functions to assist with creating
#          & numbering captions in .Rmd files
# ******************************************************************************

#' Auto-number Table or Figure Captions.
#'
#' @description Perform auto-numbering within text and for caption of figures
#'    tables, and (or) appendices. Function creates an internal counter for
#'    eliminate use of global variable counter.
#'
#' @param label Character. No restrictions yet but likely should be either
#'    Figure or Table.
#' @param sep Character. The separator between `label` and number.
#'
#' @return List. `Num` returns current caption number. `Show` displays the
#'    caption as (example) `Figure 1: This is a caption.`
#' @export
#'
#' @examples
#' # coming soon
Caption <- function(label, sep = ":") {

  # caption counter
  i <- 1L

  # to display cation with number and separator
  fn <- function(cap, period = NULL) {
    if (!is.null(period)) cap <- paste(period, cap)
    out <- sprintf(fmt = "%s %s%s %s", label, i, sep, cap)
    i <<- i + 1L
    out
  }

  list(
    Num = function() i,
    Show = fn
  )
}
# end Caption

#' Read Caption Text File.
#'
#' @description Makes easier the task of writing captions (and then reading)
#'    captions from a separate file, rather than within `.Rmd`. The text file
#'    can contain all captions (e.g., figures; tables), so long as each caption
#'    is prefixed with '<caption_name><caption_number>: ' (e.g., "Table 1: ").
#'
#' @param file Character. Name of file containing captions
#'    (along with relevant file path).
#'
#'
#' @return Named character string, where names are caption
#'    heading (e.g., 'table1').
#' @export
#'
#' @examples
#' # comin soon.
ReadCaptionFile <- function(file) {
  r <- readLines(con = file)
  # check for blank lines
  b <- grepl(pattern = "^\\s*$", x = r)
  # remove blank lines
  r <- r[!b]
  # split on the colon-space & return each caption named accordingly
  o <- Map(function(x) {
    nm <- gsub(pattern = "\\s", replacement = "", x = x[1])
    nm <- tolower(nm)
    setNames(object = x[2], nm = nm)
  }, strsplit(r, split = "\\|\\|\\s{1}"))

  # for named character string rather than list
  c(o, recursive = TRUE)
}
# end ReadCaptionFile
