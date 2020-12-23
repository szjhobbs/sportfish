# ******************************************************************************
# Created: 28-Oct-2019
# Author:  J. DuBois
# Contact: jason.dubois@wildlife.ca.gov
# Purpose: This file contains functions or methods to get tag recapture info
#          from Striped Bass or Sturgeon datasets. Also, functions or methods
#          herein provide ability to match recatpure info with tag release
#          info providing easier way to get at R for the MC/R equation.
# Note:    Striped Bass & Sturgeon datasets record recaptures slightly
#          differently. Effort has been made to create one-size-fits-both type
#          functions where possible. If not, description section will note on
#          which dataset to apply function or method in question.
# ******************************************************************************

#' @keywords internal
#' @rdname sportfish-internals
DiscPit <- function(disc, pit) {

  # out <- NA_character_

  pit[pit <= 0] <- NA

  pit <- as.character(pit)

  disc[disc %in% ""] <- NA_character_
  pit[pit %in% ""] <- NA_character_

  if (!is.na(disc)) return(disc)
  if (!is.na(pit)) return(pit)

  # out
  NA_character_
}
# end DiscPit

#' @keywords internal
#' @rdname sportfish-internals
IsRecap <- function(type, shed, retag) {

  out <- FALSE

  if (type %in% "Recap") {
    out <- TRUE
    attr(out, which = "type") <- "recap"
    return(out)
  }

  if (retag %in% "Yes") {
    out <- TRUE
    attr(out, which = "type") <- "retag"
    return(out)
  }

  if (shed %in% "Maybe" && retag %in% "No") {
    out <- TRUE
    attr(out, which = "type") <- "shedMaybeNoRetag"
    return(out)
  }

  if (shed %in% "Yes" && retag %in% "No") {
    out <- TRUE
    attr(out, which = "type") <- "shedYesNoRetag"
    return(out)
  }

  attr(out, which = "type") <- "notRecap"
  out
}
# end IsRecap

#' @keywords internal
#' @rdname sportfish-internals
GetRecapData <- function(x, d, colNames) {
  b <- d[["dn"]] %in% x |
    d[["pn"]] %in% x |
    d[["od"]] %in% x |
    d[["op"]] %in% x

  out <- d[which(b), ]
  colnames(out) <- colNames
  # class(out) <- "RecapData"
  class(out) <- c(class(out), "RecapData")
  out
}
# end GetRecapData

#' @keywords internal
#' @rdname sportfish-internals
GetRecapNum <- function(...) {

  # unlists ... for use in functions & code below
  p <- c(..., recursive = TRUE)

  # boolean to say whether or not tag is a recapture
  b <- IsRecap(
    type = p[["type"]],
    shed = p[["shed"]],
    retag = p[["retag"]]
  )

  attr_b <- attr(b, which = "type")

  out <- NA_character_
  # attr(out, which = "type") <- attr_b

  # if a recapture, then get tag number if true "recap" or else if (others,
  # e.g., recaptured - retagged)
  if (b) {
    if (attr_b %in% "recap")
      out <- DiscPit(disc = p[["dn"]], pit = p[["pn"]])
    else
      out <- DiscPit(disc = p[["od"]], pit = p[["op"]])
  }


  # otherwise if not a recaptured tag return NA
  attr(out, which = "type") <- attr_b
  out
}
# end GetRecapNum

#' Retrieve Dataframe of Sturgeon Tag Recaptures.
#'
#' @description Get description here
#'
#' @param data Dataframe containing CDFW mark-recapture Sturgeon data.
#' @param ... Field names (not quoted) in `data` where each is named (see
#'   details).
#'
#' @return List with four items: (1) `TagNum` - the tag number of the
#'   recaptured tag; (2) `TagType` - describes recapture (i.e., a
#'   recapture; a retag; etc.); (3) `RecordCount` - count of records
#'   by tag type & total count; (4) `Data` - a list with appropriate
#'   release & recapture info for each recaptured tag. List is class
#'   `GetStuRecap`.
#'
#' @details Items in `...` must be named accordingly.
#' \describe{
#'   \item{type}{sturgeon type, i.e. Tagged, Recapture, or Not Tagged;
#'               usually found in `StuType` field in `Sturgeon` data}
#'   \item{shed}{field with info on whether the sturgeon shed or
#'               possibly shed the disc tag}
#'   \item{retag}{field with info on whether the sturgeon was retagged}
#'   \item{dn}{field with original disc tag number}
#'   \item{pn}{field with original PIT number}
#'   \item{od}{field with old disc tag number}
#'   \item{op}{field with old PIT number}
#' }
#'
#' @note Admittedly, `StuType` is a poorly named field & somewhat
#'   misleading (e.g., can be confused with species). This field
#'   essentially describes the table in which data live in the database
#'   (tagged, not tagged, or recaptured). Some tagged fish can be
#'   classified as recaptured if the fish had a previous tag and was (thus)
#'   retagged. \code{GetStuRecap} and other internal functions capture
#'   all recaptured fish (including those retagged or those fish with only
#'   PIT tags, no disc tags).
#'
#' @export
#'
#' @examples
#' # coming soon
GetStuRecap <- function(data, ...) {

  # TODO: not terribly keen on this function. It is slow & somewhat confusing,
  # but it suffices for now. The idea is to really only call this function from
  # the data import file, savings its results in a .rds file for future use.
  # Though it's available to the end user, it's not designed intuitively, so
  # best used "behind the scenes." At some point maybe add check for proper
  # names for `...`. (31-Oct-2019)

  # ... should be named with type, shed, retag, dn, pn, od, op

  # for eventual 'looping' in first lapply() below
  n <- nrow(data)

  # for use in match (below) & to return as attribute to maitain original column
  # names if desired in future methods or functions
  col_nms_data <- colnames(data)

  # to get ... into convenient named vector & so user does not have to quote
  # elements (i.e., field names in data); using vapply() seemed to be the easiet
  # solution (at least on 30-Oct-2019)
  l <- as.list(substitute(expr = c(...)))
  l <- vapply(l[-1], FUN = as.character, FUN.VALUE = character(1L))

  # temporarily change column names with names(l) for convenient use in
  # GetRecapNum() & GetRecapData() where names(...) are hard coded; using
  # match() to maintain proper order in names(...) vs column order in data
  m <- match(col_nms_data, table = l)
  b <- !is.na(m)
  m <- m[b]
  colnames(data)[b] <- names(l)[m]

  # loop through each row to get recapture (tag) number if tag is in fact a
  # recapture (otherwise return NA); now (31-Oct-2019) captures tag type
  # attribute - had to do it this way as it seemed vapply() was "removing" the
  # attr set in GetRecapNum()
  tag <- vapply(seq_len(n), FUN = function(i) {
    out <- GetRecapNum(data[i, names(l)])
    c(out, attr(out, which = "type"))
  }, FUN.VALUE = character(2L), USE.NAMES = FALSE)

  # for some counts by tag type
  counts <- c(Total = n, table(tag[2, ]))
  # don't need NA values for processing below
  tag <- tag[, !is.na(tag[1, ])]

  # GetRecapNum() is performed on each records; as a result tags recaptured more
  # than once will "show up" > 1 time in the `tag` variable; we don't need
  # duplicated tags & as such remove them here before further processing in
  # GetRecapData(); we also return in the list which tags were duplicates & thus
  # removed (04-Nov-2019)
  dups <- duplicated(tag[1, ])
  dups_rm <- tag[, dups]
  tag <- tag[, !dups]

  # create a list for function output (attempted to get TagNum & Data returned
  # as a dataframe, but a list is more appropriate)
  out <- list(
    TagNum = tag[1, ],
    TagType = tag[2, ],
    RecordCount = counts,
    DupTagsRemove = dups_rm,
    Data = lapply(
      setNames(object = tag[1, ], nm = tag[1, ]),
      FUN = GetRecapData,
      d = data, colNames = col_nms_data
    )
  )

  class(out) <- "GetStuRecap"
  # class(out$Data) <- c(class(out), "RecapData")

  out
}
# end GetStuRecap

# possible functions ------------------------------------------------------

# TagCat <- function(type, disc, pit) {
#
#   # not a tag release
#   if (!(type %in% "Tag")) return("nt")
#
#   # cannot use - no external or no internal tag
#   if (disc %in% "" & (is.na(pit) || pit < 0)) return("no")
#
#   # disc only
#   if (!(disc %in% "") & is.na(pit)) return("do")
#
#   # pit only
#   if (disc %in% "" & !is.na(pit)) return("po")
#
#   # disc + pit
#   "dp"
# }
