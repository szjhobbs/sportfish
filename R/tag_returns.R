# ******************************************************************************
# Created: 08-Apr-2020
# Author:  J. DuBois
# Contact: jason.dubois@wildlife.ca.gov
# Purpose: This file contains functions & methods for analyzing angler tag
#          return data. Particularly, it provides a function to identify
#          reward tags reported on Cards as good tag or otherwise.
# ******************************************************************************

#' Check Angler-Reported Reward Disc Number.
#'
#' @description The Sturgeon Report Card provides a field for reporting a
#'   disc tag should the kept or released White Sturgeon possess one. A number
#'   of anglers report partial tag numbers or other info (e.g., $50) making
#'   difficult the process of matching with CDFW tag release data. This function
#'   sorts through these reported "numbers" to decipher what is or might be a
#'   real tag to facilitate matching with tag release data.
#'
#' @param x Character. The reported tag number or field containing such data.
#'   If field, then will need to employ \code{lapply} or \code{vapply} or the
#'   like, as function is not vectorize.
#'
#' @return Name character vector lenght = 2. `Tag` (either the tag number or
#'   blank) and `Desc` or a one- or two-word description of `Tag`.
#' @export
#'
#' @examples
#' # coming soon
CheckCardTag <- function(x) {

  # patterns
  p_tag <- "^[A-Za-z]{2}[[:punct:]|[:space:]]*[0-9]{1,5}"
  p_zip <- "^zip|\\d{5}\\-\\d{4}"
  p_4_5_digits <- "^[[:punct:]|[:space:]]*\\d{4,5}$"
  p_dollar <- "^\\${1}|^\\d{1,3}\\.\\d{2}$"
  p_card_num <- "^D\\-\\d{1,}"
  p_some_digits <- "\\d"

  # for final output if not blank but does not contain digits or any info to
  # find possible match with released disc tag
  out <- c(Tag = "", Desc = "noMatch")

  # for nothing reported
  if (is.na(x) || x %in% "") {
    out[[2]] <- "empty"
    return(out)
  }

  # for 0 reported
  if (x %in% "0") {
    out[[2]] <- "reported0"
    return(out)
  }

  # for beginning with N (as in NA, No, none, etc.); comparabel to nothing
  # reported - we assume angler is making sure we know there was no tag
  if (grepl(pattern = "^N", x = toupper(x))) {
    out[[2]] <- "startedWithN"
    return(out)
  }

  # a good tag
  if (grepl(pattern = p_tag, x = x)) {
    # minor clean up for matchin with tag release data (removing punctuation -
    # e.g., HH-1234 & setting all uppercase e.g., hh1234 --> HH1234)
    x <- gsub(pattern = "[[:punct:]|[:space:]]", replacement = "", x = x)
    x <- toupper(x)
    out[[1]] <- x
    out[[2]] <- "goodTag"
    return(out)
  }

  # when angler reports zip code thinking it's the tag number
  if (grepl(pattern = p_zip, x = x)) {
    out[[1]] <- "zip"
    out[[2]] <- "zipCodeOnly"
    return(out)
  }

  # when angler reports tag number (numeric) but no prefix (e.g., missing 'HH')
  if (grepl(pattern = p_4_5_digits, x = x)) {

    # to remove any spaces or punctuation so we get only numbers (0-9)
    x <- gsub(pattern = "\\D", replacement = "", x = x)

    # 5-digits likely only could be an ST (or $20) tag; we affix the 'ST' prefix
    # for possible matching with tag release data
    if (nchar(x) == 5) {
      out[[1]] <- paste0("ST", x)
      out[[2]] <- "prefixed-ST"
      return(out)
    }

    # all others missing prefix (e.g., 'HH') & matching would require some more
    # sleuthing (e.g., comparing reported angler capture date and [or] length
    # with tag release info of possible tag nums matching on digits)
    out[[1]] <- paste0("xx", x)
    out[[2]] <- "missingPrefix"
    return(out)
  }
  # end numeric only (4-5 digits) check

  # when the angler reports just the reward amount (e.g., $50)
  if (grepl(pattern = p_dollar, x = x)) {
    out[[1]] <- x
    out[[2]] <- "rewardOnly"
    return(out)
  }

  # for when angler likely reports Card number
  if (grepl(pattern = p_card_num, x = x)) {
    out[[2]] <- "possibleCardNum"
    return(out)
  }

  # for when angler reports some digits but not enough or too many to make %100
  # match with tag release data
  if (grepl(pattern = p_some_digits, x = x)) {
    out[[1]] <- x
    out[[2]] <- "someDigits"
    return(out)
  }

  # for everything else (angler reported something but no numbers & cannot match
  # with tag release data; mostly here angler reported something like 'blank'
  # rather than just leaving the field blank or saying 0 or 'none')
  out
}
# end CheckCardTag
