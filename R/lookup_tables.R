# ******************************************************************************
# Created: 11-Dec-2018
# Author:  J. DuBois
# Contact: jason.dubois@wildlife.ca.gov
# Purpose: This file contains look-up tables to facilitate data clean up; we
#          could change data in the database (SQL Server) but it is (likely)
#          safer to do so via look-up tables.
# ******************************************************************************

#' Look-up Table for Sturgeon Mark-Recapture Data.
#'
#' @description A list containing look-up tables for various fields found in
#'   Sturgeon data. These look-up tables assist in data clean up & make
#'   analytics easier to summarize. For example, a fish conditions of "Good" or
#'   "good" in the database will cause issues with a case-senstive language like
#'   R. These look-up tables will rectify this issue.
#'
#' @export
#'
lkp_sturgeon <- list(
  # disposition by angler post catch
  fate = c(
    Kept = "kept",
    `Released Alive` = "relAlive",
    `Released Dead` = "relDead",
    Unknown = "unk"
  ),
  # fishing mode by angler
  fishmode = c(
    `Boat` = "PrivateBoat",
    `CardOnly` = "CardOnly",
    `Commercial Fishery` = "CommercialFishery",
    `Dead` = "FoundDead",
    `Party Boat` = "PartyBoat",
    `pier` = "Pier",
    `Pier` = "Pier",
    `Private Boat` = "PrivateBoat",
    `Shore` = "Shore",
    `Unknown`= "Unknown"
  ),
  # angler fishing day or night
  daynight = c(
    ` ` = 'U',
    `Unknown` = 'U',
    `Day` = 'D',
    `Night` = 'N'
  ),
  # conditon post tagging by CDFW
  conditon = c(
    fair = "fair",
    Fair = "fair",
    `fair/poor` = "fairpoor",
    good = "good",
    Good = "good",
    `NULL` = NA_character_,
    poor = "poor",
    Poor = "poor"
  ),
  # vessel used by CDFW
  vessel = c(
    `New Alosa` = "NewAl",
    Striper = "Strpr",
    `Striper II` = "StrII"
  ),
  # sturgeon species
  species = c(
    `4` = "White",
    `Green Sturgeon` = "Green",
    `White Sturgeon` = "White",
    `GRESTU` = "Green",
    `WHISTU` = "White",
    `27` = "White",
    `28` = "Green"
  ),
  # mostly tagging location
  location = c(
    `Sacramento River` = "SacRiver",
    `Lower San Joaquin Delta` = "LwrSJDelta",
    `Tisdale Bypass` = "TisdaleBP",
    `San Pablo Bay` = "SanPablo",
    `2` = "unk",
    # `1` = "unk",
    `1` = "SanPablo",
    `Suisun Bay` = "Suisun",
    `Fremont Weir` = "FremontW",
    `Lower San Joaquin River` = "LwrSJRiver"
  ),
  # tidal state
  tide = c(
    `Flood` = "flood",
    `Ebb` = "ebb",
    `Slack/flood` = "slack",
    `Slack` = "slack"
  ),
  # trammel net mesh size
  meshsize = c(
    `0` = NA_integer_,
    `2` = NA_integer_,
    `6` = 6L,
    `7` = 7L,
    `8` = 8L
  ),
  # panel config out of 8 panels
  panel = c(
    `10/8` = '10',
    `2/8` = '2',
    `3/8` = '3',
    `4/8` = '4',
    `5/8` = '5',
    `6/8` = '6',
    `7/8` = '7',
    `8/8` = '8'
  ),
  # for fish salvage facilities
  facility = c(
    `1` = "SWP",
    `2` = "CVP"
  ),
  # for tally description Cards 2007-2011
  tallydesc = c(
    `Cards Issued` = "CardsIssued",
    `No Data` = "NoCatch",
    `No Fishing` = "DidNotFish"
  )
)
# end lkp_sturgeon

#' Look-up Table for Striped Bass Mark-Recapture Data.
#'
#' @description A list containing look-up tables for various fields found in
#'   Striped Bass data. These look-up tables assist in data clean up & make
#'   analytics easier to summarize. For example, a fish conditions of "Good" or
#'   "good" in the database will cause issues with a case-senstive language like
#'   R. These look-up tables will rectify this issue.
#'
#' @export
#'
lkp_striper <- list(
  sex = function(s, asFactor = FALSE) {
    lkp <- c(`1` = 'M', `2` = 'F')
    out <- lkp[s]
    out[is.na(out)] <- 'U'

    if (asFactor) {
      out <- factor(
        out,
        levels = c('M', 'F', 'U')#,
        # labels = c()
      )
      class(out) <- "factor"
    }

    class(out) <- c("sex_striper", class(out))
    out
  },
  capmethod = c(`1` = "GN", `2` = "FT"),
  # sex = c(`1` = 'M', `2` = 'F'),
  condition = c(`1` = "good", `2` = "fair"),
  tagvalue = c(
    `02` = "2", `05` = "5", `10` = "10",
    `20` = "20", `50` = "50", `100` = "100",
    `NM` = "NR", `NR` = "NR"
  ),
  tagaction = c(
    # rtnf = recaptured tag no fish
    # sleg = sub-legal sized fish (not tagged)
    `1` = "dtag", `2` = "recp", `3` = "rtnf",
    `4` = "sleg", `5` = "over", `6` = "dead",
    `7` = "crld"
  )
  # TODO: add more for angler tag returns
  # TODO: consider making tagvalue numeric
)
# end lkp_striper
