# ******************************************************************************
# Created: 02-Aug-2018
# Author:  J. DuBois
# Contact: jason.dubois@wildlife.ca.gov
# Purpose: This file contains functions or methods to create age-length key or
#          to get age frequency distribution.
# Source:
# ******************************************************************************

#' Make an age-length key with lengths as rows and ages as columns.
#'
#' @description \code{MakeALKey} creates an age-length key according
#'    to length frequency per age. Resulting frequency converted to
#'    proportion for application to entire length dataset.
#'
#' @param data A dataframe with length and age fields (columns).
#' @param len Field name in data containing numeric lengths.
#' @param age Field name in data containing nummeric ages.
#' @param lenBreaks How lengths are binned. See \code{\link{cut}}
#'    for more information. (Supplied to \code{breaks} in \code{cut}.)
#' @param breakLabs NULL (default), currently not used.
#'
#' @return Age-length key (length[rows] x age[columns]) with proportion
#'    or 0 in each cell. Return object is of class "table" with
#' \code{dimnames(<ALK>)} providing \code{Bin} and \code{Age} list items.
#' @export
#'
#' @note In the example below, some length bins contain no aged sample.
#'    One can adjust the "by" argument in \code{seq} as a possible fix.
#'    Or one needs to age fish within those length bins.
#'
#' @examples
#' # check range(trammel_catch[["FL"]])
#' len_breaks <- seq(from = 50, to = 220, by = 5)
#' #MakeALKey(data = trammel_catch, len = FL, age = Age, lenBreaks = len_breaks)
MakeALKey <- function(data, len, age, lenBreaks, breakLabs = NULL) {

  # get length and age columns
  l <- as.character(substitute(len))
  a <- as.character(substitute(age))

  b <- CreateLenBins(len = data[[l]], lenBreaks = lenBreaks, numericBin = TRUE)

  alk <- prop.table(table(b, data[[a]], dnn = c("Bin", "Age")), margin = 1)

  alk
}
# end MakeALKey

#' Assign age to each fish based on extant age-length data.
#'
#' @description \code{AgeEach} inspired by \code{FSA::alkIndivAge}. Here
#' \code{AgeEach} output is more tailored for use within this package.
#' \code{AgeEach} handles age assignment at the length bin level, so
#'    instance of an age-length key is not required.
#'
#' @param data A dataframe with length and age fields (columns).
#' @param len Field name in data containing numeric lengths.
#' @param age Field name in data containing nummeric ages.
#' @param lenBreaks lenBreaks How lengths are binned. See \code{\link{cut}}
#'    for more information. (Supplied to \code{breaks} in \code{cut}.)
#' @param sex (default NULL) Currently not used.
#'
#' @note Print method for convenience of output.
#'
#' @return A list with age & bin diagnostics along with
#'    a numeric vector of ages, with length = \code{nrow(data)}.
#' @export
#'
#' @examples
#' # check range(trammel_catch[["FL"]])
#' len_breaks <- seq(from = 50, to = 220, by = 5)
#' #AgeEach(data = trammel_catch, len = FL, age = Age, lenBreaks = len_breaks)
AgeEach <- function(data, len, age, lenBreaks, sex = NULL) {

  # get length and age columns
  l <- as.character(substitute(len))
  a <- as.character(substitute(age))

  # create length bins
  b <- CreateLenBins(len = data[[l]], lenBreaks = lenBreaks)

  # ordering of `a` in output was a problem because fish with no length could
  # not be binned above; so this if statement detects NAs & assigns a level of
  # `no_bin` so that any associated ages show up in `age_split`; `b` remains a
  # factor for use in split (30-Apr-2020)
  if (anyNA(b)) {
    levels(b) <- c(levels(b), "no_bin")
    b[is.na(b)] <- "no_bin"
  }

  # to check ages in each length bin
  age_split <- split(data[[a]], f = b, drop = FALSE)

  bin_check <- vapply(age_split, FUN = CheckBin, FUN.VALUE = character(1L))

  # only report values NOT NA
  bin_check <- bin_check[!is.na(bin_check)]

  a <- unsplit(value = lapply(age_split, FUN = CheckAge), f = b, drop = FALSE)

  out <- list(
    BinDiagnostics = list(
      BinCheck = bin_check,
      AllBins = levels(b),
      BinCount = nlevels(b),
      CountNA = sum(is.na(a)),
      CountTotal = length(a)
    ),
    Ages = a
  )

  class(out) <- "AgeEach"

  out
}
# end AgeEach

#' @describeIn AgeEach \code{print} method (See notes).
#' @param x Object of class 'AgeEach'.
#' @param ... Currently not used.
#' @export
print.AgeEach <- function(x, ...) {

  # get diagnostics
  bd <- x[["BinDiagnostics"]]

  out <- sprintf(
    fmt = "Number aged = %s, with %s NA.", # \n Bin Check:\n %s",
    bd[["CountTotal"]] - bd[["CountNA"]],
    bd[["CountNA"]] #,
    # bd[["BinCheck"]]
  )

  # print(out)
  cat(out, "\n", "\n", "Bins below empty or all NA:\n", sep = "")
  print(noquote(bd[["BinCheck"]]))
}
# end print.AgeEach

#' @keywords internal
#'
CheckAge <- function(age) {

  # TODO: test this function
  # TODO: finalize what to do with length(age) == 0
  # TODO: how to treat bins with no ages ??
  # TODO: nail down whole & sum_rem (still issues with double or integer values)
  # TODO: nail down replace = T or replace = F
  # TODO: age some input verification
  # TODO: set ouput to integer ??
  # TODO: add set seed component here or in AgeEach

  # age vector result of splitting age field on length bins so length(age) = 0
  # means no lengths (& thus ages) are in that specific bin return numeric(0)

  # various scenarios of incoming age vector -----------------------------------
  # age ==> (1) numeric(0) OR (2) length = 1 OR (3) length > 1
  #
  #         (4) all numbers (i.e., no NA values) OR ...
  #         (5) all NA OR ...
  #         (6) mixture of numbers & NA values
  #
  # if (1) return numeric(0) [length bin is empty]
  # if (2) if !NA see (4) else see (5)
  # if (3) see (4) OR (5) OR (6)
  #
  # if (4) use assigned age irrespective of length(age)
  # if (5) cannot do much; advise re-binning or if possible ageing
  #        some fish in bin [x, y)
  # if (6) length(age) must be > 1 so go through process using
  #        known ages (& proportions) to "age the NAs"
  #
  # processing scenario (6) **********************
  #
  # freq <- frequency (as proportion) of age variable
  # get column names of `freq` (these are the ages as character vector)
  #
  # (a) if length(freq) = 1 then set all NA
  #     values to column names of `freq` as there is only one age
  #     value for this length bin
  #
  # (b) if number NA values = 1 then
  #     a <- sample(column names of `freq`, size = 1, prop = `freq`)
  #     age[which is na] <- as.numeric(a)
  #     return age
  #
  # (c) set `freq` * sum(all NA values of age)
  #     perform integer division ...
  #     whole <- `freq` * sum(all NA values of age) %/% 1
  #
  #     get remainder using modulus
  #     remainder <- `freq` * sum(all NA values of age) %% 1
  #
  #     if all of whole = 0 then
  #       a <- sample(column names of `freq`,
  #                   size = sum(remainder),
  #                   prop = `freq`)
  #       age[which is na] <- as.numeric(a)
  #
  #       the reality with this scenario is that sum(remainder) should
  #       really be 1 if all of whole = 0 (I believe)
  #
  #     else
  #       create a character vector of ages using names of `whole` &
  #       values of `whole`
  #       new_ages <- rep(dimnames(whole)[[1]], times = whole)
  #
  #       then sample from these new ages
  #       new_ages <- sample(new_ages, size = length(new_ages), replace = T)
  #
  #       recalculate number remaining
  #       remainder <- remainder - length(new_ages)
  #
  #       sample again & combine with `new_ages` vector
  #       new_ages <- c(new_ages, sample(new_ages, size = sum_rem,
  #                                      replace = TRUE))
  #       age[which is NA] <- as.numeric(new_ages)
  #       return age
  #
  # code here ------------------------------------------------------------------

  # age is essentially empty
  if (length(age) == 0) return(numeric(length = 0L))

  # check for NAs within age vector
  is_na <- is.na(age)

  # check point: irrespective of length(age), we note if all values are aged OR
  # all values are NA; essentially not much we need to or can do here except
  # return age ---  we may issue a warning about return NA values, but we can
  # handle this in AgeEach()
  if (all(is_na) || all(!is_na)) return(age)

  # check point: we know at this point length(age) must be > 1 AND age is a
  # mixture of NA & numeric values

  # get proportion per age ignoring (for now) NA values
  freq <- prop.table(table(age, dnn = NULL))

  # get names of `freq` --- essentially age values as character
  char_ages <- dimnames(freq)[[1]]

  # if there is only one unique age in `age` then we have no other choice but to
  # assign all NA values to this age (i.e., no need for random sampling)
  if (length(char_ages) == 1) {
    age[is_na] <- as.numeric(char_ages)
    return(age)
  }

  # check point: we know at this stage we have > 1 unique age, now we need to
  # check the number of NA values; here we check if `age` contains 1 NA value
  num_not_aged <- sum(is_na)

  if (num_not_aged == 1) {
    a <- sample(char_ages, size = 1, prob = freq)
    age[is_na] <- as.numeric(a)
    return(age)
  }

  # check point: at this point we have > 1 unique age & > 1 NA value

  # essentially like using an al-key multiplying the count by proportions
  age_assign <- freq * num_not_aged

  # integer division to get complete age assignments
  whole <- age_assign %/% 1
  # modulo to get remainder (to be handled later)
  sum_rem <- sum(age_assign %% 1)
  # sum_rem <- ceiling(sum_rem)

  # the logic is that if all whole = 0 then sum_rem should equal the number not
  # aged thus we can just use num_not_aged as our size
  if (all(whole == 0)) {
    a <- sample(char_ages, size = num_not_aged, replace = TRUE, prob = freq)
    age[is_na] <- as.numeric(a)
    # print("all whole = 0")
    return(age)
  }

  # a needs "re"-sampling here before proceeding in case sum_rem != 0
  a <- rep(char_ages, times = whole)
  a <- sample(a, size = length(a), replace = TRUE)

  # in this case length(a) should equal number not aged
  if (sum_rem == 0) {
    # a <- sample(a, size = length(a), replace = TRUE)
    age[is_na] <- as.numeric(a)
    # print("sum rem = 0")
    return(age)
  }

  # NEED TO TEST LOGIC BELOW
  # check point:  here all(whole == 0) is FALSE AND sum_rem > 0

  # not sure if recalculation is necessary
  # recalculate number remaining
  # sum_rem <- length(a) - sum_rem     # WRONG

  # if (sum_rem > length(a)) {
  #   sum_rem <- sum_rem - length(a)   # WRONG
  # }

  # sum_rem <- max(sum_rem, length(a)) # WRONG

  # this *should be* the proper recalculation for sum_rem
  sum_rem <- num_not_aged - length(a)  # CORRECT

  # print calls for debugging (uncomment as needed)

  # print(a)
  # print(sum_rem)

  # combine 'a' with samples on the remainder
  a <- c(a, sample(char_ages, size = sum_rem, replace = TRUE, prob = freq))

  age[is_na] <- as.numeric(a)

  # print(a)
  # print(age_assign %% 1)
  # print("made it to the end")

  age
}
# end CheckAge

# age-length key precision check ------------------------------------------

# added 16-Apr-2019

#' @keywords internal
#'
Size <- function(Nj, n = 1500, type = c("fixed", "prop", "random")) {

  # if N is too low may cause floor() to be 0, so fn written to correct this
  fn <- function(x) if (x < 1) 1 else floor(x)

  s <- NULL

  type <- match.arg(
    arg = type,
    choices = c("fixed", "prop", "random"),
    several.ok = FALSE
  )

  fv <- numeric(1L)
  L <- dim(Nj)

  switch (
    EXPR = type,
    fixed = s <- rep(fn(n / L), times = L),
    prop = s <- vapply(n * prop.table(Nj), FUN = fn, FUN.VALUE = fv),
    random = s <- vapply(Nj, FUN = sample.int, FUN.VALUE = fv, size = 1)
  )

  # s
  list(
    s = s,
    n = if (type == "random") NA else n,
    type = type
  )
}
# end Size

#' @keywords internal
#'
SampleAges <- function(ages, size, reps = 100) {

  # boolean to check for NA values
  b <- is.na(ages)

  # maybe if needed, but for now not needed
  # if (all(b)) return(NA)

  # remove NA values for further processing
  if (sum(b) != 0) ages <- ages[!b]

  # sample everything if age count <= sample size
  if (length(ages) <= size) size <- length(ages)

  # otherwise get random sample of ages; replace FALSE (default)
  res <- sample(ages, size = size)

  # get random sample of ages; replace FALSE (default) with replication
  # primarily to increase VarTot values in ALKPrecision()
  res <- replicate(n = reps, expr = {
    sample(ages, size = size)
  }, simplify = FALSE)

  # post sample() as sample() removes attributes
  attr(res, which = "na_rm") <- sum(b)

  res
}
# end SampleAges

#' @keywords internal
#'
VarPi <- function(lj, qij, nj, pi, N) {

  n1 <- lj^2 * (qij * (1 - qij))
  n2 <- lj * (qij - pi)^2

  res <- (n1 / nj) + (n2 / N)

  colSums(res, na.rm = TRUE)
}
# end VarPi

#' Assess Precison of Age-Length Key Using Different Sampling Methods.
#'
#' @description Coming soon.
#'
#' @param data Dataframe containing age & length variables.
#' @param age Field name in `data` containing age variable.
#' @param len Field name in `data` containing length variable.
#' @param ... Passed to internal \code{Size()} function. Supply `n` for fixed or
#'   proportional sampling type. `n` is total sample size of ages (default is
#'   1500). Supply `type` as "fixed", "prop" (proportional), or "random." See
#'   Notes section.
#'
#' @note Coming soon.
#'
#' @references Coming soon.
#'
#' @return A list of class \code{ALKPrecision}
#'
#' @section Values:
#'
#' \describe{
#'   \item{VarTot}{}.
#'   \item{MeanVT}{}.
#'   \item{VarVT}{}.
#'   \item{Reps}{}.
#'   \item{SizeArgs}{list}.
#' }
#'
#' @export
#'
#' @examples
#' # coming soon.
ALKPrecision <- function(data, age, len, ...) {

  # make i = age & j = length stratum

  a <- as.character(substitute(age))
  l <- as.character(substitute(len))

  # type <- match.arg(
  #   arg = sampling,
  #   choices = c("fixed", "prop", "random"),
  #   several.ok = FALSE
  # )

  Nj <- table(data[[l]], dnn = NULL)

  N <- sum(Nj)

  lj <- prop.table(Nj)

  # sampling size
  size <- Size(Nj = Nj, ...)

  # sample ages for each length group
  res <- Map(f = SampleAges, split(data[[a]], f = data[[l]]), size[["s"]])

  # ********* some variables for `vals` below **************
  # for correct number of iterations in lapply() below
  reps <- formals(SampleAges)[["reps"]]

  # for correct function output in second vapply()
  A <- dim(table(data[[a]]))

  # for multiplication in pi & VarPi below
  Lj <- as.vector(lj)
  # ********************************************************

  # some notes *****************************************************************
  # used formals() to get number of samples(reps) -- worked well
  # can't verify na count so for now will skip `nas` -- it's not imperative
  # vals for now can return most everything then output of ALKPrecision() can
  # be selective -- may want to add pi to `out`
  # ****************************************************************************

  vals <- lapply(seq_len(reps), FUN = function(i) {

    # get each level of reps in `res`
    r <- lapply(res, FUN = `[[`, i)

    # nas <- sum(
    #   vapply(res[i], FUN = attr, FUN.VALUE = numeric(1L), which = "na_rm")
    # )

    nj <- vapply(r, FUN = length, FUN.VALUE = numeric(1L))
    nij <- t(vapply(r, FUN = table, FUN.VALUE = numeric(A)))
    qij <- nij / nj
    pi <- colSums(qij * Lj, na.rm = TRUE)
    varpi <- VarPi(lj = Lj, qij = qij, nj = nj, pi = pi, N = N)

    # vals output
    list(
      nj = nj,
      nij = nij,
      qij = qij,
      pi = pi,
      varpi = varpi,
      vartot = sum(varpi)
    )
  })
  # end vals

  # get vartot from `vals` list
  vt <- vapply(vals, FUN = function(x) x[["vartot"]], FUN.VALUE = numeric(1L))

  out <- list(
    VarTot = vt,
    MeanVT = mean(vt),
    VarVT = var(vt),
    Reps = reps,
    # NAsRemoved = nas
    SizeArgs = size[c("n", "type")]
  )

  class(out) <- "ALKPrecision"

  out
}
# end ALKPrecision


#' Assign Age from Sampled Age-Length Key.
#'
#' @description Using length data & known age-length key, assign age
#'    based on length data. Can use an age-length key covering range
#'    of ages and lengths to provide ages to all lengths.
#'
#' @param len Character vector of lengths.
#' @param size Integer vector of length frequencies.
#' @param alk Table or List of age-length frequencies.
#' @param setSeed Numeric. Default NULL for `sample`.
#'
#' @return Numeric vector of ages.
#' @export
#'
#' @examples
#' # coming soon.
AgeAssign <- function(len, size, alk, setSeed = NULL) {
  if (!is.character(len)) len <- as.character(len)
  if (is.null(alk[[len]])) return(rep(NA, times = size))
  p <- prop.table(table(alk[[len]]))
  s <- as.numeric(names(p))
  if (length(s) == 1) return(rep(s, times = size))
  set.seed(seed = setSeed)
  sample(s, size = size, replace = TRUE, prob = p)
}
# end AgeAssign
