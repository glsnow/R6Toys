# These are utility functions for converting between bases
# They are not R6 class objects, but are used in the examples
# for R6Counter and may have other use as well
# vectorized for x

#' Convert an Integer to a String in a Given Base
#'
#' Converts (vectorized) integers to their string representation in an
#' arbitrary base, using a supplied set of symbols to represent digits.
#'
#' Use `offset=0` for standard numerical bases such as binary, hex, or
#' octal. Use `offset=1` with `symb=LETTERS` for spreadsheet-style column
#' numbering (A, B, ..., Z, AA, AB, ...).
#'
#' @param x A numeric vector of (non-negative) integers to convert.
#' @param base Integer base to convert to, e.g. 2, 8, 16.
#' @param symb Character vector of symbols used to represent digits, in
#'   increasing order of value. Defaults to `0-9` followed by `LETTERS`,
#'   truncated to length `base`.
#' @param offset Integer offset applied to digit values; use `1` for
#'   1-indexed numbering schemes such as spreadsheet columns.
#'
#' @return A character vector the same length as `x`, giving the base-`base`
#'   representation of each element.
#'
#' @seealso [Base2Int()] for the inverse conversion.
#' @examples
#' Int2Base(255, base = 16)
#' Int2Base(1:30, base = 26, offset = 1, symb = LETTERS)
#'
#' @export
Int2Base <- function(x, base=10L, 
                     symb = c(0:9, LETTERS)[seq_len(base)],
                     offset=0L) {
  if(length(x) > 1) {
    return(sapply(x, Int2Base, base=base, 
                  symb=symb, offset=offset))
  }
  x <- floor(x)
  out <- if(x || offset) "" else symb[1]
  while(x) {
    r <- (x - offset) %% base
    out <- paste0(symb[r+1], out)
    x <- (x - offset) %/% base
  }
  out
}

#' Convert a String in a Given Base to an Integer
#'
#' Converts (vectorized) strings representing numbers in an arbitrary base
#' back into integers, using a supplied set of symbols to represent digits.
#' This is the inverse of [Int2Base()].
#'
#' @param x A character vector of strings to convert, using the symbols in
#'   `symb`. Matching is case-insensitive (`x` is upper-cased before use).
#' @param base Integer base to convert from, e.g. 2, 8, 16.
#' @param symb Character vector of symbols used to represent digits, in
#'   increasing order of value. Defaults to `0-9` followed by `LETTERS`,
#'   truncated to length `base`.
#' @param offset Integer offset applied to digit values; use `1` for
#'   1-indexed numbering schemes such as spreadsheet columns.
#'
#' @return An integer vector the same length as `x`. If a character of `x`
#'   is not found in `symb`, a warning is issued and `NA` is returned for
#'   that element.
#'
#' @seealso [Int2Base()] for the inverse conversion.
#' @examples
#' Base2Int("FF", base = 16)
#' Base2Int(c("A", "Z", "AA"), base = 26, offset = 1, symb = LETTERS)
#'
#' @export
Base2Int <- function(x, base=10L,
                     symb=c(0:9, LETTERS)[seq_len(base)],
                     offset=0L) {
  if(length(x) > 1) {
    return(sapply(x, Base2Int, base=base,
                  symb=symb, offset=offset))
  }
  
  x <- toupper(x)
  offset <- 1-offset
  out <- 0L
  for(i in strsplit(x, '')[[1]]) {
    w <- which(symb == i)
    if(!length(w)) {
      warning("Character <", i, "> not found in symbols.")
      return(NA)
    }
    out <- out*base
    out <- out + w - offset
  }
  out
}


