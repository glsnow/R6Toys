#' Counter Object
#'
#' @description
#' An R6 class implementing a simple integer counter with
#' increment, decrement, and pre/post-increment active bindings
#' modelled on the C-style \code{++} and \code{--} operators.
#'
#' @details
#' Create a counter with \code{R6Counter$new()}, optionally
#' supplying an initial value.  The counter can be modified with
#' \code{$inc()}, \code{$dec()}, and \code{$set()}, or via the
#' active bindings \code{pp}, \code{mm}, \code{ppp}, and
#' \code{mmm}.
#'
#' The active bindings follow C-style prefix/postfix semantics:
#' \itemize{
#'   \item \code{pp} and \code{mm} modify the counter \emph{then}
#'     return the new value (prefix-style).
#'   \item \code{ppp} and \code{mmm} capture the current value,
#'     modify the counter, \emph{then} return the captured value
#'     (postfix-style).
#' }
#'
#' @section Active bindings:
#' \describe{
#'   \item{\code{pp}}{Increment by 1 (or by \code{x} if assigned)
#'     and return the \emph{new} value.}
#'   \item{\code{mm}}{Decrement by 1 (or by \code{x} if assigned)
#'     and return the \emph{new} value.}
#'   \item{\code{ppp}}{Return the \emph{current} value, then
#'     increment by 1 (or by \code{x} if assigned).}
#'   \item{\code{mmm}}{Return the \emph{current} value, then
#'     decrement by 1 (or by \code{x} if assigned).}
#' }
#'
#' @examples
#' cnt <- R6Counter$new(10)
#' cnt$value        # 10
#'
#' cnt$inc()
#' cnt$value        # 11
#'
#' cnt$dec(3)
#' cnt$value        # 8
#'
#' cnt$pp           # increments; returns 9
#' cnt$mm           # decrements; returns 8
#'
#' cnt$ppp          # returns 8, then increments to 9
#' cnt$mmm          # returns 9, then decrements to 8
#'
#' cnt$set(0)
#' cnt$value        # 0
#'
#' @export
R6Counter <- R6Class("R6Counter",
  public = list(

    #' @field value Numeric. The current counter value.
    value = 0,

    #' @description
    #' Create a new \code{R6Counter} object.
    #' @param x Numeric. Initial counter value. Default \code{0}.
    #' @param ... Currently unused; reserved for future use.
    initialize = function(x = 0, ...) {
      self$value <- x
      return(invisible(self))
    },

    #' @description
    #' Set the counter to a specific value.
    #' @param x Numeric. Value to assign to the counter.
    #'   Default \code{0}.
    #' @return The \code{R6Counter} object (invisibly), enabling
    #'   method chaining.
    set = function(x = 0) {
      self$value <- x
      return(invisible(self))
    },

    #' @description
    #' Print the current counter value to the console.
    print = function() {
      print(self$value)
    },

    #' @description
    #' Increment the counter.
    #' @param x Numeric. Amount to add. Default \code{1}.
    #' @return The \code{R6Counter} object (invisibly), enabling
    #'   method chaining.
    inc = function(x = 1) {
      self$value <- self$value + x
      return(invisible(self))
    },

    #' @description
    #' Decrement the counter.
    #' @param x Numeric. Amount to subtract. Default \code{1}.
    #' @return The \code{R6Counter} object (invisibly), enabling
    #'   method chaining.
    dec = function(x = 1) {
      self$value <- self$value - x
      return(invisible(self))
    }
  ),
  active = list(

    #' @field pp Prefix-increment active binding. Reading increments
    #'   the counter by 1 and returns the new value. Assigning a
    #'   value \code{x} increments by \code{x} instead.
    pp = function(x) {
      if (missing(x)) x <- 1
      self$inc(x)
      return(self$value)
    },

    #' @field mm Prefix-decrement active binding. Reading decrements
    #'   the counter by 1 and returns the new value. Assigning a
    #'   value \code{x} decrements by \code{x} instead.
    mm = function(x) {
      if (missing(x)) x <- 1
      self$dec(x)
      return(self$value)
    },

    #' @field ppp Postfix-increment active binding. Returns the
    #'   current value, then increments by 1. Assigning a value
    #'   \code{x} increments by \code{x} instead.
    ppp = function(x) {
      if (missing(x)) x <- 1
      tmp <- self$value
      self$inc(x)
      return(tmp)
    },

    #' @field mmm Postfix-decrement active binding. Returns the
    #'   current value, then decrements by 1. Assigning a value
    #'   \code{x} decrements by \code{x} instead.
    mmm = function(x) {
      if (missing(x)) x <- 1
      tmp <- self$value
      self$dec(x)
      return(tmp)
    }
  )
)
