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
#' The underlying numeric \code{value} can be displayed and assigned
#' in an alternate representation via the \code{val} active binding,
#' which applies the \code{convert}/\code{iconvert} functions stored
#' in the \code{convert} field (see \code{$new()}). All of \code{print()},
#' \code{pp}, \code{mm}, \code{ppp}, and \code{mmm} report values via
#' \code{val}, so their displayed/returned values reflect the current
#' conversion.
#'
#' @section Active bindings:
#' \describe{
#'   \item{\code{pp}}{Increment by 1 (or by \code{x} if assigned)
#'     and return the \emph{new} value, converted via \code{val}.}
#'   \item{\code{mm}}{Decrement by 1 (or by \code{x} if assigned)
#'     and return the \emph{new} value, converted via \code{val}.}
#'   \item{\code{ppp}}{Return the \emph{current} value (converted via
#'     \code{val}), then increment by 1 (or by \code{x} if assigned).}
#'   \item{\code{mmm}}{Return the \emph{current} value (converted via
#'     \code{val}), then decrement by 1 (or by \code{x} if assigned).}
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
#' cnt$inc()$inc(3)$dec(2) # chaining
#' 
#' #' # convert value to different way of printing
#' 
#' ## Roman Numerals
#' rmn <- R6Counter$new(
#'   convert = function(x) as.roman(x),
#'   iconvert = function(x) as.numeric(as.roman(x))
#' )
#' 
#' rmn$val <- 'xix'
#' rmn$val
#' rmn$pp
#' rmn$pp
#' rmn$pp
#' rmn$pp <- 51
#' rmn$val
#' rmn$mm
#' rmn$mm <- 3
#' rmn$val
#' rmn$value
#' 
#' ## Dates
#' 
#' Date_c <- R6Counter$new()
#' Date_c$convert$convert <- as.Date
#' Date_c$convert$iconvert <- function(x) as.numeric(as.Date(x))
#' 
#' Date_c$val <- '2026-12-25'
#' Date_c$value
#' for(i in 1:12) {
#'   print(Date_c$pp)
#' }
#' 
#' ## Start counting in binary
#' 
#' x <- R6Counter$new(
#'   convert = function(x) Int2Base(x, 2),
#'   iconvert = function(x) Base2Int(x, 2)
#' )
#' 
#' x$val
#' for(i in 1:10) {
#'   cat(x$pp, ' ')
#' }
#' 
#' ## switch to Hex
#' 
#' x$convert <- list(
#'   convert = function(x) Int2Base(x, 16),
#'   iconvert = function(x) Base2Int(x, 16)
#' )
#' 
#' x$val
#' for(i in 1:10) {
#'   cat(x$pp, "\n")
#' }
#' 
#' @export
R6Counter <- R6Class("R6Counter",
  public = list(

    #' @field value Numeric. The current counter value.
    value = 0L,

    #' @field convert list. Functions used by active 
    #' binding \code{val} to convert \code{value}. It
    #' should have 2 members named 'convert' and 
    #' 'iconvert' for converting from the numeric value
    #' and back to the numeric value respectively
      convert = list(
        convert = function(x) {
          x
        },
        iconvert = function(x) {
          x
        }
    ),
    
    #' @description
    #' Create a new \code{R6Counter} object.
    #' @param x Numeric. Initial counter value. Default \code{0}.
    #' @param convert Function. If supplied, used as the \code{convert}
    #'   member of the \code{convert} field (converts the numeric
    #'   \code{value} to a display value for \code{$val}). Default
    #'   is the identity function.
    #' @param iconvert Function. If supplied, used as the \code{iconvert}
    #'   member of the \code{convert} field (converts a display value
    #'   assigned via \code{$val<-} back to the numeric \code{value}).
    #'   Default is the identity function.
    #' @param ... Currently unused; reserved for future use.
    initialize = function(x = 0, convert, iconvert, 
                          ...) {
      self$value <- x
      if(!missing(convert)) {
        self$convert$convert <- convert
      }
      if(!missing(iconvert)) {
        self$convert$iconvert <- iconvert
      }
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
      self$val
    },

    #' @description
    #' Increment the counter.
    #' @param x Numeric. Amount to add. Default \code{1}.
    #' @return The \code{R6Counter} object (invisibly), enabling
    #'   method chaining.
    inc = function(x = 1L) {
      self$value <- self$value + x
      return(invisible(self))
    },

    #' @description
    #' Decrement the counter.
    #' @param x Numeric. Amount to subtract. Default \code{1}.
    #' @return The \code{R6Counter} object (invisibly), enabling
    #'   method chaining.
    dec = function(x = 1L) {
      self$value <- self$value - x
      return(invisible(self))
    }
  ),
  active = list(

    #' @field val return the current value, converted with
    #' the \code{convert} function.
    val = function(x) {
      if(missing(x)) {
        return(self$convert$convert(self$value))
      } else {
        self$value <- self$convert$iconvert(x)
        return(invisible(self$value))
      }
    },

    #' @field pp Prefix-increment active binding. Reading increments
    #'   the counter by 1 and returns the new value. Assigning a
    #'   value \code{x} increments by \code{x} instead.
    pp = function(x) {
      if (missing(x)) x <- 1L
      self$inc(x)
      self$val
    },

    #' @field mm Prefix-decrement active binding. Reading decrements
    #'   the counter by 1 and returns the new value. Assigning a
    #'   value \code{x} decrements by \code{x} instead.
    mm = function(x) {
      if (missing(x)) x <- 1L
      self$dec(x)
      self$val
    },

    #' @field ppp Postfix-increment active binding. Returns the
    #'   current value, then increments by 1. Assigning a value
    #'   \code{x} increments by \code{x} instead.
    ppp = function(x) {
      if (missing(x)) x <- 1
      tmp <- self$val
      self$inc(x)
      return(tmp)
    },

    #' @field mmm Postfix-decrement active binding. Returns the
    #'   current value, then decrements by 1. Assigning a value
    #'   \code{x} decrements by \code{x} instead.
    mmm = function(x) {
      if (missing(x)) x <- 1
      tmp <- self$val
      self$dec(x)
      return(tmp)
    }
  )
)
