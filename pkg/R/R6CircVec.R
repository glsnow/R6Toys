#' Circular Vector
#'
#' @description
#' An R6 class that wraps an ordinary R vector and provides circular
#' (wrap-around) indexing. Accessing an element at a position beyond either
#' end of the vector wraps seamlessly to the other end, making the vector
#' behave as a ring.
#'
#' @details
#' All indexing uses 1-based positions, consistent with R conventions.
#' The modular arithmetic used internally means any integer position —
#' including values less than 1 or greater than \code{length(value)} — is
#' valid and always returns an element.
#'
#' A common use-case is cycling through a fixed set of labels or states.
#' For example, a circular vector of weekday names lets you step forward or
#' backward through the days of the week without boundary checks.
#'
#' @examples
#' # Cycle through weekday names
#' days <- R6CircVec$new(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
#'
#' days$val(1)     # "Mon"
#' days$val(7)     # "Sun"
#'
#' # Wrap past the end
#' days$val(8)     # "Mon"
#' days$val(0)     # "Sun"
#'
#' # Neighbour lookups: what comes after Wednesday (position 3)?
#' days$right(3)   # "Thu"
#'
#' # What comes before Monday (position 1)?
#' days$left(1)    # "Sun"  (wraps to the end)
#'
#' # Step through a week starting from Friday (position 5)
#' pos <- 5
#' for (i in 1:7) {
#'   cat(days$val(pos), "\n")
#'   pos <- pos + 1
#' }
#'
#' @export
R6CircVec <- R6Class("R6CircVec",
  public = list(

    #' @field value The underlying R vector. Any vector type is supported;
    #'   indexing wraps circularly around its length.
    value = NULL,

    #' @description
    #' Create a new \code{R6CircVec} object.
    #' @param x A vector of any type that will be accessed circularly.
    initialize = function(x) {
      self$value <- x
    },

    #' @description
    #' Return the element at position \code{i}, wrapping circularly if
    #' \code{i} is outside \code{1:length(value)}.
    #' @param i Integer. The (1-based) position to look up. Values outside
    #'   the vector bounds wrap around.
    #' @return The element of \code{value} at the wrapped position.
    val = function(i) {
      ii <- (i - 1) %% length(self$value) + 1
      self$value[ii]
    },

    #' @description
    #' Return the element immediately to the right of (i.e. after) position
    #' \code{i}, wrapping from the last element back to the first.
    #' @param i Integer. The reference position (1-based).
    #' @return The element of \code{value} one step clockwise from \code{i}.
    right = function(i) {
      ii <- i %% length(self$value) + 1
      self$value[ii]
    },

    #' @description
    #' Return the element immediately to the left of (i.e. before) position
    #' \code{i}, wrapping from the first element back to the last.
    #' @param i Integer. The reference position (1-based).
    #' @return The element of \code{value} one step counter-clockwise from
    #'   \code{i}.
    left = function(i) {
      ii <- (i - 2) %% length(self$value) + 1
      self$value[ii]
    }
  )
)
