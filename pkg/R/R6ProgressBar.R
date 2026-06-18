#' @importFrom R6 R6Class
NULL

#' Progress Bar Object
#'
#' @description
#' An R6 class that wraps R's built-in progress bar functions
#' (\code{\link{txtProgressBar}}, \code{\link[tcltk]{tkProgressBar}},
#' \code{\link{winProgressBar}}) and inherits from \code{\link{R6Counter}}.
#' This lets you update the progress bar by calling \code{$inc()} or using
#' the inherited \code{pp}/\code{mm} active bindings, without needing to
#' maintain a separate counter.
#'
#' @details
#' Create a progress bar with \code{R6ProgressBar$new(max)}.  The
#' \code{type} argument controls which underlying widget is used:
#' \code{"auto"} (the default) tries \code{"win"}, then \code{"tk"},
#' then \code{"txt"} depending on what is available.
#'
#' The \code{label} argument can be either a single
#' \code{\link{sprintf}}-style format string (e.g. \code{"\%d"}, the
#' default) that is filled with the current value, or a character vector
#' of length \code{max} whose elements are displayed one-by-one as the
#' bar advances.
#'
#' When \code{cycle = TRUE}, incrementing past \code{max} wraps the
#' displayed position back to 1 (the counter value itself keeps growing).
#'
#' The progress bar window is automatically closed when the
#' \code{R6ProgressBar} object is garbage-collected.
#'
#' @section Inherited from \code{R6Counter}:
#' All fields and methods of \code{\link{R6Counter}} are available,
#' including \code{$dec()}, \code{$set()}, and the active bindings
#' \code{pp}, \code{mm}, \code{ppp}, and \code{mmm}.  Note that the
#' inherited \code{pp}/\code{mm} active bindings update the internal
#' counter but do \emph{not} redraw the progress bar; use \code{$inc()},
#' \code{$dec()}, or \code{$set()} for that.
#'
#' @examples
#' # Basic usage: increment until finished
#' pb <- R6ProgressBar$new(100)
#' while (!pb$finished) {
#'   pb$inc()
#'   Sys.sleep(0.05)
#' }
#' pb$value   # 100
#'
#' # Reset to 0 and reuse
#' pb$reset
#' pb$value   # 0
#'
#' # Use run() to wrap a function so every call increments the bar
#' pb2 <- R6ProgressBar$new(50)
#' slow_fn <- pb2$run(function(x) x^2)
#' results <- sapply(1:50, slow_fn)
#'
#' # Vector of custom labels
#' steps <- c("Loading", "Cleaning", "Fitting", "Saving")
#' pb3 <- R6ProgressBar$new(max = 4, label = steps)
#' for (i in seq_along(steps)) {
#'   pb3$inc()
#'   Sys.sleep(0.3)
#' }
#'
#' # Clean up -- closes the progress bar window
#' rm(pb, pb2, pb3)
#' gc()
#'
#' @export
R6ProgressBar <- R6Class("R6ProgressBar",
  inherit = R6Counter,
  public = list(

    #' @field value Integer. The current progress bar position.
    value = 0,

    #' @field type Character. The progress bar type in use:
    #'   \code{"txt"}, \code{"tk"}, or \code{"win"}.
    type = "",

    #' @field label Character. The label template or vector supplied at
    #'   construction.
    label = "",

    #' @field max Integer. The maximum value of the progress bar.
    max = 0,

    #' @field pb The underlying progress bar object returned by
    #'   \code{txtProgressBar}, \code{tkProgressBar}, or
    #'   \code{winProgressBar}.
    pb = NULL,

    #' @field cycle Logical. Whether incrementing past \code{max} wraps
    #'   the display position back to 1.
    cycle = FALSE,

    #' @description
    #' Create a new \code{R6ProgressBar} object.
    #'
    #' @param max Integer. The maximum value of the progress bar (required).
    #' @param min Integer. The minimum value. Default \code{0}.
    #' @param initial Integer. The initial position of the bar. Default \code{0}.
    #' @param type Character. Progress bar type. One of \code{"auto"},
    #'   \code{"txt"}, \code{"tk"}, or \code{"win"}. \code{"auto"} (the
    #'   default) selects \code{"win"} if \code{winProgressBar} is available,
    #'   then \code{"tk"} if the \pkg{tcltk} package is available and the Tk
    #'   display is running, otherwise \code{"txt"}.
    #' @param style Integer. Style for text progress bars (1, 2, or 3).
    #'   Only used when \code{type = "txt"}. Default \code{3}.
    #' @param label Character. Either a single \code{\link{sprintf}}-style
    #'   format string (default \code{"\%d"}) evaluated with the current
    #'   counter value, or a character vector of length \code{max} providing
    #'   per-step labels.
    #' @param cycle Logical. If \code{TRUE}, the displayed bar position wraps
    #'   back to 1 after reaching \code{max}. Default \code{FALSE}.
    #' @param ... Additional arguments passed to the underlying progress bar
    #'   constructor (e.g. \code{title} for \code{winProgressBar}).
    #' @return A new \code{R6ProgressBar} object (invisibly).
    initialize = function(max, min = 0,
                          initial = 0,
                          type = c("auto", "txt", "tk", "win"),
                          style = 3, label = "%d",
                          cycle = FALSE,
                          ...) {
      self$value <- initial
      type <- match.arg(type)
      if (type == "auto") {
        if (exists("winProgressBar", mode = "function")) {
          type <- "win"
        } else if (require(tcltk) && .TkUp) {
          type <- "tk"
        } else {
          type <- "txt"
        }
      }
      if (type == "tk") require("tcltk")
      self$type  <- type
      self$label <- label
      self$max   <- max
      self$cycle <- cycle
      if (length(label) > 1) {
        lab <- label[1]
      } else {
        lab <- sprintf(label, initial)
      }
      self$pb <- switch(type,
        txt = txtProgressBar(min = min, max = max, initial = initial,
                             label = lab, style = style, ...),
        tk  = tkProgressBar(min = min, max = max, initial = initial,
                            label = lab, ...),
        win = winProgressBar(min = min, max = max, initial = initial,
                             label = lab, ...)
      )
      invisible(self)
    },

    #' @description
    #' Increment the progress bar position and redraw the bar.
    #' @param x Numeric. Amount to increment. Default \code{1}.
    #' @return The \code{R6ProgressBar} object (invisibly).
    inc = function(x = 1) {
      super$inc(x)
      if (self$cycle) {
        super$set(((self$value - 1) %% self$max) + 1)
      }
      if (length(self$label) > 1) {
        lab <- self$label[self$value]
      } else {
        lab <- sprintf(self$label, self$value)
      }
      switch(self$type,
        txt = setTxtProgressBar(self$pb, self$value, label = lab),
        tk  = setTkProgressBar(self$pb, self$value,  label = lab),
        win = setWinProgressBar(self$pb, self$value, label = lab)
      )
    },

    #' @description
    #' Decrement the progress bar position and redraw the bar.
    #' @param x Numeric. Amount to decrement. Default \code{1}.
    #' @return The \code{R6ProgressBar} object (invisibly).
    dec = function(x = 1) {
      super$dec(x)
      if (length(self$label) > 1) {
        if (self$value) {
          lab <- self$label[self$value]
        } else {
          lab <- self$label[1]
        }
      } else {
        lab <- sprintf(self$label, self$value)
      }
      switch(self$type,
        txt = setTxtProgressBar(self$pb, self$value, label = lab),
        tk  = setTkProgressBar(self$pb, self$value,  label = lab),
        win = setWinProgressBar(self$pb, self$value, label = lab)
      )
    },

    #' @description
    #' Set the progress bar position to an arbitrary value and redraw.
    #' @param x Numeric. The value to set the bar to. Default \code{0}.
    #' @return The \code{R6ProgressBar} object (invisibly).
    set = function(x = 0) {
      super$set(x)
      if (length(self$label) > 1) {
        if (self$value) {
          lab <- self$label[self$value]
        } else {
          lab <- self$label[1]
        }
      } else {
        lab <- sprintf(self$label, self$value)
      }
      switch(self$type,
        txt = setTxtProgressBar(self$pb, self$value, label = lab),
        tk  = setTkProgressBar(self$pb, self$value,  label = lab),
        win = setWinProgressBar(self$pb, self$value, label = lab)
      )
      return(invisible(self))
    },

    #' @description
    #' Wrap a function so that every call automatically increments the
    #' progress bar by 1 before invoking the original function.
    #' @param FUN A function to wrap. Defaults to \code{\link{identity}}.
    #' @return A new function with the same signature as \code{FUN} that
    #'   increments the bar on each call.
    #' @examples
    #' pb <- R6ProgressBar$new(10)
    #' step <- pb$run(function(x) x^2)
    #' result <- sapply(1:10, step)
    run = function(FUN = I) {
      function(...) {
        self$inc()
        FUN(...)
      }
    }
  ),
  private = list(
    finalize = function() {
      close(self$pb)
    }
  ),
  active = list(

    #' @field reset Active binding. Reading resets the bar to 0.
    #'   Assigning a value \code{x} resets it to \code{x} instead.
    reset = function(x) {
      if (missing(x)) {
        x <- 0
      }
      self$set(x)
    },

    #' @field finished Active binding. Returns \code{TRUE} when
    #'   \code{value >= max}, \code{FALSE} otherwise.
    finished = function(...) {
      self$value >= self$max
    }
  )
)
