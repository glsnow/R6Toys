#' Point Stack
#'
#' @description
#' An R6 class implementing a stack of two-dimensional (x, y) points. Points
#' are pushed onto and popped from the top of the stack in last-in, first-out
#' (LIFO) order. Internal storage is pre-allocated and grows automatically as
#' needed, so repeated \code{push()} calls do not incur a reallocation on
#' every step.
#'
#' @details
#' Points can be supplied to \code{push()} either as two separate scalar
#' arguments (\code{x} and \code{y}) or as a length-2 vector passed to the
#' first argument alone. Both forms store the same result.
#'
#' The active bindings \code{x} and \code{y} provide read-only access to all
#' currently stored x- and y-coordinates as plain numeric vectors, which is
#' convenient for plotting or bulk computation without modifying the stack.
#'
#' @examples
#' ps <- R6PointStack$new()
#'
#' # Push points as separate x, y arguments
#' ps$push(1, 2)
#' ps$push(3, 4)
#'
#' # Push a point as a length-2 vector
#' ps$push(c(5, 6))
#'
#' ps$length        # 3
#'
#' # Peek at the top without removing it
#' ps$peek()        # c(5, 6)
#'
#' # Access all stored coordinates
#' ps$x             # c(1, 3, 5)
#' ps$y             # c(2, 4, 6)
#'
#' # Plot the points
#' plot(ps$x, ps$y)
#'
#' # Pop the top point
#' ps$pop()         # c(5, 6)
#' ps$length        # 2
#'
#' # Generate a Sierpinski triangle
#' corners <- cbind(c(0, 1, 1/2), c(0, 0, sqrt(3)/2))
#' cur <- c(1/2, 1/2)
#' tri_fractal <- R6PointStack$new()
#' 
#' for(i in 1:1000) {
#'   w <- sample(1:3, 1)
#'   cur[1] <- (cur[1] + corners[w, 1]) / 2
#'   cur[2] <- (cur[2] + corners[w, 2]) / 2
#'   tri_fractal$push(cur)
#' }
#' 
#' plot(tri_fractal$x, tri_fractal$y, pch = ".", asp=1)
#' 
#' for(i in 1:10000) {
#'   w <- sample(1:3, 1)
#'   cur[1] <- (cur[1] + corners[w, 1]) / 2
#'   cur[2] <- (cur[2] + corners[w, 2]) / 2
#'   tri_fractal$push(cur)
#' }
#' 
#' plot(tri_fractal$x, tri_fractal$y, pch = ".", asp=1)
#' plot(tri_fractal$x, tri_fractal$y, pch = ".", asp=1, 
#' xlim = c(0, 0.2), ylim = c(0, 0.2))
#' 
#' @export
R6PointStack <- R6Class("R6PointStack",
  public = list(

    #' @field length Non-negative integer. The number of points currently on
    #'   the stack.
    length = 0,

    #' @description
    #' Push a point onto the top of the stack. Internal storage is doubled
    #' automatically when capacity is exceeded.
    #' @param x Numeric. Either the x-coordinate (when \code{y} is also
    #'   supplied) or a length-2 numeric vector \code{c(x, y)} (when \code{y}
    #'   is omitted).
    #' @param y Numeric. The y-coordinate. Optional when \code{x} is a
    #'   length-2 vector.
    #' @return The \code{R6PointStack} object (invisibly), enabling method
    #'   chaining.
    push = function(x, y) {
      self$length <- self$length + 1
      if (length(private$xx) <= self$length) {
        private$xx <- c(private$xx, rep(NA, self$length))
        private$yy <- c(private$yy, rep(NA, self$length))
      }
      if (missing(y)) {
        private$xx[self$length] <- x[1]
        private$yy[self$length] <- x[2]
      } else {
        private$xx[self$length] <- x
        private$yy[self$length] <- y
      }
      return(invisible(self))
    },

    #' @description
    #' Remove and return the point at the top of the stack.
    #' @return A length-2 numeric vector \code{c(x, y)} for the removed point,
    #'   or \code{NULL} if the stack is empty.
    pop = function() {
      if (self$length < 1) return(NULL)
      ol <- self$length
      self$length <- ol - 1
      return(c(private$xx[ol], private$yy[ol]))
    },

    #' @description
    #' Return the point at the top of the stack without removing it.
    #' @return A length-2 numeric vector \code{c(x, y)} for the top point,
    #'   or \code{NULL} if the stack is empty.
    peek = function() {
      if (self$length < 1) return(NULL)
      return(c(private$xx[self$length], private$yy[self$length]))
    }
  ),
  private = list(
    # Pre-allocated internal storage for x- and y-coordinates.
    # Capacity doubles automatically in push() when length is exceeded.
    xx = numeric(100),
    yy = numeric(100)
  ),
  active = list(

    #' @field x Read-only numeric vector of all stored x-coordinates, in
    #'   push order (bottom to top).
    x = function() private$xx[seq_len(self$length)],

    #' @field y Read-only numeric vector of all stored y-coordinates, in
    #'   push order (bottom to top).
    y = function() private$yy[seq_len(self$length)]
  )
)
