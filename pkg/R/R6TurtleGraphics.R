#' Turtle Graphics
#'
#' @description
#' An R6 class implementing a turtle graphics system. A turtle has a position,
#' a heading, and a pen that can be raised or lowered. When the pen is down,
#' movement traces a line on the graphics device. Multiple turtles can share
#' the same plot by creating subsequent objects with \code{add = TRUE}.
#'
#' @details
#' The turtle starts at position \code{(0, 0)}, facing up (0 degrees), with
#' the pen up. Call \code{PenDown()} to begin drawing, then use \code{move()}
#' and \code{turn()} to navigate.
#'
#' Angles are in degrees. Positive angles turn clockwise (the default
#' orientation matches compass-style headings). The turtle's turn and step
#' size from the most recent call are remembered in \code{last.turn} and
#' \code{last.move}, and can be repeated via the shorthand active bindings
#' \code{t}, \code{r}, \code{l}, \code{m}, \code{f}, and \code{b}.
#'
#' All lines drawn by a turtle are stored internally in an \code{R6DLL}
#' (\code{lines.dll}), so the entire picture can be redrawn at any time by
#' calling \code{plot()}. Each contiguous pen-down segment can have its own
#' graphical parameters set with \code{NewPen()}.
#'
#' @section Pen control:
#' \describe{
#'   \item{\code{PenDown()}}{Lower the pen. Subsequent movement draws a line.}
#'   \item{\code{PenUp()}}{Raise the pen. Subsequent movement does not draw.}
#'   \item{\code{NewPen(...)}}{Change graphical parameters (e.g. \code{col},
#'     \code{lwd}) for the next pen-down segment. If the pen is currently down
#'     it is lifted and re-lowered so the change takes effect cleanly.}
#' }
#'
#' @section Movement:
#' \describe{
#'   \item{\code{move(dist)}}{Move \code{dist} units in the current direction,
#'     drawing if the pen is down.}
#'   \item{\code{forward(dist)}}{Alias for \code{move(dist)}.}
#'   \item{\code{back(dist)}}{Move \code{dist} units in the opposite direction.}
#'   \item{\code{MoveTo(x, y)}}{Teleport to \code{(x, y)} without drawing,
#'     even if the pen is down (the current path is ended and restarted).}
#'   \item{\code{DrawTo(x, y)}}{Draw a line directly to \code{(x, y)},
#'     lowering the pen temporarily if needed.}
#' }
#'
#' @section Turning:
#' \describe{
#'   \item{\code{turn(angle)}}{Turn clockwise by \code{angle} degrees.}
#'   \item{\code{right(angle)}}{Alias for \code{turn(angle)}.}
#'   \item{\code{left(angle)}}{Turn counter-clockwise by \code{angle} degrees.}
#'   \item{\code{TurnTo(angle)}}{Set the heading to an absolute \code{angle}.}
#' }
#'
#' @section Plotting:
#' \describe{
#'   \item{\code{plot()}}{Redraw the entire picture from scratch (opens a new
#'     plot window unless \code{add = TRUE}).}
#'   \item{\code{Add()}}{Redraw all lines and the turtle onto the
#'     \emph{current} graphics device without creating a new plot.}
#'   \item{\code{drawTurtle()}}{Draw the turtle icon at its current position
#'     and heading (called internally by \code{plot()} and \code{Add()}).}
#'   \item{\code{print()}}{No-op; suppresses automatic R6 output.}
#' }
#'
#' @section Active binding shorthands:
#' \describe{
#'   \item{\code{t}}{Read: repeat last turn. Assign \code{t <- angle}:
#'     turn by \code{angle} degrees.}
#'   \item{\code{r}}{Read: turn clockwise by \code{abs(last.turn)}.
#'     Assign \code{r <- angle}: turn clockwise by \code{angle}.}
#'   \item{\code{l}}{Read: turn counter-clockwise by \code{abs(last.turn)}.
#'     Assign \code{l <- angle}: turn counter-clockwise by \code{angle}.}
#'   \item{\code{m}}{Read: repeat last move. Assign \code{m <- dist}:
#'     move \code{dist} units.}
#'   \item{\code{f}}{Read: move forward by \code{abs(last.move)}.
#'     Assign \code{f <- dist}: move forward \code{dist} units.}
#'   \item{\code{b}}{Read: move backward by \code{abs(last.move)}.
#'     Assign \code{b <- dist}: move backward \code{dist} units.}
#'   \item{\code{up}}{Raise the pen (equivalent to \code{PenUp()}).}
#'   \item{\code{down}}{Lower the pen (equivalent to \code{PenDown()}).}
#'   \item{\code{show}}{Show the turtle icon.}
#'   \item{\code{hide}}{Hide the turtle icon.}
#' }
#'
#' @examples
#' ## Example 1: Draw a 7-sided polygon (heptagon)
#' t1 <- R6TurtleGraphics$new()
#' t1$PenDown()
#'
#' nsides <- 7
#' len <- 200 / nsides
#' for (i in 1:nsides) {
#'   t1$move(len)
#'   t1$turn(360 / nsides)
#'   if (interactive()) Sys.sleep(0.3)
#' }
#'
#' ## Example 2: Rotating rainbow squares
#' t2 <- R6TurtleGraphics$new()
#' t2$PenDown()
#'
#' nsides <- 4
#' len    <- 200 / nsides
#' npoly  <- 13
#' cols   <- rainbow(npoly)
#' for (i in 1:npoly) {
#'   t2$NewPen(col = cols[i])
#'   for (j in 1:nsides) {
#'     t2$move(len)
#'     t2$turn(360 / nsides)
#'     if (interactive()) Sys.sleep(0.1)
#'   }
#'   t2$turn(360 / npoly)
#' }
#'
#' ## Example 3: Dance of the turtles — many turtles share one plot
#' npoly  <- 13
#' nsides <- 300
#' len    <- 200 / nsides
#' cols   <- rainbow(npoly)
#'
#' turtles <- lapply(seq_len(npoly), function(i) {
#'   tmp <- R6TurtleGraphics$new(add = (i != 1), autoplot = FALSE)
#'   tmp$turn(360 / npoly * i)
#'   tmp$NewPen(col = cols[i])
#'   tmp$PenDown()
#'   tmp$turtle.size <- 4
#'   tmp$pen.cex     <- 1
#'   tmp
#' })
#'
#' turtles[[1]]$plot()
#'
#' for (j in 1:nsides) {
#'   for (i in seq_len(npoly)) {
#'     turtles[[i]]$move(len)
#'     turtles[[i]]$turn(360 / nsides)
#'     turtles[[i]]$plot()
#'   }
#'   if (interactive()) Sys.sleep(0.02)
#' }
#'
#' @note
#' When \code{autoplot} is \code{TRUE}, a new plot will be created each time a
#'  new move is made.  For interactive use this will probably not be a problem,
#' but if you do commands in a loop, then this could become burdomsome.  You may
#' want to use a graphical device with recording turned off, or set 
#' \code{autoplot = FALSE}.
#' 
#' @export
R6TurtleGraphics <- R6Class("R6TurtleGraphics",
  public = list(

    #' @field show.turtle Logical. Whether the turtle icon is drawn after each
    #'   operation. Default \code{TRUE}.
    show.turtle = TRUE,

    #' @field turtle.size Numeric. Radius of the turtle icon in plot units.
    #'   Default \code{10}.
    turtle.size = 10,

    #' @field add Logical. If \code{TRUE}, \code{plot()} draws onto the
    #'   existing graphics device instead of opening a new plot.
    #'   Default \code{FALSE}.
    add = FALSE,

    #' @field autoplot Logical. If \code{TRUE}, the plot is automatically
    #'   refreshed after every move, turn, or pen operation.
    #'   Default \code{TRUE}.
    autoplot = TRUE,

    #' @field paper.size Numeric. Half-width and half-height of the plot
    #'   window; the plot spans \code{[-paper.size, paper.size]} on both axes.
    #'   Default \code{100}.
    paper.size = 100,

    #' @field cur.pos Numeric vector of length 2. The turtle's current
    #'   \code{(x, y)} position. Default \code{c(0, 0)}.
    cur.pos = c(0, 0),

    #' @field cur.dir Numeric. The turtle's current heading in degrees.
    #'   Default \code{0} (facing up).
    cur.dir = 0,

    #' @field cur.param Named list. Graphical parameters (e.g. \code{col},
    #'   \code{lwd}) applied to lines drawn by the current pen-down segment.
    #'   Default \code{list(col = "blue")}.
    cur.param = list(col = "blue"),

    #' @field pen.pch Integer. Point character used to mark the pen's
    #'   position when the pen is down. Default \code{10}.
    pen.pch = 10,

    #' @field pen.cex Numeric. Character expansion for the pen position marker.
    #'   Default \code{2}.
    pen.cex = 2,

    #' @field last.turn Numeric. The angle (degrees) used in the most recent
    #'   \code{turn()} call. Used by the \code{t}, \code{r}, and \code{l}
    #'   active bindings to repeat the last turn. Default \code{15}.
    last.turn = 15,

    #' @field last.move Numeric. The distance used in the most recent
    #'   \code{move()} call. Used by the \code{m}, \code{f}, and \code{b}
    #'   active bindings to repeat the last move. Default \code{25}.
    last.move = 25,

    #' @field pen.down Logical. \code{TRUE} when the pen is lowered and
    #'   movement draws lines; \code{FALSE} when the pen is raised.
    #'   Default \code{FALSE}.
    pen.down = FALSE,

    #' @field angle.shift Numeric. Offset (degrees) added to \code{cur.dir}
    #'   before converting to radians for movement calculations. Default
    #'   \code{-90} (so that heading 0 points up).  This can be changed if
    #' you want 0 to point a different direction.
    angle.shift = -90,

    #' @field angle.scale Numeric. Conversion factor from degrees to radians,
    #'   with sign controlling rotation direction. Default \code{-pi/180}
    #'   (clockwise-positive degrees).  This can be changed to use radians
    #' or other angle measurement instead of degrees, or to turn counter-clockwise.
    angle.scale = -pi / 180,

    #' @field turtle.col Character. Colour used to draw the turtle icon.
    #'   Default \code{"forestgreen"}.
    turtle.col = "forestgreen",

    #' @field lines.dll An \code{R6DLL} object storing all pen-down line
    #'   segments. Each node holds a \code{R6PointStack} of vertices and the
    #'   graphical parameters for that segment.
    lines.dll = NULL,

    #' @field cur.par List. A snapshot of \code{par()} from the most recent
    #'   \code{plot()} call. May be useful if you want to manually add
    #' to the plot.
    cur.par = list(),

    #' @description
    #' Create a new \code{R6TurtleGraphics} object and optionally open a plot.
    #' @param add Logical. If \code{TRUE}, draw onto the existing device rather
    #'   than creating a new plot. Default \code{FALSE}.
    #' @param size Numeric. Half-extent of the plot window (\code{paper.size}).
    #'   Default \code{100}.
    #' @param show.turtle Logical. Whether to display the turtle icon.
    #'   Default \code{TRUE}.
    #' @param autoplot Logical. Whether to refresh the plot after each
    #'   operation. Default \code{TRUE}.
    initialize = function(add = FALSE, size = 100,
                          show.turtle = TRUE, autoplot = TRUE) {
      self$lines.dll   <- R6DLL$new()
      self$show.turtle <- show.turtle
      self$add         <- add
      self$autoplot    <- autoplot
      self$paper.size  <- size
      if (autoplot) {
        self$plot()
      }
    },

    #' @description
    #' Draw the turtle icon at the current position and heading. The turtle is
    #' rendered as a circle with a semi-circular head indicating the direction of travel.
    #' When the pen is down, a point marker is also drawn at the current
    #' position. Called internally by \code{plot()} and \code{Add()}.
    drawTurtle = function() {
      theta <- seq(0, 2 * pi, length.out = 100)
      lines(self$turtle.size * cos(theta) + self$cur.pos[1],
            self$turtle.size * sin(theta) + self$cur.pos[2],
            col = self$turtle.col)
      tmp   <- (self$cur.dir + self$angle.shift) * self$angle.scale
      theta <- seq(tmp - pi / 2, tmp + pi / 2, length.out = 50)
      lines(0.3 * self$turtle.size * cos(theta) +
              self$turtle.size * cos(tmp) + self$cur.pos[1],
            0.3 * self$turtle.size * sin(theta) +
              self$turtle.size * sin(tmp) + self$cur.pos[2],
            col = self$turtle.col)
      if (self$pen.down) {
        points(self$cur.pos[1], self$cur.pos[2],
               pch = self$pen.pch,
               col = self$cur.param$col,
               cex = self$pen.cex)
      }
    },

    #' @description
    #' Raise the pen. Subsequent movement will not draw lines.
    #' @return The \code{R6TurtleGraphics} object (invisibly).
    PenUp = function() {
      self$pen.down <- FALSE
      if (self$autoplot && self$show.turtle) {
        self$plot()
      }
      invisible(self)
    },

    #' @description
    #' Lower the pen. Subsequent movement will draw lines using the current
    #' graphical parameters (\code{cur.param}). Starts a new line segment in
    #' \code{lines.dll} beginning at the current position.
    #' @return The \code{R6TurtleGraphics} object (invisibly).
    PenDown = function() {
      self$pen.down <- TRUE
      tmp     <- new.env(parent = emptyenv())
      tmp$ps  <- R6PointStack$new()
      tmp$par <- self$cur.param
      tmp$ps$push(self$cur.pos)
      self$lines.dll$push(tmp)
      if (self$autoplot && self$show.turtle) {
        self$plot()
      }
      invisible(self)
    },

    #' @description
    #' Change graphical parameters for the next pen-down segment. If the pen
    #' is currently down it is lifted and re-lowered so the new parameters
    #' take effect on a fresh segment.
    #' @param ... Named graphical parameters passed to \code{lines()}, such as
    #'   \code{col} or \code{lwd}.
    #' The \code{cur.param} field is updated with the arguments, if you specify
    #' a color and line width with the first call, then only change the color
    #' with a second call, the line width from the first call will be preserved.
    #' @return The \code{R6TurtleGraphics} object (invisibly).
    NewPen = function(...) {
      pd  <- self$pen.down
      if (pd) self$PenUp()
      tmp <- list(...)
      for (i in names(tmp)) {
        self$cur.param[[i]] <- tmp[[i]]
      }
      if (pd) self$PenDown()
      invisible(self)
    },

    #' @description
    #' Redraw the complete picture. Unless \code{add = TRUE}, a new plot
    #' window is opened spanning \code{[-paper.size, paper.size]} on both
    #' axes with equal aspect ratio and no margins. All stored line segments
    #' and the turtle icon are then rendered via \code{Add()}.
    plot = function() {
      if (!self$add) {
        op <- par(mar = c(0, 0, 0, 0) + 0.1)
        on.exit(par(op))
        plot.new()
        plot.window(xlim = c(-self$paper.size, self$paper.size),
                    ylim = c(-self$paper.size, self$paper.size),
                    asp  = 1)
        self$cur.par <- par(no.readonly = TRUE)
      }
      self$Add()
    },

    #' @description
    #' Render all stored line segments and the turtle icon onto the current
    #' graphics device without creating a new plot. Useful when \code{add =
    #' TRUE} or when compositing multiple turtles on one canvas.
    #' @return The \code{R6TurtleGraphics} object (invisibly).
    Add = function() {
      self$lines.dll$iter_all(function(e) {
        tmp   <- e$par
        tmp$x <- e$ps$x
        tmp$y <- e$ps$y
        do.call(lines, tmp)
      })
      if (self$show.turtle) self$drawTurtle()
      return(invisible(self))
    },

    #' @description
    #' Set the turtle's heading to an absolute angle.
    #' @param angle Numeric. New heading in degrees. \code{0} points up;
    #'   positive values rotate clockwise.
    #' @return The \code{R6TurtleGraphics} object (invisibly).
    TurnTo = function(angle) {
      self$cur.dir <- angle
      if (self$autoplot && self$show.turtle) self$plot()
      invisible(self)
    },

    #' @description
    #' Teleport the turtle to an absolute position without drawing, even if
    #' the pen is down (the current path is ended and a new path is started at the new
    #' position).
    #' @param x Numeric. Target x-coordinate.
    #' @param y Numeric. Target y-coordinate.
    #' @return The \code{R6TurtleGraphics} object (invisibly).
    MoveTo = function(x, y) {
      tmp <- self$pen.down
      if (tmp) self$PenUp()
      self$cur.pos <- c(x, y)
      if (tmp) self$PenDown()
      if (self$autoplot) self$plot()
      invisible(self)
    },

    #' @description
    #' Draw a line directly to \code{(x, y)}, lowering the pen temporarily
    #' if it is currently up.
    #' @param x Numeric. Target x-coordinate.
    #' @param y Numeric. Target y-coordinate.
    #' @return The \code{R6TurtleGraphics} object (invisibly).
    DrawTo = function(x, y) {
      pd <- self$pen.down
      if (!pd) self$PenDown()
      tmp <- self$lines.dll$peek_last()
      tmp$ps$push(c(x, y))
      self$cur.pos <- c(x, y)
      if (!pd) self$PenUp()
      if (self$autoplot) self$plot()
      invisible(self)
    },

    #' @description
    #' Turn the turtle by a relative angle, updating \code{last.turn}.
    #' @param angle Numeric. Degrees to turn; positive values rotate clockwise.
    #' @return The \code{R6TurtleGraphics} object (invisibly).
    turn = function(angle) {
      self$last.turn <- angle
      self$TurnTo(self$cur.dir + angle)
    },

    #' @description
    #' Turn the turtle clockwise. Alias for \code{turn(angle)}.
    #' @param angle Numeric. Degrees to turn clockwise.
    #' @return The \code{R6TurtleGraphics} object (invisibly).
    right = function(angle) {
      self$turn(angle)
    },

    #' @description
    #' Turn the turtle counter-clockwise. Equivalent to \code{turn(-angle)}.
    #' @param angle Numeric. Degrees to turn counter-clockwise.
    #' @return The \code{R6TurtleGraphics} object (invisibly).
    left = function(angle) {
      self$turn(-angle)
    },

    #' @description
    #' Move the turtle forward by \code{dist} units in the current direction,
    #' updating \code{last.move}. Draws a line if the pen is down.
    #' @param dist Numeric. Distance to move. Negative values move backwards.
    #' @return The \code{R6TurtleGraphics} object (invisibly).
    move = function(dist) {
      self$last.move <- dist
      tmp.angle <- (self$cur.dir + self$angle.shift) * self$angle.scale
      new.x     <- self$cur.pos[1] + dist * cos(tmp.angle)
      new.y     <- self$cur.pos[2] + dist * sin(tmp.angle)
      if (self$pen.down) {
        self$DrawTo(new.x, new.y)
      } else {
        self$MoveTo(new.x, new.y)
      }
    },

    #' @description
    #' Move forward by \code{dist} units. Alias for \code{move(dist)}.
    #' @param dist Numeric. Distance to move forward.
    #' @return The \code{R6TurtleGraphics} object (invisibly).
    forward = function(dist) self$move(dist),

    #' @description
    #' Move backward by \code{dist} units. Equivalent to
    #' \code{move(-dist)}.
    #' @param dist Numeric. Distance to move backward.
    #' @return The \code{R6TurtleGraphics} object (invisibly).
    back = function(dist) self$move(-dist),

    #' @description
    #' No-op print method; suppresses the default R6 object summary.
    print = function() {
      return(invisible(self))
    }
  ),
  active = list(

    #' @field t Read: repeat the last turn. Assign \code{t <- angle}:
    #'   turn by \code{angle} degrees.
    t = function(a) {
      if (missing(a)) {
        self$turn(self$last.turn)
      } else {
        self$turn(a)
      }
    },

    #' @field r Read: turn clockwise by \code{abs(last.turn)}. Assign
    #'   \code{r <- angle}: turn clockwise by \code{angle} degrees.
    r = function(a) {
      if (missing(a)) {
        self$turn(abs(self$last.turn))
      } else {
        self$turn(a)
      }
    },

    #' @field l Read: turn counter-clockwise by \code{abs(last.turn)}.
    #'   Assign \code{l <- angle}: turn counter-clockwise by \code{angle}
    #'   degrees.
    l = function(a) {
      if (missing(a)) {
        self$turn(-abs(self$last.turn))
      } else {
        self$turn(-a)
      }
    },

    #' @field up Raise the pen (equivalent to \code{PenUp()}).
    up = function() {
      self$PenUp()
    },

    #' @field down Lower the pen (equivalent to \code{PenDown()}).
    down = function() {
      self$PenDown()
    },

    #' @field m Read: repeat the last move. Assign \code{m <- dist}:
    #'   move \code{dist} units in the current direction.
    m = function(dist) {
      if (missing(dist)) {
        self$move(self$last.move)
      } else {
        self$move(dist)
      }
    },

    #' @field f Read: move forward by \code{abs(last.move)}. Assign
    #'   \code{f <- dist}: move forward \code{dist} units.
    f = function(dist) {
      if (missing(dist)) {
        self$move(abs(self$last.move))
      } else {
        self$move(dist)
      }
    },

    #' @field b Read: move backward by \code{abs(last.move)}. Assign
    #'   \code{b <- dist}: move backward \code{dist} units.
    b = function(dist) {
      if (missing(dist)) {
        self$move(-abs(self$last.move))
      } else {
        self$move(-dist)
      }
    },

    #' @field show Show the turtle icon. Assign \code{show <- TRUE} to
    #'   display it or \code{show <- FALSE} to hide it.
    show = function(x = TRUE) {
      self$show.turtle <- x
      if (x) {
        self$drawTurtle()
      } else if (self$autoplot) {
        self$plot()
      }
    },

    #' @field hide Hide the turtle icon. Equivalent to \code{show <- FALSE}.
    hide = function(x = TRUE) {
      self$show <- !x
    }
  )
)
