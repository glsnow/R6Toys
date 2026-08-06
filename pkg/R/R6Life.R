#' Conway's Game of Life
#'
#' @description
#' An R6 class that simulates Conway's Game of Life on a two-dimensional
#' integer grid. Live cells are stored as a two-column matrix of (x, y)
#' coordinates, and each call to \code{$tick()} advances the simulation by
#' one or more generations according to the classic rules.
#'
#' @details
#' The Game of Life is a zero-player cellular automaton devised by the
#' mathematician John Horton Conway in 1970. The universe is an infinite
#' (or bounded) two-dimensional grid of square cells, each of which is either
#' \emph{alive} or \emph{dead}. At each generation the following rules are
#' applied simultaneously to every cell:
#' \enumerate{
#'   \item A live cell with fewer than 2 live neighbours dies
#'     (underpopulation).
#'   \item A live cell with 2 or 3 live neighbours survives.
#'   \item A live cell with more than 3 live neighbours dies
#'     (overpopulation).
#'   \item A dead cell with exactly 3 live neighbours becomes alive
#'     (reproduction).
#' }
#'
#' Neighbours are the 8 cells surrounding a given cell (Moore
#' neighbourhood).
#'
#' @section Boundary conditions:
#' The \code{boundary} field controls how cells that move or are born outside
#' the current grid extent are handled:
#' \describe{
#'   \item{\code{"expand"} (default)}{The grid limits (\code{xlim},
#'     \code{ylim}) grow automatically to contain all live cells. The
#'     simulation is effectively unbounded.}
#'   \item{\code{"torus"} or \code{"wrap"}}{The grid wraps toroidally: cells
#'     that leave one edge re-enter from the opposite edge.}
#'   \item{\code{"flat"} or \code{"cut"}}{Cells born or moving outside the
#'     fixed grid limits are removed.}
#' }
#'
#' @references
#' Wikipedia contributors. \emph{Conway's Game of Life}. Wikipedia, The Free
#' Encyclopedia.
#' \url{https://en.wikipedia.org/wiki/Conway\%27s_Game_of_Life}
#'
#' Gardner, M. (1970). Mathematical Games: The fantastic combinations of John
#' Conway's new solitaire game "life". \emph{Scientific American},
#' \strong{223}(4), 120--123.
#'
#' @examples
#' ## A glider — a small pattern that travels diagonally across the grid
#' g <- R6Life$new(xlim = c(1, 20), ylim = c(1, 20), boundary = "expand")
#' g$autoplot <- FALSE
#'
#' # Set up the glider pattern
#' g$add(c(2, 3))
#' g$add(c(3, 1))
#' g$add(c(3, 3))
#' g$add(c(4, 2))
#' g$add(c(4, 3))
#'
#' g$plot()
#'
#' # Advance 20 generations
#' if(interactive()) {
#'   g$tick(n = 20, pause = 0.15, plot = TRUE)
#' } else {
#'   g$tick(n = 20, pause = 0, plot = FALSE)
#'   g$plot()
#' }
#'
#' ## A blinker — a period-2 oscillator
#' b <- R6Life$new(xlim = c(1, 10), ylim = c(1, 10), boundary = "flat")
#' b$autoplot <- FALSE
#' b$add(c(5, 4))
#' b$add(c(5, 5))
#' b$add(c(5, 6))
#' b$plot()
#'
#' if(interactive()) {
#'   b$tick(n = 20, pause = 0.3, plot = TRUE)
#' } else {
#'   b$tick(n = 20, pause = 0, plot = FALSE)
#'   b$plot()
#' }
#'
#' ## Torus (wrap-around) boundary with a glider
#' tw <- R6Life$new(xlim = c(1, 15), ylim = c(1, 15), boundary = "torus")
#' tw$autoplot <- FALSE
#' tw$add(c(5, 3))$add(c(6, 1))$add(c(6, 3))
#' tw$add(c(7, 2))$add(c(7, 3))
#' 
#' if(interactive()) {
#'   tw$tick(n = 100, pause = 0.1, plot = TRUE)
#' } else {
#'   tw$tick(n = 100, pause = 0, plot = FALSE)
#'   tw$plot()
#' }
#'
#' @export
R6Life <- R6Class("R6Life",
  public = list(

    #' @field cur Two-column integer matrix. Each row is the \code{(x, y)}
    #'   coordinate of a live cell in the current generation. \code{NULL}
    #'   until \code{$initialize()} is called.
    cur = NULL,

    #' @field prev Two-column integer matrix. The live cell coordinates from
    #'   the generation immediately before the most recent \code{$tick()}.
    #'   \code{NULL} before the first tick. Used by \code{$plotchange()} to
    #'   highlight births and deaths.
    prev = NULL,

    #' @field xlim Integer vector of length 2. The \code{c(min, max)}
    #'   x-extent of the grid. Grows automatically when
    #'   \code{boundary = "expand"}.
    xlim = NA,

    #' @field ylim Integer vector of length 2. The \code{c(min, max)}
    #'   y-extent of the grid. Grows automatically when
    #'   \code{boundary = "expand"}.
    ylim = NA,

    #' @field lifecol Character. Colour used to fill live cells. Default
    #'   \code{"black"}.
    lifecol = "black",

    #' @field deadcol Character. Colour used to fill dead (empty) cells.
    #'   Default \code{"white"}.
    deadcol = "white",

    #' @field bordercol Character. Colour of the grid lines drawn between
    #'   cells. Default \code{"grey"}.
    bordercol = "grey",

    #' @field boundary Character. Boundary condition applied after each
    #'   generation. One of \code{"expand"} (default), \code{"torus"} /
    #'   \code{"wrap"}, or \code{"flat"} / \code{"cut"}. See the Boundary
    #'   conditions section for details.
    boundary = "expand",

    #' @field autoplot Logical. If \code{TRUE}, \code{$plot()} is called
    #'   automatically after \code{$add()}, \code{$remove()},
    #'   \code{$toggle()}, and each generation step in \code{$tick()}.
    #'   Default \code{TRUE}.
    autoplot = TRUE,

    #' @field pause Numeric. Seconds to pause between generation plots when
    #'   \code{$tick()} advances multiple generations. Default \code{0.1}.
    pause = 0.1,

    #' @description
    #' Create a new \code{R6Life} object with an empty grid.
    #' @param xlim Integer vector of length 2. Initial x-extent of the grid.
    #'   Default \code{c(1, 25)}.
    #' @param ylim Integer vector of length 2. Initial y-extent of the grid.
    #'   Default \code{c(1, 25)}.
    #' @param boundary Character. Boundary condition; one of
    #'   \code{"expand"}, \code{"torus"}, \code{"wrap"}, \code{"flat"}, or
    #'   \code{"cut"}. Default \code{"expand"}.
    initialize = function(xlim = c(1, 25), ylim = c(1, 25),
                          boundary = "expand") {
      self$xlim     <- xlim
      self$ylim     <- ylim
      self$cur      <- matrix(ncol = 2, nrow = 0)
      self$boundary <- boundary
    },

    #' @description
    #' Apply the current boundary condition to \code{self$cur}. Called
    #' internally by \code{$tick()} after each generation step; it is rarely
    #' necessary to call this directly.
    #' \itemize{
    #'   \item \code{"expand"} — expands \code{xlim} and \code{ylim} to
    #'     contain all live cells.
    #'   \item \code{"torus"} / \code{"wrap"} — wraps coordinates modularly
    #'     within the fixed grid limits.
    #'   \item \code{"flat"} / \code{"cut"} — removes any live cells outside
    #'     the fixed grid limits.
    #' }
    #' @return The \code{R6Life} object (invisibly).
    applyBound = function() {
      if (self$boundary == "expand") {
        self$xlim <- range(self$xlim, self$cur[, 1])
        self$ylim <- range(self$ylim, self$cur[, 2])
      } else if (self$boundary %in% c("torus", "wrap")) {
        tmp.x    <- self$xlim[1]
        tmp.y    <- self$ylim[1]
        tmp      <- self$cur
        tmp[, 1] <- (tmp[, 1] - tmp.x) %% (diff(self$xlim) + 1) + tmp.x
        tmp[, 2] <- (tmp[, 2] - tmp.y) %% (diff(self$ylim) + 1) + tmp.y
        self$cur <- tmp
      } else if (self$boundary %in% c("flat", "cut")) {
        w        <- self$xlim[1] <= self$cur[, 1] &
                    self$cur[, 1] <= self$xlim[2] &
                    self$ylim[1] <= self$cur[, 2] &
                    self$cur[, 2] <= self$ylim[2]
        self$cur <- self$cur[w, ]
      }
      invisible(self)
    },

    #' @description
    #' Add a single live cell to the grid. If the cell is already alive the
    #' call is a no-op. Updates the plot if \code{autoplot} is \code{TRUE}.
    #' @param xy Integer vector of length 2. The \code{c(x, y)} coordinate of
    #'   the cell to add.
    #' @return The \code{R6Life} object (invisibly).
    add = function(xy) {
      w <- self$cur[, 1] == xy[1] & self$cur[, 2] == xy[2]
      if (!any(w)) {
        self$cur <- rbind(self$cur, xy)
      }
      if (self$autoplot) self$plot()
      invisible(self)
    },

    #' @description
    #' Remove a single live cell from the grid. If the cell is already dead
    #' the call is a no-op. Updates the plot if \code{autoplot} is
    #' \code{TRUE}.
    #' @param xy Integer vector of length 2. The \code{c(x, y)} coordinate of
    #'   the cell to remove.
    #' @return The \code{R6Life} object (invisibly).
    remove = function(xy) {
      w <- self$cur[, 1] == xy[1] & self$cur[, 2] == xy[2]
      if (any(w)) {
        self$cur <- self$cur[!w, , drop = FALSE]
      }
      if (self$autoplot) self$plot()
      invisible(self)
    },

    #' @description
    #' Toggle the state of a single cell: kill it if it is alive, or bring it
    #' to life if it is dead. Updates the plot if \code{autoplot} is
    #' \code{TRUE}.
    #' @param xy Integer vector of length 2. The \code{c(x, y)} coordinate of
    #'   the cell to toggle.
    #' @return The \code{R6Life} object (invisibly).
    toggle = function(xy) {
      w <- self$cur[, 1] == xy[1] & self$cur[, 2] == xy[2]
      if (any(w)) {
        self$cur <- self$cur[!w, , drop = FALSE]
      } else {
        self$cur <- rbind(self$cur, xy)
      }
      if (self$autoplot) self$plot()
      invisible(self)
    },

    #' @description
    #' Draw the current generation. Live cells are filled with
    #' \code{lifecol}, dead cells with \code{deadcol}, and grid lines are
    #' drawn in \code{bordercol}. The plot uses a square aspect ratio with no
    #' margins.
    #' @return The \code{R6Life} object (invisibly).
    plot = function() {
      op <- par(mar = c(0, 0, 0, 0) + 0.1, pty = "s")
      on.exit(par(op))
      plot.new()
      plot.window(xlim = self$xlim + c(-0.5, 0.5),
                  ylim = self$ylim + c(-0.5, 0.5),
                  asp  = 1)
      clip(self$xlim[1] - 0.51, self$xlim[2] + 0.51,
           self$ylim[1] - 0.51, self$ylim[2] + 0.51)
      rect(self$xlim[1] - 0.5, self$ylim[1] - 0.5,
           self$xlim[2] + 0.5, self$ylim[2] + 0.5,
           col = self$deadcol, border = NA)
      for (i in seq_len(nrow(self$cur))) {
        rect(self$cur[i, 1] - 0.5, self$cur[i, 2] - 0.5,
             self$cur[i, 1] + 0.5, self$cur[i, 2] + 0.5,
             col = self$lifecol, border = NA)
      }
      abline(h = seq(self$ylim[1] - 0.5, self$ylim[2] + 0.5),
             col = self$bordercol)
      abline(v = seq(self$xlim[1] - 0.5, self$xlim[2] + 0.5),
             col = self$bordercol)
      invisible(self)
    },

    #' @description
    #' Draw the current generation with births and deaths highlighted.
    #' Cells that were alive in both the previous and current generation are
    #' shown in \code{lifecol}; newly born cells in \code{birthcol}; cells
    #' that died in \code{deathcol}. Has no effect if \code{$prev} is
    #' \code{NULL} (i.e. before the first \code{$tick()}).
    #' @param birthcol Character. Colour for newly born cells.
    #'   Default \code{"green"}.
    #' @param deathcol Character. Colour for cells that just died.
    #'   Default \code{"red"}.
    #' @return The \code{R6Life} object (invisibly).
    plotchange = function(birthcol = "green", deathcol = "red") {
      if (is.null(self$prev)) return(invisible(self))
      op <- par(mar = c(0, 0, 0, 0) + 0.1, pty = "s")
      on.exit(par(op))
      plot.new()
      plot.window(xlim = self$xlim + c(-0.5, 0.5),
                  ylim = self$ylim + c(-0.5, 0.5),
                  asp  = 1)
      clip(self$xlim[1] - 0.51, self$xlim[2] + 0.51,
           self$ylim[1] - 0.51, self$ylim[2] + 0.51)
      rect(self$xlim[1] - 0.5, self$ylim[1] - 0.5,
           self$xlim[2] + 0.5, self$ylim[2] + 0.5,
           col = self$deadcol, border = NA)
      for (i in seq_len(nrow(self$cur))) {
        if (any(self$prev[, 1] == self$cur[i, 1] &
                self$prev[, 2] == self$cur[i, 2])) {
          rect(self$cur[i, 1] - 0.5, self$cur[i, 2] - 0.5,
               self$cur[i, 1] + 0.5, self$cur[i, 2] + 0.5,
               col = self$lifecol, border = NA)
        } else {
          rect(self$cur[i, 1] - 0.5, self$cur[i, 2] - 0.5,
               self$cur[i, 1] + 0.5, self$cur[i, 2] + 0.5,
               col = birthcol, border = NA)
        }
      }
      for (i in seq_len(nrow(self$prev))) {
        if (!any(self$prev[i, 1] == self$cur[, 1] &
                 self$prev[i, 2] == self$cur[i, 2])) {
          rect(self$prev[i, 1] - 0.5, self$prev[i, 2] - 0.5,
               self$prev[i, 1] + 0.5, self$prev[i, 2] + 0.5,
               col = deathcol, border = NA)
        }
      }
      abline(h = seq(self$ylim[1] - 0.5, self$ylim[2] + 0.5),
             col = self$bordercol)
      abline(v = seq(self$xlim[1] - 0.5, self$xlim[2] + 0.5),
             col = self$bordercol)
      invisible(self)
    },

    #' @description
    #' Advance the simulation by \code{n} generations. At each step the four
    #' Conway rules are applied simultaneously to all cells using a
    #' neighbour-count matrix, \code{self$prev} is updated, the boundary
    #' condition is enforced via \code{$applyBound()}, and the grid is
    #' re-plotted if \code{plot = TRUE} (with a \code{self$pause}-second delay
    #' between frames when \code{n > 1}).
    #' @param n Positive integer. Number of generations to advance.
    #'   Default \code{1}.
    #' @param pause Numeric. Delay between frames in seconds. Default \code{self$pause}.
    #' @param plot Logical. Whether to plot after each generation.
    #'   Defaults to \code{self$autoplot}.
    #' @return The \code{R6Life} object (invisibly).
    tick = function(n = 1, pause = self$pause, plot = self$autoplot) {
      nbr <- expand.grid(-1:1, -1:1)
      ii  <- R6Counter$new(n)
      while (ii$mmm) {
        self$prev <- self$cur
        tmp       <- self$cur
        if (nrow(tmp) < 1) break

        if (self$boundary %in% c("torus", "wrap")) {
          tmp.x <- tmp.y <- -1
          tmp[, 1] <- tmp[, 1] - tmp.x
          tmp[, 2] <- tmp[, 2] - tmp.y
          tmp.mat <- tmp.mat2 <- matrix(0,
            ncol = self$ylim[2] + 2,
            nrow = self$xlim[2] + 2)
        } else {
          tmp.x    <- min(tmp[, 1]) - 2
          tmp.y    <- min(tmp[, 2]) - 2
          tmp[, 1] <- tmp[, 1] - tmp.x
          tmp[, 2] <- tmp[, 2] - tmp.y
          tmp.mat  <- tmp.mat2 <- matrix(0,
            ncol = max(tmp[, 2]) + 1,
            nrow = max(tmp[, 1]) + 1)
        }

        for (i in seq_len(nrow(tmp))) {
          tmp2           <- cbind(nbr[, 1] + tmp[i, 1], nbr[, 2] + tmp[i, 2])
          tmp.mat[tmp2]  <- tmp.mat[tmp2] + 1
        }
        if (self$boundary %in% c("torus", "wrap")) {
          tmp.mat[2, ]                    <- tmp.mat[2, ] + tmp.mat[nrow(tmp.mat), ]
          tmp.mat[1, ]                    <- tmp.mat[1, ] + tmp.mat[nrow(tmp.mat) - 1, ]
          tmp.mat[, 2]                    <- tmp.mat[, 2] + tmp.mat[, ncol(tmp.mat)]
          tmp.mat[, 1]                    <- tmp.mat[, 1] + tmp.mat[, ncol(tmp.mat) - 1]
          tmp.mat[nrow(tmp.mat) + c(-1, 0), ] <- 0
          tmp.mat[, ncol(tmp.mat) + c(-1, 0)] <- 0
        }
        tmp.mat2[tmp]              <- 1
        if (self$boundary %in% c("torus", "wrap")) {
          tmp.mat2[1, ]  <- tmp.mat2[nrow(tmp.mat2) - 1, ]
          tmp.mat2[, 1]  <- tmp.mat2[, nrow(tmp.mat2) - 1]
        }
        tmp.mat2[tmp.mat == 3]              <- 1
        tmp.mat2[tmp.mat < 3 | tmp.mat > 4] <- 0
        self$cur    <- which(tmp.mat2 == 1, arr.ind = TRUE)
        self$cur[, 1] <- self$cur[, 1] + tmp.x
        self$cur[, 2] <- self$cur[, 2] + tmp.y
        self$applyBound()
        if (plot) {
          self$plot()
          if (ii$value) Sys.sleep(pause)
        }
      }
      invisible(self)
    }
  )
)
