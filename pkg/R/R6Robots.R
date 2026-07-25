#' Robots Game
#'
#' @description
#' An [R6::R6Class] object implementing a version of the classic unix
#' \code{robots} game. The player is placed on a grid along with a number
#' of robots. On each turn the player moves one step (or teleports), and
#' every robot then takes one step directly towards the player. Robots
#' that collide with each other (or with a previously formed junk pile)
#' are destroyed and turn into a junk pile. The player loses if a robot
#' moves onto the player's square or if the player colides with a junk pile; 
#' the player wins once every robot has
#' been destroyed.
#'
#' @details
#' The game board has \code{ncol} columns and \code{nrow} rows. Positions
#' are stored as two-column matrices of \code{(x, y)} coordinates.
#'
#' @examples
#' \dontrun{
#' g <- Robots$new(nrobots = 10)
#' g$move("N")
#' g$move("SE2")
#' g$move("T")
#' 
#'
#' ## interactive graphics ----
#' 
#' # open a graphics device that works with
#' # getGraphicsEvent
#' dev.new(width=6, height=3, noRStudioGD = TRUE) 
#' r <- Robots$new(30)
#' getGraphicsEvent(
#'   prompt='Ready',
#'   onKeybd=function(A){
#'     switch(A,
#'            q=r$move('nw'),
#'            w=r$move('n'),
#'            e=r$move('ne'),
#'            a=r$move('w'),
#'            s=r$move('.'),
#'            d=r$move('e'),
#'            z=r$move('sw'),
#'            x=r$move('s'),
#'            c=r$move('se'),
#'            t=r$move('t'),
#'            ' '=r$move('.'))
#'   }
#' )
#' }
#' 
#' @note
#' The \code{move} method does not return the object invisibly like
#' other R6 objects (for chaining purposes), the return value is
#' compatible with the \code{getGraphicsEvent} functions.
#'
#' @export
R6Robots <- R6Class('R6Robots', 
                  public = list(
                    #' @field nrow Number of rows on the game board.
                    nrow=20,
                    #' @field ncol Number of columns on the game board.
                    ncol=50,
                    #' @field player A one-row, two-column matrix giving the player's
                    #'   current \code{(x, y)} position. Becomes a zero-row matrix once
                    #'   the player has been destroyed by a robot.
                    player=matrix(ncol=2, nrow=1),
                    #' @field corpse A one-row, two-column matrix giving the
                    #'   \code{(x, y)} position at which the player was destroyed, used
                    #'   for plotting/printing after a loss. \code{NA} while the player
                    #'   is alive.
                    corpse=matrix(NA, ncol=2, nrow=1),
                    #' @field robots A two-column matrix with one row per living robot,
                    #'   giving each robot's current \code{(x, y)} position.
                    robots=matrix(ncol=2, nrow=0),
                    #' @field piles A two-column matrix with one row per junk pile,
                    #'   giving each pile's \code{(x, y)} position. Junk piles are
                    #'   formed where two or more robots collide, and are treated as
                    #'   obstacles that also destroy any robot that runs into them.
                    piles=matrix(ncol=2, nrow=0),
                    #' @description
                    #' Creates a new game. Places the player at the center of the board
                    #' and scatters \code{nrobots} robots at random, non-overlapping
                    #' locations.
                    #' @param nrobots Number of robots to create.
                    #' @param nrow,ncol Dimensions of the game board.
                    #' @param auto.print Whether to call \code{print()} automatically
                    #'   after each state change.
                    #' @param auto.plot Whether to call \code{plot()} automatically
                    #'   after each state change.
                    #' @return A new \code{Robots} object.
                    initialize= function(nrobots=10, nrow=20, ncol=50,
                                         auto.print=FALSE, auto.plot=TRUE) {
                      self$nrow <- nrow
                      self$ncol <- ncol
                      self$auto.print <- auto.print
                      self$auto.plot <- auto.plot
                      self$player[1,] <- c(round(self$ncol/2), round(self$nrow/2))
                      self$robots <- matrix(nrow=nrobots, ncol=2)
                      self$robots[,1] <- sample(self$ncol, nrobots, replace=TRUE)
                      self$robots[,2] <- sample(self$nrow, nrobots, replace=TRUE)
                      dups <- which(duplicated(rbind(self$player, self$robots)))-1
                      i <- 0
                      while(length(dups)) {
                        i <- i + 1
                        if(i > 100) stop('unable to place robots in clear space')
                        self$robots[dups,1] <- sample(self$ncol, length(dups), replace=TRUE)
                        self$robots[dups,2] <- sample(self$nrow, length(dups), replace=TRUE)
                        dups <- which(duplicated(rbind(self$player, self$robots)))-1
                      }
                      if(self$auto.print) self$print()
                      if(self$auto.plot) self$plot()
                    },
                    #' @field robot.pch Plotting character (\code{pch}) used to draw
                    #'   robots when \code{plot()} is called.
                    robot.pch=3,
                    #' @field player.pch Plotting character (\code{pch}) used to draw
                    #'   the player when \code{plot()} is called.
                    player.pch=16,
                    #' @field pile.pch Plotting character (\code{pch}) used to draw
                    #'   junk piles when \code{plot()} is called.
                    pile.pch=8,
                    #' @field robot.char Single character used to represent a robot
                    #'   when \code{print()} is called.
                    robot.char='+',
                    #' @field player.char Single character used to represent the
                    #'   player when \code{print()} is called.
                    player.char='O',
                    #' @field pile.char Single character used to represent a junk pile
                    #'   when \code{print()} is called.
                    pile.char='*',
                    #' @field corpse.char Single character used to represent the
                    #'   player's corpse when \code{print()} is called.
                    corpse.char='X',
                    #' @field auto.plot Logical. If \code{TRUE}, \code{plot()} is
                    #'   called automatically after \code{initialize()} and after
                    #'   every \code{move()}.
                    auto.plot=TRUE,
                    #' @field auto.print Logical. If \code{TRUE}, \code{print()} is
                    #'   called automatically after \code{initialize()} and after
                    #'   every \code{move()}.
                    auto.print=FALSE,
                    #' @description
                    #' Draws the current board state using base graphics: a
                    #' checkerboard background, and points for the player, corpse,
                    #' robots, and junk piles. Displays a warning title if a robot is
                    #' within one move of the player.
                    #' @return \code{NULL}, invisibly. Called for its side effect of
                    #'   drawing a plot.
                    plot = function() {
                      old.par <- par(mar=rep(1,4)+0.1)
                      on.exit(par(old.par))
                      plot.new()
                      plot.window(xlim=c(1,self$ncol), ylim=c(1,self$nrow))
                      tmp <- matrix(0, nrow=self$ncol, ncol=self$nrow)
                      tmp <- (row(tmp) + col(tmp)) %% 2
                      image(seq_len(self$ncol), seq_len(self$nrow), tmp, col=c('white','lightgrey'), add=TRUE)
                      box()
                      points(self$player, pch=self$player.pch)
                      points(self$corpse, pch=self$player.pch, col='red')
                      points(self$robots, pch=self$robot.pch)
                      points(self$piles,  pch=self$pile.pch)
                      d <- apply(self$robots, 1, function(x){
                        (x[1]-self$player[1])^2 + (x[2]-self$player[2])^2
                      })
                      if(nrow(self$player) & any(d < 3)){ title(main='Warning: Robot within 1 move')}
                    },
                    #' @description
                    #' Prints an ASCII-art representation of the current board state
                    #' to the console, bordered by \code{-} and \code{|} characters.
                    #' Prints a warning message if a robot is within one move of the
                    #' player.
                    #' @return \code{NULL}, invisibly. Called for its side effect of
                    #'   printing to the console.
                    print = function() {
                      tmp <- matrix(" ", ncol=self$ncol, nrow=self$nrow)
                      tmp[self$player[,2:1, drop=FALSE]] <- self$player.char
                      tmp[self$robots[,2:1, drop=FALSE]] <- self$robot.char
                      tmp[self$piles[,2:1, drop=FALSE]]  <- self$pile.char
                      tmp[self$corpse[,2:1, drop=FALSE]] <- self$corpse.char
                      tmp <- tmp[rev(seq_len(self$nrow)),]
                      tmp <- cbind("|", tmp, "|")
                      tmp <- rbind("-", tmp, "-")
                      tmp2 <- apply(tmp, 1, paste, collapse='')
                      tmp3 <- paste(tmp2, collapse='\n')
                      cat(tmp3,"\n")
                      d <- apply(self$robots, 1, function(x){
                        (x[1]-self$player[1])^2 + (x[2]-self$player[2])^2
                      })
                      if(nrow(self$player) & any(d < 3)){ cat('Warning: Robot within 1 move\n')}
                    },
                    #' @field safe.teleport Logical. If \code{TRUE}, teleporting
                    #'   (\code{move("T")}) will keep re-rolling a random destination
                    #'   until it lands at least two squares (in squared distance)
                    #'   away from every robot and pile, so the player cannot teleport
                    #'   directly into danger.
                    safe.teleport = TRUE,
                    #' @field move.mat A 3x3 character matrix mapping relative grid
                    #'   position to compass-direction move codes (\code{"N"},
                    #'   \code{"NE"}, \code{"E"}, \code{"SE"}, \code{"S"}, \code{"SW"},
                    #'   \code{"W"}, \code{"NW"}, and \code{"."} for no movement),
                    #'   used internally by \code{move()} to translate a direction
                    #'   string into a change in position.
                    move.mat = matrix(c('NW', 'W', 'SW',
                                        'N', '.', 'S',
                                        'NE', 'E', 'SE'), 3, 3),
                    #' @description
                    #' Advances the game by one turn. The player is moved first, then
                    #' every robot takes one step towards the player's new position.
                    #' Colliding robots become junk piles, and any robot landing on
                    #' the player ends the game. If \code{auto.print} or
                    #' \code{auto.plot} is \code{TRUE}, the board is redrawn after the
                    #' move.
                    #' @param dir A direction string, one of \code{"N"}, \code{"NE"},
                    #'   \code{"E"}, \code{"SE"}, \code{"S"}, \code{"SW"}, \code{"W"},
                    #'   \code{"NW"}, \code{"."} (stay put), or \code{"T"} (teleport
                    #'   to a random square). A direction may be followed by a number
                    #'   of steps, e.g. \code{"N3"} to move north three times in
                    #'   succession.
                    #' @return \code{NULL}, invisibly, or (if the game has ended)
                    #'   \code{invisible(1)}.
                    move = function(dir) {
                      steps <- 1
                      d1 <- sub('[0-9]+', '', dir)
                      d2 <- as.numeric(sub('[^0-9]+', '', dir))
                      w <- which(self$move.mat == toupper(d1))
                      if(length(w) == 0) {
                        if(toupper(d1)=="T") {
                          self$player[1] <- sample(self$ncol,1)
                          self$player[2] <- sample(self$nrow,1)
                          while(self$safe.teleport){
                            d <- apply(rbind(self$robots,self$piles), 1, function(x){
                              (x[1]-self$player[1])^2 + (x[2]-self$player[2])^2
                            })
                            if(nrow(self$player) & any(d<3)) {
                              self$player[1] <- sample(self$ncol,1)
                              self$player[2] <- sample(self$nrow,1)
                            } else {
                              self$safe.teleport <- FALSE
                            }
                          }
                        }
                        w <- 5
                      }
                      if(!is.na(d2)) {
                        steps <- d2
                      }
                      for(i in seq_len(steps)) {
                        if( (w %in% c(1,4,7)) && self$player[2] < self$nrow) {
                          self$player[2] <- self$player[2] + 1
                        }
                        if( (w %in% c(3,6,9)) && self$player[2] > 1) {
                          self$player[2] <- self$player[2] - 1
                        }
                        if( (w %in% c(1,2,3)) && self$player[1] > 1 ) {
                          self$player[1] <- self$player[1] - 1
                        }
                        if( (w %in% c(7,8,9)) && self$player[1] < self$ncol) {
                          self$player[1] <- self$player[1] + 1
                        }
                        self$robots[,1] <- self$robots[,1] + sign(self$player[1]-self$robots[,1])
                        self$robots[,2] <- self$robots[,2] + sign(self$player[2]-self$robots[,2])
                        if(anyDuplicated(rbind(self$player, self$robots, self$piles))) {
                          w <- duplicated(rbind(self$robots, self$piles)) | duplicated(rbind(self$robots, self$piles), fromLast=TRUE)
                          w <- w[seq_len(nrow(self$robots))]
                          self$piles <- unique(rbind(self$piles, self$robots[w,]))
                          self$robots <- self$robots[!w,,drop=FALSE]
                          if(nrow(self$robots)==0) {
                            cat("Congratulations!")
                          }
                          if(duplicated(rbind(self$player, self$robots, self$piles), fromLast=TRUE)[1]) {
                            cat("You have been munched by a robot\n")
                            self$corpse <- self$player
                            self$player <- self$player[numeric(0),]
                          }
                        }
                      }
                      if(self$auto.print) self$print()
                      if(self$auto.plot) { 
                        self$plot()
                        if(length(self$player)==0) {
                          title(main='You have been munched by a robot')
                          return(invisible(1))
                        }
                        if(nrow(self$robots)==0) {
                          title(main='Congratulations!')
                          return(invisible(1))
                        }
                      }
                    }
                  ))