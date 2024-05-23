library(R6)

TurtleGraphics <- R6Class("TurtleGraphics",
  public = list(
    show.turtle=TRUE,
    initialize=function(size=100, show.turtle=TRUE) {
      op <- par(mar=c(0,0,0,0)+0.1)
      on.exit(par(op))
      plot.new()
      plot.window(xlim=c(-size,size), 
                  ylim=c(-size,size),
                  asp=1)
      self$cur.pos=c(0,0)
      self$cur.dir=0
      self$show.turtle <- show.turtle
      self$rp <- sprintf("rp%04d", floor(runif(1, 0, 10000)))
      self$recordedplot[[self$rp]] <- recordPlot()
      if(self$show.turtle) {
        self$drawTurtle()
      }
    },
    cur.pos = c(0,0),
    cur.dir = 0,
    cur.color = 'blue',
    cur.lwd = 1,
    last.turn = 15,
    last.move = 25,
    pen.down = FALSE,
    angle.shift = -90,
    angle.scale = -pi/180,
    recordedplot = new.env(parent=emptyenv()),
    rp = "",
    turtle.col = "forestgreen",
    drawTurtle = function() {
      theta <- seq(0,2*pi, length=100)
      lines(10*cos(theta)+self$cur.pos[1], 
            10*sin(theta)+self$cur.pos[2], 
            col=self$turtle.col)
      tmp <- (self$cur.dir + self$angle.shift)*self$angle.scale
      theta <- seq(tmp-pi/2, tmp+pi/2, length=50)
      lines(3*cos(theta)+cos(tmp)*10+self$cur.pos[1],
            3*sin(theta)+sin(tmp)*10+self$cur.pos[2],
            col=self$turtle.col)
      if(self$pen.down) {
        points(self$cur.pos[1], self$cur.pos[2], col=self$cur.color,
               cex=2)
      }
    },
    MoveTo = function(x,y) {
      replayPlot(self$recordedplot[[self$rp]])
      self$cur.pos <- c(x,y)
      if(self$show.turtle) self$drawTurtle() 
      invisible(self)
    },
    DrawTo = function(x,y) {
      replayPlot(self$recordedplot[[self$rp]])
      lines(c(self$cur.pos[1], x),
            c(self$cur.pos[2], y),
            col=self$cur.color,
            lwd=self$cur.lwd)
      self$cur.pos <- c(x,y)
      self$recordedplot[[self$rp]] <- recordPlot()
      if(self$show.turtle) self$drawTurtle()
      invisible(self)
    },
    TurnTo = function(angle) {
      replayPlot(self$recordedplot[[self$rp]])
      self$cur.dir <- angle
      if(self$show.turtle) self$drawTurtle()
      invisible(self)
    },
    turn = function(angle) {
      self$last.turn <- angle
      self$TurnTo(self$cur.dir + angle)
    },
    right = function(angle) {
      self$last.turn <- angle
      self$Turn(angle)
    },
    left = function(angle) {
      self$last.turn <- -angle
      self$Turn(-angle)
    },
    PenDown = function() {
      self$pen.down=TRUE
      if(self$show.turtle) self$drawTurtle() 
      invisible(self)
    },
    PenUp = function() {
      self$pen.down=FALSE
      if(self$show.turtle) self$drawTurtle() 
      invisible(self)
    },
    move = function(dist) {
      self$last.move <- dist
      tmp.angle <- (self$cur.dir + self$angle.shift)*self$angle.scale
      new.x <- self$cur.pos[1] + dist*cos(tmp.angle)
      new.y <- self$cur.pos[2] + dist*sin(tmp.angle)
      if(self$pen.down) {
        self$DrawTo(new.x, new.y)
      } else {
        self$MoveTo(new.x, new.y)
      }
      invisible(self)
    },
    forward = function(dist) self$move(dist),
    back = function(dist) self$move(-dist)
  ),
  active = list(
    t = function(a) {
      if(missing(a)) {
        self$turn(self$last.turn)
      } else {
        self$turn(a)
      }
    },
    r = function(a) {
      if(missing(a)) {
        self$turn(abs(self$last.turn))
      } else {
        self$turn(a)
      }
    },
    l = function(a) {
      if(missing(a)) {
        self$turn(-abs(self$last.turn))
      } else {
        self$turn(-a)
      }
    },
    up = function(x) {
      self$PenUp()
      invisible(self)
    },
    down = function(x) {
      self$PenDown()
      invisible(self)
    },
    m = function(dist) {
      if(missing(dist)) {
        self$move(self$last.dist)
      } else {
        self$move(dist)
      }
    },
    f = function(dist) {
      if(missing(dist)) {
        self$move(abs(self$last.dist))
      } else {
        self$move(dist)
      }
    },
    b = function(dist) {
      if(missing(dist)) {
        self$move(-abs(self$last.dist))
      } else {
        self$move(dist)
      }
    },
    show = function() {
      self$show.turtle <- TRUE  
      self$drawTurtle()
      invisible(self)
    },
    hide = function() {
      self$show.turtle <- FALSE
      replayPlot(self$recordedplot[[self$rp]])
      invisible(self)
    }
  )
)

