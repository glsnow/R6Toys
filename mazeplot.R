library(R6)

PointStack <- R6Class("PointStack",
                      public = list(
                        length=0,
                        push = function(x,y) {
                          self$length <- self$length + 1
                          if(length(private$xx) <= self$length) {
                            private$xx <- c(private$xx, rep(NA, self$length))
                            private$yy <- c(private$yy, rep(NA, self$length))
                          }
                          if(missing(y)) {
                            private$xx[self$length] <- x[1]
                            private$yy[self$length] <- x[2]
                          } else {
                            private$xx[self$length] <- x
                            private$yy[self$length] <- y
                          }
                        },
                        pop = function() {
                          if(self$length < 1) return(NULL)
                          ol <- self$length
                          self$length <- ol - 1
                          return(c(private$xx[ol], private$yy[ol]))
                        }
                      ),
                      private = list(
                        xx = numeric(10),
                        yy = numeric(10)
                      ),
                      active=list(
                        x = function() private$xx[seq_len(self$length)],
                        y = function() private$yy[seq_len(self$length)]
                      )
)

dirmat <- cbind(c(0,2,0,-2), c(2,0,-2,0))

circVec <- R6Class("circVec",
                   public=list(
                     value=NULL,
                     initialize=function(x) {
                       self$value <- x
                     },
                     val = function(i) {
                       ii <- (i-1) %% length(self$value) + 1
                       self$value[ii]
                     },
                     right = function(i) {
                       ii <- (i) %% length(self$value) + 1
                       self$value[ii]
                     },
                     left = function(i) {
                       ii <- (i-2)%%length(self$value) + 1
                       self$value[ii]
                     }
                   )
                   )



cols <- circVec$new(c('lightblue', 'yellow', 'green', 'pink'))

eye.param <- c(0.5, 0.5, 1)

plotProjRect <- function(xyz, eye.param=eye.param, fill=NA) {
  x.new <- (xyz[,1]-eye.param[1])*(eye.param[3])/(xyz[,3]+eye.param[3]) + eye.param[1]
  y.new <- (xyz[,2]-eye.param[2])*(eye.param[3])/(xyz[,3]+eye.param[3]) + eye.param[2]
  polygon(x.new, y.new, border='black', col=fill)
}

plotmaze2d <- function(maze, position, direction=NA,
                       walladj=0.3) {
  old.par <- par(mar=c(0,0,0,0)+0.1)
  on.exit(par(old.par))
  nr <- nrow(maze)
  nc <- ncol(maze)
  
  gcols <- rev(grey.colors(3))
  
  tmp.x <- seq(0.5, nr+0.5) + c(-walladj, walladj)
  tmp.x[1] <- tmp.x[2] - 0.5
  tmp.x[nr+1] <- tmp.x[nr] + 0.5
  
  tmp.y <- seq(0.5, nc+0.5) + c(-walladj, walladj)
  tmp.y[1] <- tmp.y[2] - 0.5
  tmp.y[nc+1] <- tmp.y[nc] + 0.5
  
  image(tmp.x, tmp.y, maze, xlab='', ylab='', xaxt='n', yaxt='n', asp=1, bty='n',
        col = gcols)
  
  if(!missing(position)) {
    if(is.na(direction)) {
      points(position[1], position[2], pch=16, cex=1.5, col='blue')
    } else {
      trimat <- rbind(c(0,0.35),
                      c(0.2,-0.2),
                      c(-0.2,-0.2))
      if(direction > 2) trimat[,2] <- -trimat[,2]
      if(direction %% 2 == 0) trimat <- trimat[,c(2,1)]
      polygon(position[1]+trimat[,1], position[2]+trimat[,2],
            border='blue', lwd=2)
    }
  }
}

plotmaze2d(maze, start, 1)

plotmaze3d <- function(maze, position, direction) {
  m.pts <- PointStack$new()
  
  cur.pt <- position
  dirmat <- dirmat/2
  dirvec <- circVec$new(1:4)

  while(maze[cur.pt[1], cur.pt[2]]==0){
    m.pts$push(cur.pt)
    cur.pt <- cur.pt + dirmat[direction,]
  }

  old.par <- par(mar=c(0,0,0,0)+0.1)
  on.exit(par(old.par))
  plot.new()
  plot.window(0:1, 0:1, asp=1)
  rect(0,0,1,1, col='lightgrey', border='black')
  clip(0,1,0,1)
  
  while(m.pts$length) {
    cur.pt <- m.pts$pop()
    # dist <- (cur.pt[1]-position[1])*dirmat[direction,1] + 
    #   (cur.pt[2]-position[2])*dirmat[direction,2]
    dist <- m.pts$length + 0.5
    if(maze[rbind(cur.pt+dirmat[direction,])]==1) {  # draw back wall
      plotProjRect(cbind(c(0,0,1,1), c(0,1,1,0), dist),
                   eye.param, cols$val(direction))
    }
    plotProjRect(cbind(c(0,1,1,0), 1, pmax(0,c(dist-1, dist-1, dist, dist))),
                 eye.param, 'grey') # ceiling
    plotProjRect(cbind(c(0,1,1,0), 0, pmax(0,c(dist-1, dist-1, dist, dist))),
                 eye.param, 'darkgrey') # floor
    if(maze[rbind(cur.pt+dirmat[dirvec$left(direction),])]==1) { # left wall
      plotProjRect(cbind(0, c(0,0,1,1), pmax(0,c(dist, dist-1, dist-1, dist))),
                   eye.param, fill=cols$left(direction))
    } else { # left open
      plotProjRect(cbind(c(-1,-1,0,0), c(0,1,1,0), dist),
                   eye.param, cols$val(direction))  # back
      plotProjRect(cbind(c(-1,0,0,-1), 1, pmax(0,c(dist-1, dist-1, dist, dist))),
                   eye.param, 'grey') # ceiling
      plotProjRect(cbind(c(-1,0,0,-1), 0, pmax(0,c(dist-1, dist-1, dist, dist))),
                   eye.param, 'darkgrey') # floor
    }
    if(maze[rbind(cur.pt+dirmat[dirvec$right(direction),])]==1) { # right wall
      plotProjRect(cbind(1, c(0,0,1,1), pmax(0,c(dist, dist-1, dist-1, dist))),
                   eye.param, fill=cols$right(direction))
    } else { # right open
      plotProjRect(cbind(c(1,1,2,2), c(0,1,1,0), dist),
                   eye.param, cols$val(direction))  # back
      plotProjRect(cbind(c(1,2,2,1), 1, pmax(0,c(dist-1, dist-1, dist, dist))),
                   eye.param, 'grey') # ceiling
      plotProjRect(cbind(c(1,2,2,1), 0, pmax(0,c(dist-1, dist-1, dist, dist))),
                   eye.param, 'darkgrey') # floor
    }
    
  }
  
}


plotmaze3d(maze, c(3,5), 1)


dev.new(width=14, height=7, noRStudioGD = TRUE)
par(mfrow=c(1,2))
eventEnv <- getGraphicsEventEnv()

tmpenv <- new.env(parent=emptyenv())

tmpenv$cur <- c(3,3)
tmpenv$dir <- 1

kbfun <- function(key) {

  dirmat <- rbind(c(0,1), c(1,0), c(0,-1), c(-1,0))
  dirvec <- circVec$new(1:4)
  
  if(key=='q') return(invisible(1))
  if(key=='Up') {
    cand <- tmpenv$cur + dirmat[tmpenv$dir,]
    if(maze[rbind(cand)]==0) {
      tmpenv$cur <- cand
    } else {
      cat("Ooops\n")
    }
  }
  if(key=='Left') {
    tmpenv$dir <- dirvec$left(tmpenv$dir)
  }
  if(key=='Right') {
    tmpenv$dir <- dirvec$right(tmpenv$dir)
  }
  plotmaze2d(maze, tmpenv$cur, tmpenv$dir, walladj=0)
  plotmaze3d(maze, tmpenv$cur, tmpenv$dir)
  NULL
}

plotmaze2d(maze, tmpenv$cur, tmpenv$dir, walladj=0)
plotmaze3d(maze, tmpenv$cur, tmpenv$dir)

setGraphicsEventHandlers(prompt='Arrow Keys',
                         onKeybd=kbfun)
getGraphicsEvent()

