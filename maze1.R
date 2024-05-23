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

m.pts <- PointStack$new()
d.pts <- PointStack$new()

nr <- 2*15+3
nc <- 2*10+3

maze <- matrix(1, nrow=nr, ncol=nc)
maze[1,] <- maze[nr,] <- 0
maze[,1] <- maze[,nc] <- 0

#cur <- c(nr-2, 3)
tmp1 <- seq(3, nr-2, by=2)
tmp2 <- seq(3, nc-2, by=2)
start <- c( sample(tmp1, 1), sample(tmp2,1))
maze[rbind(start)] <- 0

m.pts$push(start)

gcols <- rev(grey.colors(3))

tmp.x <- seq(0.5, nr+0.5) + c(-0.3, 0.3)
tmp.x[1] <- tmp.x[2] - 0.5
tmp.x[nr+1] <- tmp.x[nr] + 0.5

tmp.y <- seq(0.5, nc+0.5) + c(-0.3, 0.3)
tmp.y[1] <- tmp.y[2] - 0.5
tmp.y[nc+1] <- tmp.y[nc] + 0.5

dist <- 1
mdist <- 0
stop <- c(0,0)

while(m.pts$length) {

  cur <- m.pts$pop()
  dist <- dist - 1
  
  image(tmp.x, tmp.y, maze, xlab='', ylab='', xaxt='n', yaxt='n', asp=1, bty='n',
        col = gcols)
  points(cur[1], cur[2], pch=16, col='blue', cex=2)

  Sys.sleep(0.1)
  
  
  for(i in sample(4)) {
    d.pts$push(dirmat[i,1], dirmat[i,2])
  }
  

  while(d.pts$length) {
    d <- d.pts$pop()
    if(maze[rbind(cur+d)]){
      maze[rbind(cur+d/2, cur+d)] <- c(0,0)
      m.pts$push(cur)
      cur <- cur+d
      m.pts$push(cur)
      dist <- dist + 2
      if(dist > mdist) {
        mdist <- dist
        stop <- cur
      }
      d.pts$length <- 0
      break
    }
  }
}

points(stop[1], stop[2], pch=16, col='red', cex=2)

