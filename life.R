library(R6)

## Counter ----

Counter <- R6Class('Counter',
                   public = list(
                     val = 0,
                     print = function() {
                       print(self$val)
                     },
                     initialize = function(value=0) {
                       self$val <- value
                     },
                     Inc = function(n=1) {
                       self$val <- self$val + n
                       invisible(self)
                     },
                     Dec = function(n=1) {
                       self$val <- self$val - n
                       invisible(self)
                     }
                   ),
                   active=list(
                     pp = function(n=1) {
                       tmp <- self$val
                       self$Inc(n)
                       tmp
                     },
                     ppp = function(n=1) {
                       self$Inc(n)
                       self$val
                     },
                     mm = function(n=1) {
                       tmp <- self$val
                       self$Dec(n)
                       tmp
                     },
                     pmm = function(n=1) {
                       self$Dec(n)
                       self$val
                     },
                     mmm = function(n=1) {
                       self$Dec(n)
                       self$val
                     }
                   )
                   )

a <- Counter$new()
a$Inc()
a
a$Inc()$Inc()$Inc()
a
a$Dec(2)
a
a$pp
a$pp <- 5
a
a$ppp <- 5
a

a <- Counter$new(5)

while(a$mm) {
  print(a)
}

a$val <- 5

while(a$pmm) {
  print(a)
}

a$val <- 5

while(a$val) {
  print(a$mm)
}

a$val <- 5
while(a$val) {
  print(a$mmm)
}

## Life ----

Life <- R6Class(
  "Life", 
  public = list(
    cur = NULL,
    prev = NULL,
    xlim = NA,
    ylim = NA,
    lifecol = 'black',
    deadcol = 'white',
    bordercol = 'grey',
    boundary = 'expand',
    autoplot = TRUE,
    pause = 0.1,
    initialize = function(xlim=c(1,25), ylim=c(1,25),
                          boundary='expand') {
      self$xlim <- xlim
      self$ylim <- ylim
      self$cur <- matrix(ncol=2, nrow=0)
      self$boundary=boundary
    },
    applyBound = function() {
      if(self$boundary == 'expand') {
        self$xlim <- range(self$xlim, self$cur[,1])
        self$ylim <- range(self$ylim, self$cur[,2])
      } else if(self$boundary %in% c('torus', 'wrap')) {
        tmp.x <- self$xlim[1]
        tmp.y <- self$ylim[1]
        tmp <- self$cur
        tmp[,1] <- (tmp[,1] - tmp.x) %% 
          (diff(self$xlim)+1) + tmp.x
        tmp[,2] <- (tmp[,2] - tmp.y) %%
          (diff(self$ylim)+1) + tmp.y
        self$cur <- tmp
      } else if(self$boundary %in% c('flat', 'cut')) {
        w <- self$xlim[1] <= self$cur[,1] & self$cur[,1] <= self$xlim[2] &
          self$ylim[1] <= self$cur[,2] & self$cur[,2] <= self$ylim[2]
        self$cur <- self$cur[w,]
      }
      invisible(self)
    },
    add = function(xy) {
      w <- self$cur[,1] == xy[1] & self$cur[,2] == xy[2]
      if(!any(w)) {
        self$cur <- rbind(self$cur, xy)
      }
      if(self$autoplot) {
        self$plot()
      }
      invisible(self)
    },
    remove = function(xy) {
      w <- self$cur[,1] == xy[1] & self$cur[,2] == xy[2]
      if(any(w)) {
        self$cur <- self$cur[!w,,drop=FALSE]
      }
      if(self$autoplot) {
        self$plot()
      }
      invisible(self)
    },
    toggle = function(xy) {
      w <- self$cur[,1] == xy[1] & self$cur[,2] == xy[2]
      if(any(w)) {
        self$cur <- self$cur[!w,,drop=FALSE]
      } else {
        self$cur <- rbind(self$cur, xy)
      }
      if(self$autoplot) {
        self$plot()
      }
      invisible(self)
    },
    plot = function() {
      op <- par(mar=c(0,0,0,0)+0.1, pty='s')
      on.exit(par(op))
      plot.new()
      plot.window(xlim=self$xlim + c(-0.5, 0.5),
                  ylim=self$ylim + c(-0.5, 0.5),
                  asp=1)
      clip(self$xlim[1]-0.51, self$xlim[2]+0.51,
           self$ylim[1]-0.51, self$ylim[2]+0.51)
      rect(self$xlim[1]-0.5, self$ylim[1]-0.5,
           self$xlim[2]+0.5, self$ylim[2]+0.5,
           col=self$deadcol, border=NA)
      for(i in seq_len(nrow(self$cur))) {
        rect(self$cur[i,1]-0.5, self$cur[i,2]-0.5,
             self$cur[i,1]+0.5, self$cur[i,2]+0.5,
             col=self$lifecol, border=NA)
      }
      abline(h=seq(self$ylim[1]-0.5, self$ylim[2]+0.5),
             col=self$bordercol)
      abline(v=seq(self$xlim[1]-0.5, self$xlim[2]+0.5),
             col=self$bordercol)
      invisible(self)
    },
    plotchange = function(birthcol='green', deathcol='red') {
      if(is.null(self$prev)) return(invisible(self))
      op <- par(mar=c(0,0,0,0)+0.1, pty='s')
      on.exit(par(op))
      plot.new()
      plot.window(xlim=self$xlim + c(-0.5, 0.5),
                  ylim=self$ylim + c(-0.5, 0.5),
                  asp=1)
      clip(self$xlim[1]-0.51, self$xlim[2]+0.51,
           self$ylim[1]-0.51, self$ylim[2]+0.51)
      rect(self$xlim[1]-0.5, self$ylim[1]-0.5,
           self$xlim[2]+0.5, self$ylim[2]+0.5,
           col=self$deadcol, border=NA)
      for(i in seq_len(nrow(self$cur))) {
        if(any(self$prev[,1]==self$cur[i,1] & self$prev[,2]==self$cur[i,2])) {
          rect(self$cur[i,1]-0.5, self$cur[i,2]-0.5,
               self$cur[i,1]+0.5, self$cur[i,2]+0.5,
               col=self$lifecol, border=NA)
        } else {
          rect(self$cur[i,1]-0.5, self$cur[i,2]-0.5,
               self$cur[i,1]+0.5, self$cur[i,2]+0.5,
               col=birthcol, border=NA)
        }
      }
      for(i in seq_len(nrow(self$prev))) {
        if(!any(self$prev[i,1]==self$cur[,1] & self$prev[i,2]==self$cur[i,2])) {
          rect(self$prev[i,1]-0.5, self$prev[i,2]-0.5,
               self$prev[i,1]+0.5, self$prev[i,2]+0.5,
               col=deathcol, border=NA)
        }
      }
      abline(h=seq(self$ylim[1]-0.5, self$ylim[2]+0.5),
             col=self$bordercol)
      abline(v=seq(self$xlim[1]-0.5, self$xlim[2]+0.5),
             col=self$bordercol)
      invisible(self)
    },
    tick = function(n=1, plot=self$autoplot) {
      nbr <- expand.grid(-1:1, -1:1)
      ii <- Counter$new(n)
      while(ii$mm) {
        self$prev <- self$cur
        tmp <- self$cur
        if(nrow(tmp) < 1) break
        
        if(self$boundary %in% c('torus', 'wrap')) {
          tmp.x <- tmp.y <- -1

          tmp[,1] <- tmp[,1] - tmp.x
          tmp[,2] <- tmp[,2] - tmp.y
          
          tmp.mat <- tmp.mat2 <- matrix(0, ncol=self$ylim[2]+2, nrow=self$xlim[2]+2)
        } else {
          tmp.x <- min(tmp[,1])-2
          tmp.y <- min(tmp[,2])-2
          
          tmp[,1] <- tmp[,1] - tmp.x
          tmp[,2] <- tmp[,2] - tmp.y
          tmp.mat <- tmp.mat2 <- matrix(0, ncol=max(tmp[,2])+1, nrow=max(tmp[,1])+1)
        }
        
        for(i in seq_len(nrow(tmp))) {
          tmp2 <- cbind(nbr[,1]+tmp[i,1], nbr[,2]+tmp[i,2])
          tmp.mat[tmp2] <- tmp.mat[tmp2] + 1
        }
        if(self$boundary %in% c('torus', 'wrap')) {
          tmp.mat[2,] <- tmp.mat[2,] + tmp.mat[nrow(tmp.mat),]
          tmp.mat[1,] <- tmp.mat[1,] + tmp.mat[nrow(tmp.mat)-1,]
          tmp.mat[,2] <- tmp.mat[,2] + tmp.mat[,ncol(tmp.mat)]
          tmp.mat[,1] <- tmp.mat[,1] + tmp.mat[,ncol(tmp.mat)-1]
          tmp.mat[nrow(tmp.mat)+c(-1,0),] <- 0
          tmp.mat[,ncol(tmp.mat)+c(-1,0)] <- 0
        }
        tmp.mat2[tmp] <- 1
        if(self$boundary %in% c('torus', 'wrap')) {
          tmp.mat2[1,] <- tmp.mat2[nrow(tmp.mat2)-1,]
          tmp.mat2[,1] <- tmp.mat2[,nrow(tmp.mat2)-1]
        }
        tmp.mat2[tmp.mat==3] <- 1
        tmp.mat2[tmp.mat < 3 | tmp.mat > 4] <- 0
        self$cur <- which(tmp.mat2 == 1, arr.ind=TRUE)
        self$cur[,1] <- self$cur[,1] + tmp.x
        self$cur[,2] <- self$cur[,2] + tmp.y
        self$applyBound()
        if(plot) {
          self$plot()
          if(ii$val){
            Sys.sleep(self$pause)
          }
        }
      }
      invisible(self)
    }
  )
)


l1 <- Life$new()
l1$add(c(25,5))$add(c(25,6))$add(c(25,7))$add(c(24,7))$add(c(23,6))
l1$toggle(c(5,5))
l1$remove(c(5,5))$add(c(5,8))

l1$prev <- l1$cur

l1$plot()
while(length(xy <- locator(1))){
  l1$toggle(c(round(xy$x), round(xy$y)))
}

l1$plotchange()

l1$tick()

l1$boundary <- 'wrap'
l1$tick(1)
