library(R6)

Robots <- R6Class('Robots', 
                  public = list(
                    nrow=20,
                    ncol=50,
                    player=matrix(ncol=2, nrow=1),
                    corpse=matrix(NA, ncol=2, nrow=1),
                    robots=matrix(ncol=2, nrow=0),
                    piles=matrix(ncol=2, nrow=0),
                    initialize= function(nrobots=10) {
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
                    robot.pch=3,
                    player.pch=16,
                    pile.pch=8,
                    robot.char='+',
                    player.char='O',
                    pile.char='*',
                    corpse.char='X',
                    auto.plot=TRUE,
                    auto.print=FALSE,
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
                    safe.teleport = TRUE,
                    move.mat = matrix(c('NW', 'W', 'SW',
                                         'N', '.', 'S',
                                         'NE', 'E', 'SE'), 3, 3),
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

r <- Robots$new()
r$plot()
r$print()

## interactive graphics ----

dev.new()
r <- Robots$new(30)
getGraphicsEvent(
  prompt='Ready',
  onKeybd=function(A){
    switch(A,
           q=r$move('nw'),
           w=r$move('n'),
           e=r$move('ne'),
           a=r$move('w'),
           s=r$move('.'),
           d=r$move('e'),
           z=r$move('sw'),
           x=r$move('s'),
           c=r$move('se'),
           t=r$move('t'),
           ' '=r$move('.'))
  }
)

dev.new(width=6, height=3, noRStudioGD = TRUE)
r <- Robots$new(30)
getGraphicsEvent(
  prompt='Ready',
  onKeybd=function(A){
    switch(A,
           "'"=r$move('nw'),
           ","=r$move('n'),
           "."=r$move('ne'),
           a=r$move('w'),
           o=r$move('.'),
           e=r$move('e'),
           ";"=r$move('sw'),
           q=r$move('s'),
           j=r$move('se'),
           t=r$move('t'),
           y=r$move('t'),
           ' '=r$move('.'))
  }
)

