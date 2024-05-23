library(R6)
#library(crayon)

MPuz <- R6Class("MPuz",
                public=list(
                  autoprint=FALSE,
                  autoplot=TRUE,
                  incorrect = list(),
                  correct = "",
                  original = "",
                  current = "",
                  plt.cex = 2.5,
                  generate = function(autoprint=FALSE, autoplot=!autoprint) {
                    self$autoprint <- autoprint
                    self$autoplot <- autoplot
                    self$incorrect <- setNames(rep(list(numeric(0)), 10), LETTERS[1:10])
                    self$correct <- setNames(rep("", 10), LETTERS[1:10])
                    private$ans <- setNames(sample(0:9), LETTERS[1:10])
                    a1 <- a2 <- a3 <- 0
                    while( (a1 < 1000) | (a2 <1000) | (a3 < 10000)  ) {
                      t <- sample(101:999, 1)
                      b <- sample(12:99, 1)
                      b1 <- b %/% 10
                      b2 <- b %% 10
                      a1 <- b2 * t
                      a2 <- b1 * t
                      a3 <- b*t
                    }
                    self$original <- sprintf("    %3d\nx    %2d\n-------\n   %4d\n+ %4d \n-------\n  %5d\n",
                                             t, b, a1, a2, a3)
                    for(i in 1:10) {
                      self$original <- gsub(as.character(private$ans[i]), names(private$ans)[i], self$original)
                    }
                    self$current <- self$original
                    if(self$autoprint) self$print()
                    if(self$autoplot) self$plot()
                  },
                  print = function() {
                    cat("\n", self$current, "\n", sep='')
                  },
                  plot = function() {
                    op <- par(mar=rep(0,4)+0.1)
                    on.exit(par(op))
                    plot.new()
                    text(0.75, 0.5, self$current, adj=1, cex=self$plt.cex, 
                         family='mono')
                  },
                  initialize=function(...) self$generate(...),
                  guess = function(...) {
                    dots <- list(...)
                    names(dots) <- toupper(names(dots))
                    res <- logical(0)
                    for(i in seq_along(dots)) {
                      let <- names(dots)[i]
                      nums <- dots[[i]]
                      for(num in nums) {
                        if( private$ans[let] == num) {  # correct
                          self$correct[let] <- num
                          self$current <- gsub(let, as.character(num), self$current)
                          cat('Correct: ', let, "=", num, "\n")
                          res <- c(res, TRUE)
                        } else {  # incorrect
                          self$incorrect[[let]] <- c(self$incorrect[[let]], num)
                          cat('Wrong: ', let, "is not", num, "\n")
                          res <- c(res, FALSE)
                        }
                      }
                    }
                    if(self$autoprint) self$print()
                    if(self$autoplot) self$plot()
                    if((self$autoprint | self$autoplot) & self$finished() ) {
                      cat("\nCongratulations!\n\n")
                    }
                    return(invisible(res))
                  },
                  finished = function() {
                    !grepl("[A-J]", self$current)
                  }
                ),
                private=list(
                  ans = numeric(0)
                ),
                active=list(
                  a=function(x) self$guess(A=x),
                  A=function(x) self$guess(A=x),
                  b=function(x) self$guess(B=x),
                  B=function(x) self$guess(B=x),
                  c=function(x) self$guess(C=x),
                  C=function(x) self$guess(C=x),
                  d=function(x) self$guess(D=x),
                  D=function(x) self$guess(D=x),
                  e=function(x) self$guess(E=x),
                  E=function(x) self$guess(E=x),
                  f=function(x) self$guess(F=x),
                  F=function(x) self$guess(F=x),
                  g=function(x) self$guess(G=x),
                  G=function(x) self$guess(G=x),
                  h=function(x) self$guess(H=x),
                  H=function(x) self$guess(H=x),
                  i=function(x) self$guess(I=x),
                  I=function(x) self$guess(I=x),
                  j=function(x) self$guess(J=x),
                  J=function(x) self$guess(J=x)
                )
                )
