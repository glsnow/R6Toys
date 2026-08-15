R6Associate <- R6Class("R6Associate",
          public = list(
            blank = NULL, # copy without labels for use with tkplot
            layout = matrix(ncol=2, nrow=0), # current layout
            cur = NULL, 
            plot_param = list(vertex.size=10, vertex.color='lightblue',
                              vertex.frame.color=NA, vertex.shape='rectangle'),
            autoplot = TRUE,
            autoprint = FALSE,
            zoom = TRUE,
            initialize = function(graph, layout, autoplot=TRUE, 
                                  autoprint=FALSE,
                                  zoom=TRUE, blank_symbol="-") {
              self$autoplot <- autoplot
              self$autoprint <- autoprint
              self$zoom <- zoom
              private$blank_symbol <- blank_symbol # chane if '-' is in some words
              if(class(graph) != 'igraph') {
                graph <- make_graph(graph, directed=FALSE)
              }
              private$ans <- graph
              self$blank <- graph
              V(self$blank)$name <- ""
              private$partial <- gsub("[a-zA-Z]", private$blank_symbol, V(private$ans)$name)
              private$partial[1] <- V(private$ans)$name[1]
              if(missing(layout)) {
                self$layout <- layout_nicely(private$ans)
              } else {
                self$layout <- layout
              }
              private$status <- rep(0, length(V(private$ans)))
              private$status[1] <- 2
              tmp <- unique(unlist(adjacent_vertices(private$ans, 1)))
              private$status[tmp] <- 1
              self$cur <- subgraph(private$ans, c(1, tmp))
              V(self$cur)$name <- private$partial[c(1, tmp)]
              if(self$autoplot) self$plot()
              if(self$autoprint) self$print()
              private$ng <- R6Counter$new()
              return(invisible(self))
            },
            print = function() {
              print(self$clues)
            },
            plot = function() {
              w <- which(private$status > 0)
              tmp.p <- self$plot_param
              tmp.p$vertex.size <- tmp.p$vertex.size * nchar(V(self$cur)$name)
              tmp.p$x <- self$cur
              tmp.p$layout <- self$layout[w,]
              if(!self$zoom) {
                tmp.p$xlim <- range(self$layout[,1])
                tmp.p$ylim <- range(self$layout[,2])
              }
              do.call(plot.igraph, tmp.p)
              return(invisible(self))
            },
            guess = function(guess) {
              private$ng$pp
              guess.re <- paste0(
                "^", 
                paste(strsplit(guess, '')[[1]], collapse="("),
                paste(rep(")?", nchar(guess)-1), collapse='')
              )
              w <- which(private$status == 1)
              p1 <- V(private$ans)$name[w]
              p2 <- private$partial[w]
              
              # match all
              tmp <- regexpr(guess.re, p1, ignore.case=TRUE)
              w1 <- which(tmp == 1)
              substr(private$partial[w[w1]], 
                     1, 
                     attr(tmp, 'match.length')[w1] ) <-
                substr(p1[w1], 1, attr(tmp, 'match.length')[w1])
              
              # match remaining
              tmp1 <- regexpr(private$blank_symbol, p2)
              p3 <- substr(p1, tmp1, NULL)
              tmp2 <- regexpr(guess.re, p3, ignore.case=TRUE)
              w2 <- which(tmp2 == 1)
              substr(private$partial[w[w2]], 
                     1, 
                     tmp1[w2] + attr(tmp2, 'match.length')[w2] - 1) <-
                substr(p1[w2], 1, tmp1[w2] + attr(tmp2, 'match.length')[w2] - 1)
              
              # update 
              private$status[!grepl(private$blank_symbol, private$partial)] <- 2
              w3 <- unique(unlist(
                adjacent_vertices(private$ans, which(private$status==2))
              ))
              w4 <- setdiff(w3, which(private$status==2))
              private$status[w4] <- 1
              
              w3 <- sort(union(w3, which(private$status==2)))
              tmp <- subgraph(private$ans, w3)
              V(tmp)$name <- private$partial[w3]
              self$cur <- tmp
              if(self$finished) cat("Congratulations!\n")
              if(self$autoplot) {
                self$plot()
              }
              if(self$autoprint) {
                self$print()
              }
            }
          ),
          private = list(
            ans = NULL,
            partial = character(0),
            status = numeric(0),
            ng = NULL,
            blank_symbol = "-" # used for unguessed letters
          ),
          active = list(
            Score = function() {
              private$ng$value
            },
            clues = function() {
              tmp <- adjacent_vertices(self$cur, grep(private$blank_symbol, V(self$cur)$name))
              sapply(tmp, function(x) attr(x, 'names'), simplify = FALSE)
            },
            finished = function() {
              all(private$status == 2)
            },
            g = function(x) {
              self$guess(x)
            }
          )
                       )

## Example ----

# create graph from see also section of help pages starting with 'lm'

lm1 <- c(
  "lm", "coef",
  "lm", "effects", 
  "lm", "residuals",
  "lm", "fitted",
  "lm", "vcov",
  "coef", "fitted",
  "coef", "residuals",
  "coef", "glm",
  "glm", "fitted",
  "glm", "residuals",
  "glm", "esoph",
  "glm", "infert",
  "residuals", "resid",
  "fitted", "fitted.values"
) |>
  make_graph(directed=FALSE)

a <- R6Associate$new(lm1)
a$layout <- layout_nicely(lm1)
a$plot()

## don't run
tkid <- tkplot(a$blank, layout=a$layout)
# adjust positions in tk window
a$layout <- tk_coords(tkid)
tk_off()
## end don't run

a$guess("r")
a$guess("co")
a$guess("e")
a$guess("residuals")

