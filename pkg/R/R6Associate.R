#' Association Game Object
#'
#' @description
#' An R6 class implementing a word-association guessing game
#' inspired by iAssociate.  The game is represented as an
#' \code{igraph} graph whose vertex names are the words to
#' be guessed.  Starting from one revealed word, players
#' incrementally uncover adjacent words by typing guesses;
#' each correct guess reveals matching letters and unlocks
#' the neighbours of any fully-solved words.
#'
#' @details
#' Construct the game with \code{R6Associate$new()}, passing
#' an \code{igraph} object (undirected) or an edge-list
#' vector suitable for \code{igraph::make_graph()}.  The
#' first vertex in the graph is revealed automatically at
#' start-up.
#'
#' Guesses are matched using a fuzzy prefix regex so that
#' typing \code{"co"} will reveal the letters \code{c} and
#' \code{o} wherever they occur at the start of an active
#' word or at the start of the unguessed section, and 
#' typing a full word reveals it entirely.
#'
#' The current subgraph is stored in \code{$cur} and
#' re-plotted automatically after each guess when
#' \code{autoplot = TRUE}.
#'
#' @section Public fields:
#' \describe{
#'   \item{\code{blank}}{A copy of the graph with all vertex
#'     labels removed; useful with \code{igraph::tkplot()}
#'     for manual layout adjustment.}
#'   \item{\code{layout}}{Numeric matrix (n x 2) holding the
#'     current vertex layout coordinates.}
#'   \item{\code{cur}}{The \code{igraph} subgraph currently
#'     visible to the player (revealed and adjacent
#'     vertices).}
#'   \item{\code{plot_param}}{Named list of graphical
#'     parameters forwarded to \code{igraph::plot.igraph()}.}
#'   \item{\code{autoplot}}{Logical. If \code{TRUE}
#'     (default), the graph is re-plotted after each guess.}
#'   \item{\code{autoprint}}{Logical. If \code{TRUE},
#'     clues are printed to the console after each guess.}
#'   \item{\code{zoom}}{Logical. If \code{TRUE} (default),
#'     the plot axes zoom to the current subgraph; if
#'     \code{FALSE} the full layout extent is preserved.}
#' }
#'
#' @section Active bindings:
#' \describe{
#'   \item{\code{Score}}{Integer. Number of guesses made so
#'     far (lower is better).}
#'   \item{\code{clues}}{Named list. For each still-hidden
#'     adjacent word, the names of its already-revealed
#'     neighbours serve as clues.}
#'   \item{\code{finished}}{Logical. \code{TRUE} when all
#'     words have been fully revealed.}
#'   \item{\code{g}}{Shorthand active binding that accepts
#'     a character string and calls \code{$guess()}.
#'     Use as \code{a$g <- "word"}.}
#' }
#'
#' @examples
#' \dontrun{
#' library(igraph)
#'
#' # Build a graph from the "See Also" links of help pages
#' # starting with 'lm'
#' lm1 <- c(
#'   "lm", "coef",
#'   "lm", "effects",
#'   "lm", "residuals",
#'   "lm", "fitted",
#'   "lm", "vcov",
#'   "coef", "fitted",
#'   "coef", "residuals",
#'   "coef", "glm",
#'   "glm", "fitted",
#'   "glm", "residuals",
#'   "glm", "esoph",
#'   "glm", "infert",
#'   "residuals", "resid",
#'   "fitted", "fitted.values"
#' ) |>
#'   make_graph(directed = FALSE)
#'
#' a <- R6Associate$new(lm1)
#' a$layout <- layout_nicely(lm1)
#' a$plot()
#'
#' ## Optionally adjust layout interactively:
#' ## tkid <- tkplot(a$blank, layout = a$layout)
#' ## a$layout <- tk_coords(tkid)
#' ## tk_off()
#'
#' a$guess("r")
#' a$guess("co")
#' a$guess("e")
#' a$guess("residuals")
#' }
#'
#' @importFrom igraph make_graph V layout_nicely adjacent_vertices subgraph plot.igraph
#' @export
R6Associate <- R6Class("R6Associate",
          public = list(

            #' @field blank An \code{igraph} object identical to the answer
            #'   graph but with all vertex labels removed. Useful with
            #'   \code{igraph::tkplot()} for interactive layout adjustment
            #'   without revealing the answers.
            blank = NULL, # copy without labels for use with tkplot

            #' @field layout Numeric matrix with two columns. Each row holds
            #'   the \code{(x, y)} coordinates used when plotting the
            #'   corresponding vertex. Updated by assigning the result of
            #'   \code{igraph::tk_coords()} after interactive layout editing.
            layout = matrix(ncol=2, nrow=0), # current layout

            #' @field cur An \code{igraph} subgraph containing only the
            #'   vertices that are currently visible to the player (fully
            #'   solved vertices and their immediate neighbours). Vertex
            #'   names reflect partial progress, with unguessed letters
            #'   replaced by the \code{blank_symbol}.
            cur = NULL,

            #' @field plot_param Named list of graphical parameters forwarded
            #'   to \code{igraph::plot.igraph()} when \code{$plot()} is
            #'   called. Defaults: \code{vertex.size = 10},
            #'   \code{vertex.color = "lightblue"},
            #'   \code{vertex.frame.color = NA},
            #'   \code{vertex.shape = "rectangle"}.
            plot_param = list(vertex.size=10, vertex.color='lightblue',
                              vertex.frame.color=NA, vertex.shape='rectangle'),

            #' @field autoplot Logical. If \code{TRUE} (default), the game
            #'   graph is re-plotted automatically after each call to
            #'   \code{$guess()}.
            autoplot = TRUE,

            #' @field autoprint Logical. If \code{TRUE}, \code{$print()} is
            #'   called automatically after each guess to display the current
            #'   clues in the console. Default \code{FALSE}.
            autoprint = FALSE,

            #' @field zoom Logical. If \code{TRUE} (default), plot axes are
            #'   scaled to the current visible subgraph. If \code{FALSE}, the
            #'   full layout extent is preserved so nodes do not appear to
            #'   jump as the graph grows.
            zoom = TRUE,

            #' @description
            #' Create a new \code{R6Associate} game object.
            #' @param graph An \code{igraph} object or an edge-list
            #'   character vector passed to
            #'   \code{igraph::make_graph(graph, directed = FALSE)}.
            #' @param layout Optional numeric matrix (n x 2) of vertex
            #'   coordinates.  If omitted,
            #'   \code{igraph::layout_nicely()} is used.
            #' @param autoplot Logical. Re-plot after each guess?
            #'   Default \code{TRUE}.
            #' @param autoprint Logical. Print clues after each guess?
            #'   Default \code{FALSE}.
            #' @param zoom Logical. Zoom plot to current subgraph?
            #'   Default \code{TRUE}.
            #' @param blank_symbol Character. Placeholder for unguessed
            #'   letters.  Change from the default \code{"-"} if that
            #'   character appears in your word list.
            #' @return The \code{R6Associate} object (invisibly).
            initialize = function(graph, layout, autoplot=TRUE, 
                                  autoprint=FALSE,
                                  zoom=TRUE, blank_symbol="-") {
              self$autoplot <- autoplot
              self$autoprint <- autoprint
              self$zoom <- zoom
              private$blank_symbol <- blank_symbol # chane if '-' is in some words
              if(!inherits(graph, 'igraph')) {
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

            #' @description
            #' Print the current clues to the console.
            print = function() {
              print(self$clues)
            },

            #' @description
            #' Plot the current state of the game graph.
            #' @return The \code{R6Associate} object (invisibly).
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

            #' @description
            #' Submit a guess.  The guess is matched (case-insensitively)
            #' as a fuzzy prefix against all currently active words;
            #' matching letters are revealed.  Fully solved words unlock
            #' their neighbours.
            #' @param guess Character string to guess.
            #' @return The \code{R6Associate} object (invisibly).
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
              return(invisible(self))
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

            #' @field Score Integer. Total number of guesses submitted.
            Score = function() {
              private$ng$value
            },

            #' @field clues Named list of neighbour names for each
            #'   partially-revealed (active) vertex.
            clues = function() {
              tmp <- adjacent_vertices(self$cur, grep(private$blank_symbol, V(self$cur)$name))
              sapply(tmp, function(x) attr(x, 'names'), simplify = FALSE)
            },

            #' @field finished Logical. \code{TRUE} when all words are
            #'   fully revealed.
            finished = function() {
              all(private$status == 2)
            },

            #' @field g Shorthand active binding for \code{$guess()}.
            #'   Assign a character string: \code{a$g <- "word"}.
            g = function(x) {
              self$guess(x)
            }
          )
)
