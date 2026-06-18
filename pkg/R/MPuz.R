library(R6)

#' Multiplication Puzzle Game
#'
#' @description
#' An R6 class that generates and manages a multiplication
#' cryptarithmetic puzzle.  Each digit (0--9) in a standard
#' long-multiplication layout is replaced by a unique letter (A--J).
#' Players guess the digit that each letter represents until the
#' puzzle is completely solved.
#'
#' @details
#' A new puzzle is created with \code{MPuz$new()}.  The puzzle
#' displays a multiplication of a 3-digit number by a 2-digit number,
#' together with both partial products and the final product.  Every
#' distinct digit is encoded as a distinct letter so that no
#' information about the actual values is revealed at the start.
#'
#' Players submit guesses via \code{$guess()} (or the letter active
#' bindings) until \code{$finished()} returns \code{TRUE}.
#'
#' @section Active bindings:
#' Letters \code{A}--\code{J} (both upper- and lower-case) are
#' available as active bindings.  Assigning a digit value to a
#' binding is shorthand for calling \code{$guess()}.  For example:
#'
#' \preformatted{puz <- MPuz$new()
#' puz$A <- 3   # equivalent to puz$guess(A = 3)
#' puz$b <- 7   # equivalent to puz$guess(B = 7)}
#'
#' @examples
#' puz <- MPuz$new()
#'
#' # View current state as text
#' puz$print()
#'
#' # Submit guesses
#' puz$guess(A = 5, B = 3)
#'
#' # Shorthand via active bindings
#' puz$C <- 7
#'
#' # Check completion
#' puz$finished()
#'
#' # Review score
#' puz$score()
#'
#' # Start a fresh puzzle in console mode
#' puz$generate(autoprint = TRUE, autoplot = FALSE)
#'
#' @export
MPuz <- R6Class("MPuz",
  public = list(

    #' @field autoprint Logical. If \code{TRUE}, the puzzle state is
    #'   printed to the console automatically after \code{$generate()}
    #'   or \code{$guess()}.
    autoprint = FALSE,

    #' @field autoplot Logical. If \code{TRUE}, the puzzle state is
    #'   rendered as a plot automatically after \code{$generate()} or
    #'   \code{$guess()}.
    autoplot = TRUE,

    #' @field incorrect Named list (letters A--J). Each element is a
    #'   numeric vector of digit values guessed incorrectly for that
    #'   letter.
    incorrect = list(),

    #' @field correct Named character vector (letters A--J). Each
    #'   element is the digit string for a correctly solved letter, or
    #'   \code{""} for unsolved letters.
    correct = "",

    #' @field original Character string. The puzzle layout as first
    #'   generated, with every digit replaced by its assigned letter.
    original = "",

    #' @field current Character string. The current puzzle layout;
    #'   correctly guessed letters are replaced back with their digits.
    current = "",

    #' @field plt.cex Numeric. Character expansion factor used when
    #'   plotting the puzzle. Default \code{2.5}.
    plt.cex = 2.5,

    #' @description
    #' Generate a new multiplication puzzle.  Randomly selects a
    #' 3-digit multiplicand and a 2-digit multiplier such that both
    #' partial products are at least 4 digits and the final product is
    #' at least 5 digits.  Digits 0--9 are mapped one-to-one to
    #' letters A--J.  Resets \code{correct}, \code{incorrect},
    #' \code{original}, and \code{current}.
    #' @param autoprint Logical. Overrides the \code{autoprint} field
    #'   for this call and all subsequent calls. Default \code{FALSE}.
    #' @param autoplot Logical. Overrides the \code{autoplot} field
    #'   for this call and all subsequent calls.
    #'   Default \code{!autoprint}.
    generate = function(autoprint = FALSE, autoplot = !autoprint) {
      self$autoprint <- autoprint
      self$autoplot  <- autoplot
      self$incorrect <- setNames(rep(list(numeric(0)), 10), LETTERS[1:10])
      self$correct   <- setNames(rep("", 10), LETTERS[1:10])
      private$ans    <- setNames(sample(0:9), LETTERS[1:10])
      a1 <- a2 <- a3 <- 0
      while ((a1 < 1000) | (a2 < 1000) | (a3 < 10000)) {
        t  <- sample(101:999, 1)
        b  <- sample(12:99, 1)
        b1 <- b %/% 10
        b2 <- b %% 10
        a1 <- b2 * t
        a2 <- b1 * t
        a3 <- b * t
      }
      self$original <- sprintf(
        "    %3d\nx    %2d\n-------\n   %4d\n+ %4d \n-------\n  %5d\n",
        t, b, a1, a2, a3
      )
      for (i in 1:10) {
        self$original <- gsub(
          as.character(private$ans[i]),
          names(private$ans)[i],
          self$original
        )
      }
      self$current <- self$original
      if (self$autoprint) self$print()
      if (self$autoplot)  self$plot()
    },

    #' @description
    #' Print the current puzzle state to the console.  Solved letters
    #' appear as their corresponding digit; unsolved positions still
    #' show the letter.
    print = function() {
      cat("\n", self$current, "\n", sep = "")
    },

    #' @description
    #' Render the current puzzle state as a monospace text graphic in
    #' the active graphics device.  Plot margins are suppressed; text
    #' size is controlled by \code{plt.cex}.
    plot = function() {
      op <- par(mar = rep(0, 4) + 0.1)
      on.exit(par(op))
      plot.new()
      text(0.75, 0.5, self$current, adj = 1, cex = self$plt.cex,
           family = "mono")
    },

    #' @description
    #' Create a new \code{MPuz} object and generate an initial puzzle.
    #' Arguments are forwarded to \code{$generate()}.
    #' @param ... Arguments passed to \code{$generate()}
    #'   (\code{autoprint}, \code{autoplot}).
    initialize = function(...) self$generate(...),

    #' @description
    #' Submit one or more letter-digit guesses.  Named arguments map
    #' letter names to digit values (0--9).  Multiple letters and/or a
    #' vector of candidate digits per letter may be supplied in a
    #' single call.  Correct guesses update \code{correct} and
    #' \code{current}; incorrect guesses are appended to
    #' \code{incorrect}.  A message is printed for each guess.  If
    #' \code{autoprint} or \code{autoplot} is \code{TRUE} the updated
    #' puzzle is displayed; a congratulatory message is shown when
    #' \code{$finished()} becomes \code{TRUE}.
    #' @param ... Named arguments of the form \code{LETTER = digit}.
    #'   Letter names are coerced to upper-case.  A single argument
    #'   may be a vector of candidate digits to try in sequence.
    #' @return A logical vector (invisibly) indicating which guesses
    #'   were correct (\code{TRUE}) or incorrect (\code{FALSE}).
    guess = function(...) {
      dots        <- list(...)
      names(dots) <- toupper(names(dots))
      res         <- logical(0)
      for (i in seq_along(dots)) {
        let  <- names(dots)[i]
        nums <- dots[[i]]
        for (num in nums) {
          if (private$ans[let] == num) {
            self$correct[let] <- num
            self$current      <- gsub(let, as.character(num), self$current)
            cat("Correct: ", let, "=", num, "\n")
            res <- c(res, TRUE)
          } else {
            self$incorrect[[let]] <- c(self$incorrect[[let]], num)
            cat("Wrong: ", let, "is not", num, "\n")
            res <- c(res, FALSE)
          }
        }
      }
      if (self$autoprint) self$print()
      if (self$autoplot)  self$plot()
      if ((self$autoprint | self$autoplot) & self$finished()) {
        cat("\nCongratulations!\n\n")
      }
      return(invisible(res))
    },

    #' @description
    #' Test whether all letters in the puzzle have been solved.
    #' @return \code{TRUE} if no letters remain in \code{current};
    #'   \code{FALSE} otherwise.
    finished = function() {
      !grepl("[A-J]", self$current)
    },

    #' @description
    #' Report the number of correct and incorrect guesses made so far.
    #' Prints a one-line summary to the console.
    #' @return A named integer vector
    #'   \code{c(correct = ..., incorrect = ...)} (invisibly).
    score = function() {
      correct   <- sum(grepl("[0-9]", self$correct))
      incorrect <- sum(sapply(self$incorrect, length))
      cat("Score: ", correct, "correct, ", incorrect, "incorrect\n")
      return(invisible(c(correct = correct, incorrect = incorrect)))
    }
  ),
  private = list(
    # Named integer vector mapping each letter (A--J) to its true
    # digit value. Not accessible outside the object.
    ans = numeric(0)
  ),
  active = list(
    #' @field a Shorthand active binding for letter A (lower-case).
    a = function(x) self$guess(A = x),
    #' @field A Shorthand active binding for letter A (upper-case).
    A = function(x) self$guess(A = x),
    #' @field b Shorthand active binding for letter B (lower-case).
    b = function(x) self$guess(B = x),
    #' @field B Shorthand active binding for letter B (upper-case).
    B = function(x) self$guess(B = x),
    #' @field c Shorthand active binding for letter C (lower-case).
    c = function(x) self$guess(C = x),
    #' @field C Shorthand active binding for letter C (upper-case).
    C = function(x) self$guess(C = x),
    #' @field d Shorthand active binding for letter D (lower-case).
    d = function(x) self$guess(D = x),
    #' @field D Shorthand active binding for letter D (upper-case).
    D = function(x) self$guess(D = x),
    #' @field e Shorthand active binding for letter E (lower-case).
    e = function(x) self$guess(E = x),
    #' @field E Shorthand active binding for letter E (upper-case).
    E = function(x) self$guess(E = x),
    #' @field f Shorthand active binding for letter F (lower-case).
    f = function(x) self$guess(F = x),
    #' @field F Shorthand active binding for letter F (upper-case).
    `F` = function(x) self$guess(F = x),
    #' @field g Shorthand active binding for letter G (lower-case).
    g = function(x) self$guess(G = x),
    #' @field G Shorthand active binding for letter G (upper-case).
    G = function(x) self$guess(G = x),
    #' @field h Shorthand active binding for letter H (lower-case).
    h = function(x) self$guess(H = x),
    #' @field H Shorthand active binding for letter H (upper-case).
    H = function(x) self$guess(H = x),
    #' @field i Shorthand active binding for letter I (lower-case).
    i = function(x) self$guess(I = x),
    #' @field I Shorthand active binding for letter I (upper-case).
    I = function(x) self$guess(I = x),
    #' @field j Shorthand active binding for letter J (lower-case).
    j = function(x) self$guess(J = x),
    #' @field J Shorthand active binding for letter J (upper-case).
    J = function(x) self$guess(J = x)
  )
)
