#' Command History Recorder
#'
#' @description
#' An R6 class that records the expressions, values, and visibility
#' of top-level commands evaluated at the console, using a task
#' callback (see \code{\link{addTaskCallback}}). Useful for
#' capturing a session's history of expressions and results for
#' later inspection, reuse, or copying to the clipboard.
#'
#' @details
#' Call \code{$start()} to begin recording; each subsequent
#' top-level expression evaluated at the console is appended to the
#' history. Call \code{$stop()} to remove the task callback and
#' halt recording. Use \code{$skip()} to evaluate an expression
#' without it being recorded while recording is active.
#'
#' The first \code{save()} call after \code{$start()} corresponds to
#' the \code{start()} call itself and is discarded, so only
#' subsequently typed expressions are stored.
#'
#' @examples
#' 
#' h1 <- R6History$new()
#' h1$start()
#' 1+2
#' 3+4
#' 5*6
#' h1$skip(7+8)
#' 9*9
#' tmp <- 10+10
#' 3-2; 4-2
#' h1$stop()
#' out <- h1$all
#' 
#' h1$visible()
#' h1$visible(TRUE)
#' h1$expr()
#' h1$expr(NULL)
#' h1$expr(numeric(0))
#' h1$expr(1:3)
#' h1$expr(1:3, TRUE)
#' h1$expr(3:1, TRUE)
#' h1$value(TRUE)
#' h1$value(1:3, addNames=TRUE)
#' 
#' tmp <- h1$expr(1, deparse=FALSE)
#' tmp[[2]] <- 3
#' eval(tmp)
#'
#' @export
R6History <- R6Class("R6History",
  public = list(

    #' @description
    #' Create a new \code{R6History} object.
    initialize = function() {
      private$tcb_name = paste0(
        sample(letters, 10, replace=TRUE),
        collapse=''
      )
      return(invisible(self))
    },

    #' @description
    #' Task-callback handler that appends an evaluated top-level
    #' expression to the history. Called automatically by
    #' \code{\link{addTaskCallback}} once recording has started;
    #' not normally called directly.
    #' @param expr The expression that was evaluated.
    #' @param value The value the expression produced.
    #' @param ok Logical; whether the expression completed without
    #'   error. Currently unused.
    #' @param visible Logical; whether the expression's value would
    #'   be auto-printed.
    #' @return \code{TRUE}, as required by
    #'   \code{\link{addTaskCallback}} to keep the callback active.
    save = function(expr, value, ok, visible) {
      if(private$record) {
        private$lngth <- private$lngth + 1
        if(private$lngth > length(private$hst)) {
          length(private$hst) <- 2*length(private$hst)
        }
        private$hst[[private$lngth]] <- 
          list(expr=expr,
               value=value, visible=visible)
      } else {
        private$record <- TRUE
      }
      return(TRUE)
    },

    #' @description
    #' Begin recording by registering a task callback that saves
    #' each subsequently evaluated top-level expression.
    #' @return The \code{R6History} object (invisibly), enabling
    #'   method chaining.
    start = function() {
      addTaskCallback(self$save, name=private$tcb_name)
      return(invisible(self))
    },

    #' @description
    #' Stop recording by removing the task callback registered by
    #' \code{start()}.
    stop = function() {
      removeTaskCallback(private$tcb_name)
    },

    #' @description
    #' Evaluate an expression without recording it, even while
    #' recording is active.
    #' @param expr An expression to evaluate without recording.
    #' @return The value of evaluating \code{expr}.
    skip = function(expr) {
      private$record <- FALSE
      eval(expr)
    },

    #' @description
    #' Normalize an index argument into a vector of history
    #' positions. Used internally by \code{expr}, \code{value}, and
    #' \code{visible}.
    #' @param w \code{NULL}, a zero-length vector, or \code{TRUE}
    #'   selects all entries; a numeric vector selects entries by
    #'   position (see \code{fromLast}).
    #' @param fromLast Logical; if \code{TRUE}, numeric positions in
    #'   \code{w} are counted from the most recent entry backwards.
    #' @return An integer vector of history positions.
    check_w = function(w, fromLast=FALSE) {
      if(is.null(w) || (length(w) == 0) ||
          ((length(w) == 1) && is.logical(w) && w)
      ) {
        w <- seq_len(self$length)
      }
      if(fromLast && is.numeric(w)) {
        w <- self$length + 1 - w
      }
      w
    },

    #' @description
    #' Return the visibility flag(s) for history entries.
    #' @param w \code{NULL}, a zero-length vector, or \code{TRUE}
    #'   selects all entries; a numeric vector selects entries by
    #'   position (see \code{fromLast}). Defaults to the most
    #'   recent entry.
    #' @param fromLast Logical; if \code{TRUE}, numeric positions in
    #'   \code{w} are counted from the most recent entry backwards.
    #' @return A logical vector of visibility flags for entries
    #'   \code{w}.
    visible = function(w=self$length, fromLast=FALSE) {
      w <- self$check_w(w, fromLast)
      sapply(private$hst[w], `[[`, 'visible')
    },

    #' @description
    #' Return the expression(s) for history entries, optionally
    #' deparsed to character and/or written to the clipboard.
    #' @param w \code{NULL}, a zero-length vector, or \code{TRUE}
    #'   selects all entries; a numeric vector selects entries by
    #'   position (see \code{fromLast}). Defaults to the most
    #'   recent entry.
    #' @param fromLast Logical; if \code{TRUE}, numeric positions in
    #'   \code{w} are counted from the most recent entry backwards.
    #' @param clipboard Logical; if \code{TRUE}, write the deparsed
    #'   expression(s) to the clipboard instead of returning them.
    #' @param deparse Logical; if \code{TRUE}, return expressions as
    #'   deparsed character strings rather than language objects.
    #' @return The selected expression(s), as language object(s) or,
    #'   if \code{deparse} or \code{clipboard} is \code{TRUE}, as
    #'   character strings. If \code{clipboard} is \code{TRUE}, the
    #'   deparsed expressions are returned invisibly.
    expr = function(w=self$length, fromLast=FALSE,
                    clipboard=FALSE, deparse=TRUE) {
      w <- self$check_w(w, fromLast)
      tmp <- sapply(private$hst[w], `[[`, 'expr')
      if(deparse || clipboard) {
        tmp <- sapply(tmp, base::deparse)
      }
      if(clipboard) {
        if(is.list(tmp)) {
          tmp <- unlist(tmp)
        }
        writeLines(tmp, "clipboard")
        return(invisible(tmp))
      } else {
        if(is.list(tmp) && (length(tmp)==1)) {
          tmp <- tmp[[1]]
        }
        return(tmp)
      }
    }, 

    #' @description
    #' Return the value(s) for history entries, optionally named by
    #' their deparsed expressions.
    #' @param w \code{NULL}, a zero-length vector, or \code{TRUE}
    #'   selects all entries; a numeric vector selects entries by
    #'   position (see \code{fromLast}). Defaults to the most
    #'   recent entry.
    #' @param fromLast Logical; if \code{TRUE}, numeric positions in
    #'   \code{w} are counted from the most recent entry backwards.
    #' @param simplify Logical; passed to \code{\link{sapply}} when
    #'   collecting values.
    #' @param addNames Logical; if \code{TRUE}, name the returned
    #'   values by their corresponding deparsed expressions.
    #' @return The selected value(s), simplified according to
    #'   \code{simplify} and optionally named according to
    #'   \code{addNames}.
    value = function(w=self$length, fromLast=FALSE,
                     simplify=TRUE, addNames=FALSE) {
      w <- self$check_w(w, fromLast)
      tmp <- sapply(private$hst[w], `[[`, 'value',
                    simplify=simplify)
      if(addNames) {
        tmp <- setNames(tmp, self$expr(w))
      }
      return(tmp)
    }
  ),
  active = list(

    #' @field length The number of recorded history entries.
    length = function() {
      return(private$lngth)
    },

    #' @field all The full list of recorded history entries.
    all = function() {
      return(private$hst[seq_len(private$lngth)])
    }
  ),
  private = list(
    hst = vector("list", 10),
    lngth = 0,
    record = FALSE,
    tcb_name = ""
  )
)



## examples ----

h1 <- R6History$new()
h1$start()
1+2
3+4
5*6
h1$skip(7+8)
9*9
tmp <- 10+10
3-2; 4-2
h1$stop()
out <- h1$all

h1$visible()
h1$visible(TRUE)
h1$expr()
h1$expr(NULL)
h1$expr(numeric(0))
h1$expr(1:3)
h1$expr(1:3, TRUE)
h1$expr(3:1, TRUE)
h1$value(TRUE)
h1$value(1:3, addNames=TRUE)

tmp <- h1$expr(1, deparse=FALSE)
tmp[[2]] <- 3
eval(tmp)

