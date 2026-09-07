R6History <- R6Class(
  public = list(
    initialize = function() {
      private$tcb_name = paste0(
        sample(letters, 10, replace=TRUE),
        collapse=''
      )
      return(invisible(self))
    },
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
    start = function() {
      addTaskCallback(self$save, name=private$tcb_name)
      return(invisible(self))
    },
    stop = function() {
      removeTaskCallback(private$tcb_name)
    },
    skip = function(expr) {
      private$record <- FALSE
      eval(expr)
    },
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
    visible = function(w=self$length, fromLast=FALSE) {
      w <- self$check_w(w, fromLast)
      sapply(private$hst[w], `[[`, 'visible')
    },
    expr = function(w=self$length, fromLast=FALSE,
                    clipboard=FALSE, deparse=TRUE) {
      w <- self$check_w(w, fromLast)
      tmp <- sapply(private$hst[w], `[[`, 'expr')
      if(deparse || clipboard) {
        tmp <- sapply(tmp, base::deparse)
      }
      if(clipboard) {
        writeLines(tmp, "clipboard")
        return(invisible(tmp))
      } else {
        return(tmp)
      }
    }, 
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
    length = function() {
      return(private$lngth)
    },
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
{2+4
  3+5
  }
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
