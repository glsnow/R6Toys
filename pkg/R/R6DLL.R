
R6DLL <- R6Class("R6DLL", 
                 public = list(length=0, warn=FALSE,
                               found=FALSE),
                 private = list(first=NULL, last=NULL,
                                current=NULL),
                 cloneable = FALSE
)

## push ----

R6DLL$set("public", "push", function(x) {
  node <- new.env(hash=FALSE, parent=emptyenv())
  node$value <- x
  node$prv <- node$nxt <- NULL
  if(is.null(private$last)) {
    private$first <- private$last <- node
  } else {
    node$prv <- private$last
    private$last$nxt <- node
    private$last <- node
  }
  self$length <- self$length + 1
  return(invisible(self))
}
)

## pop ----

R6DLL$set("public", "pop", function() {
  if(is.null(private$last)) {
    if(self$warn) warning("DLL is empty, nothing to pop.")
    return(NULL)
  }
  node <- private$last
  if(is.null(node$prv)) {
    private$first <- private$last <- private$current <- NULL
  } else {
    if(identical(private$current, node)) {
      private$current <- NULL
    }
    private$last <- node$prv
    private$last$nxt <- NULL
  }
  node$prv <- node$nxt <- NULL
  self$length <- self$length - 1
  return(node$value)
})

## unshift ----

R6DLL$set("public", "unshift", function(x, iter_reset=FALSE) {
  node <- new.env(hash=FALSE, parent=emptyenv())
  node$value <- x
  node$prv <- node$nxt <- NULL
  if(is.null(private$first)) {
    private$first <- private$last <- private$current <- node
  } else {
    node$nxt <- private$first
    private$first$prv <- node
    private$first <- node
  }
  if(iter_reset) {
    private$current <- node
  }
  self$length <- self$length + 1
  return(invisible(self))
}
)

## shift ----

R6DLL$set("public", "shift", function() {
  if(is.null(private$first)) {
    if(self$warn) warning("Empty DLL, nothing to shift.")
    return(NULL)
  }
  node <- private$first
  if(is.null(node$nxt)) {
    private$first <- private$last <- private$current <- NULL
  } else {
    if(identical(private$current, node)) {
      private$current <- NULL
    }
    private$first <- node$nxt
    private$first$prv <- NULL
  }
  node$prv <- node$nxt <- NULL
  self$length <- self$length - 1
  return(node$value)
})

## finalize ----

R6DLL$set("private", "finalize", function() {
  while(!is.null(private$last)) {
    self$pop()
  }
})

## peek_first ----

R6DLL$set("public", "peek_first", function() {
  if(is.null(private$first) && self$warn) warning("Empty DLL, nothing to peek.")
  private$first$value
})

## peek_last ----

R6DLL$set("public", "peek_last", function() {
  if(is.null(private$last) && self$warn) warning("Empty DLL, nothing to peek.")
  private$last$value
})

## peek_current ----

R6DLL$set("public", "peek_current", function() {
  if(is.null(private$current) && self$warn) warning("Current Node not set, nothing to peek.")
  private$current$value
})

## peek_next ----

R6DLL$set("public", "peek_next", function() {
  if(is.null(private$current)) {
    if(self$warn) warning("Current unset, peeking at first.")
    return(private$first$value)
  }
  private$current$nxt$value
})

## peek_prev ----
R6DLL$set("public", "peek_prev", function() {
  if(is.null(private$current)) {
    if(self$warn) warning("Current unset, peeking at last.")
    return(private$last$value)
  }
  private$current$prv$value
})

## iter_reset ----

R6DLL$set("public", "iter_reset", function(last=FALSE) {
  self$found <- FALSE
  if(is.null(last) || is.na(last)) {
    private$current <- NULL
  } else if(last) {
    private$current <- private$last
  } else {
    private$current <- private$first
  }
  return(invisible(self))
})

## iter_next ----

R6DLL$set("public", "iter_next", function() {
  self$found <- FALSE
  if(is.null(private$current)) {
    if(self$warn) warning('Current was unset, moving to first.')
    private$current <- private$first
  } else {
    private$current <- private$current$nxt
  }
  return(invisible(self))
})

## iter_prev ----

R6DLL$set("public", "iter_prev", function() {
  self$found <- FALSE
  if(is.null(private$current)) {
    if(self$warn) warning('Current was unset, moving to last.')
    private$current <- private$last
  } else {
    private$current <- private$current$prv
  }
  return(invisible(self))
})

## iter_all ----

R6DLL$set("public", "iter_all", function(FUN=print, fromLast=FALSE) {
  cn <- private$current
  if(fromLast) {
    self$iter_reset(last=TRUE)
    while(!self$finished) {
      FUN(private$current$value)
      self$iter_prev()
    }
  } else {
    self$iter_reset(last=FALSE)
    while(!self$finished) {
      FUN(private$current$value)
      self$iter_next()
    }
  }
  private$current <- cn
  return(invisible(self))
})

## iter_find ----

R6DLL$set("public", "iter_find", function(FUN=function(x) TRUE, ..., fromLast=FALSE) {
  if(is.null(private$current)) {
    self$iter_reset(last=fromLast)
  }
  if(self$found) {
    if(fromLast) {
      self$iter_prev()
    } else {
      self$iter_next()
    }
  }
  while(!self$finished) {
    if(FUN(private$current$value, ...)) {
      self$found <- TRUE
      break
    }
    if(fromLast) {
      self$iter_prev()
    } else {
      self$iter_next()
    }
  }
  return(invisible(self))
})

## finished ----

R6DLL$set("active", "finished", function() {
  is.null(private$current)
})

## insert ----

R6DLL$set("public", "insert", function(x, loc=c('after','before')) {
  loc <- match.arg(loc)
  if(is.null(private$current)) {
    if(self$warn) warning("No current node, inserting at ", 
                          ifelse(loc=='after', 'start', 'end'), ".")
    if(loc=='after') {
      self$unshift(x)
    } else {
      self$push(x)
    }
  } else {
    if(loc=='after') {
      if(is.null(private$current$nxt)) {
        self$push(x)
      } else {
        node <- new.env(hash=FALSE, parent=emptyenv())
        node$value <- x
        node$prv <- private$current
        node$nxt <- private$current$nxt
        private$current$nxt$prv <- node
        private$current$nxt <- node
        self$length <- self$length + 1
      }
    } else { # loc == 'before'
      if(is.null(private$current$prv)) {
        self$unshift(x)
      } else {
        node <- new.env(hash=FALSE, parent=emptyenv())
        node$value <- x
        node$prv <- private$current$prv
        node$nxt <- private$current
        private$current$prv$nxt <- node
        private$current$prv <- node
        self$length <- self$length + 1
      }
    }
  }
  return(invisible(self))
})

## remove ----

R6DLL$set("public", "remove", function() {
  if(is.null(private$current)) {
    if(self$warn) warning("No Current Node to remove.")
    return(NULL)
  }
  self$found <- FALSE
  if(identical(private$current, private$first)) {
    self$shift()
  } else if(identical(private$current, private$last)) {
    self$pop()
  } else {
    node <- private$current
    node$prv$nxt <- node$nxt
    node$nxt$prv <- node$prv
    private$current <- node$nxt
    node$prv <- node$nxt <- NULL
    self$length <- self$length - 1
    return(node$value)
  }
})

## swap ----

R6DLL$set("public", "swap", function(loc=c('after','before')) {
  if(is.null(private$current)){
    if(self$warn) warning("No current node, no swap is being made.")
    return(invisible(self))
  }
  loc <- match.arg(loc)
  if(loc=='after') {
    if(is.null(private$current$nxt)) {
      if(self$warn) warning("No node after the current one, no swap is being made.")
      return(invisible(self))
    }
    tmpnode <- private$current$nxt
    tmpnode2 <- self$remove()
    private$current <- tmpnode
    self$insert(tmpnode2, 'after')
  } else { # before
    if(is.null(private$current$prv)) {
      if(self$warn) warning("No node before the current one, no swap is being made.")
      return(invisible(self))
    }
    tmpnode <- private$current$prv
    tmpnode2 <- self$remove()
    private$current <- tmpnode
    self$insert(tmpnode2, 'before')
  }
})

## toList ----

R6DLL$set("public", "toList", function(FUN=I, fromLast=FALSE,
                                       simplify=TRUE) {
  cn <- private$current
  out <- vector("list", self$length)
  tmp <- R6Counter$new()
  if(fromLast) {
    self$iter_reset(last=TRUE)
    while(!self$finished) {
      out[[tmp$pp]] <- FUN(private$current$value)
      self$iter_prev()
    }
  } else {
    self$iter_reset(last=FALSE)
    while(!self$finished) {
      out[[tmp$pp]] <- FUN(private$current$value)
      self$iter_next()
    }
  }
  private$current <- cn
  if(is.function(simplify)) {
    return(do.call(simplify, out))
  } else if(simplify) {
    return(simplify2array(out))
  } else {
    return(out)
  }
})
