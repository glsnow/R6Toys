## still need to go through and add the warnings in many methods


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
    private$first <- private$last <- private$current <- node
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
  private$first$value
})

## peek_last ----

R6DLL$set("public", "peek_last", function() {
  private$last$value
})

## peek_current ----

R6DLL$set("public", "peek_current", function() {
  private$current$value
})

## peek_next ----

R6DLL$set("public", "peek_next", function() {
  private$current$nxt$value
})

## peek_prev ----
R6DLL$set("public", "peek_prev", function() {
  private$current$prv$value
})

## iter_reset ----

R6DLL$set("public", "iter_reset", function(last=FALSE) {
  self$found <- FALSE
  if(last) {
    private$current <- private$last
  } else {
    private$current <- private$first
  }
  return(invisible(self))
})

## iter_next ----

R6DLL$set("public", "iter_next", function() {
  self$found <- FALSE
  private$current <- private$current$nxt
  return(invisible(self))
})

## iter_prev ----

R6DLL$set("public", "iter_prev", function() {
  self$found <- FALSE
  private$current <- private$current$prv
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
                          ifelse(loc=='after', 'end', 'start'), ".")
    if(loc=='after') {
      self$push(x)
    } else {
      self$unshift(x)
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


## tests ----

dll <- R6DLL$new()
dll$push(1)$push(2)$push("c")
dll$pop()
dll$pop()
dll$pop()
dll$pop()

for(i in 1:10) dll$push(letters[i])
dll$shift()
dll$shift()
dll$pop()
dll$unshift(2)
dll$unshift(1)


dll$iter_reset()
while(!dll$finished) {
  print( dll$peek_current() )
  dll$iter_next()
}

dll$iter_reset(last=TRUE)$iter_prev()$iter_prev()$iter_prev()
dll$peek_current()
dll$peek_next()
dll$peek_prev()

dll$iter_reset()
while(is.numeric(dll$peek_current())) {
  dll$iter_next()
}
dll$peek_current()

dll$insert("Something New")
dll$insert("Something old", "before")

dll$iter_all()

dll$iter_next()$iter_next()
dll$peek_current()

while(dll$peek_current() < "g") {
  dll$iter_next()
}

dll$peek_current()

dll$remove()

dll$iter_reset()
repeat {
  print(dll$peek_current())
  if (dll$iter_next()$finished)
    break
}

dll$iter_find(function(x) grepl("[A-Z]", x))
dll$peek_current()
dll$iter_find(function(x) grepl("[A-Z]", x))
dll$peek_current()
dll$iter_find(function(x) grepl("[A-Z]", x))
dll$peek_current()

state.dll <- R6DLL$new()

# insertion sort
for(i in seq_along(state.name)) {
  state.dll$iter_reset()
  state.dll$iter_find(
    function(x) x$stats['Population'] > state.x77[i,'Population']
                      )
  state.dll$insert(
    list(name=state.name[i], abb=state.abb[i], 
         stats=state.x77[i,]),
    loc= if(is.null(state.dll$peek_current())) "after" else "before"
  )
}

state.dll$iter_all(function(x) cat(x$name, ": ", x$stats[['Population']], "\n"))
