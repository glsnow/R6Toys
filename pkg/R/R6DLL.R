#' Doubly Linked List (R6)
#'
#' @description
#' An R6 class that implements a doubly linked list (DLL). Each node stores an
#' arbitrary R value and maintains forward and backward pointers, allowing
#' efficient insertion, removal, and bidirectional traversal without copying
#' data.
#'
#' @details
#' Nodes are lightweight environments with three fields: \code{value},
#' \code{prv} (pointer to the previous node), and \code{nxt} (pointer to the
#' next node). The list tracks its head (\code{first}), tail (\code{last}),
#' and an optional iterator position (\code{current}) in private fields.
#'
#' All mutating methods return \code{invisible(self)}, enabling method chaining:
#' \preformatted{
#' dll <- R6DLL$new()
#' dll$push(1)$push(2)$push(3)
#' }
#'
#' The iterator (\code{current}) is independent of the list structure and is
#' managed via the \code{iter_*} family of methods. The active binding
#' \code{finished} signals when the iterator has walked off either end of the
#' list.
#'
#' @field length Non-negative integer giving the number of nodes currently in
#'   the list.
#' @field warn Logical flag. When \code{TRUE}, operations that cannot be
#'   completed (e.g. popping from an empty list) emit an R \code{warning()}.
#'   Defaults to \code{FALSE}.
#' @field found Logical flag set by \code{iter_find()}. \code{TRUE} when the
#'   most recent search matched the current node; \code{FALSE} otherwise.
#'
#' @section Stack and queue operations:
#' \describe{
#'   \item{\code{push(x)}}{
#'     Append value \code{x} to the tail of the list. Equivalent to an
#'     \emph{enqueue} or stack \emph{push} operation. Increments
#'     \code{self$length} and returns the object invisibly.
#'   }
#'   \item{\code{pop()}}{
#'     Remove and return the value at the tail of the list. Equivalent to a
#'     stack \emph{pop}. If the removed node was also \code{current}, the
#'     iterator is reset to \code{NULL}. Returns \code{NULL} (with an optional
#'     warning) when the list is empty.
#'   }
#'   \item{\code{unshift(x, iter_reset = FALSE)}}{
#'     Prepend value \code{x} to the head of the list. If
#'     \code{iter_reset = TRUE}, the iterator's \code{current} pointer is moved
#'     to the newly created head node. Returns the object invisibly.
#'   }
#'   \item{\code{shift()}}{
#'     Remove and return the value at the head of the list. Equivalent to a
#'     queue \emph{dequeue}. If the removed node was also \code{current}, the
#'     iterator is reset to \code{NULL}. Returns \code{NULL} (with an optional
#'     warning) when the list is empty.
#'   }
#' }
#'
#' @section Peek operations:
#' \describe{
#'   \item{\code{peek_first()}}{
#'     Return the value of the head node without modifying the list. Emits an
#'     optional warning when the list is empty.
#'   }
#'   \item{\code{peek_last()}}{
#'     Return the value of the tail node without modifying the list. Emits an
#'     optional warning when the list is empty.
#'   }
#'   \item{\code{peek_current()}}{
#'     Return the value of the current iterator node without modifying the
#'     list. Emits an optional warning when \code{current} is unset
#'     (\code{NULL}).
#'   }
#'   \item{\code{peek_next()}}{
#'     Return the value of the node immediately after \code{current}. If
#'     \code{current} is unset, returns the head node's value and optionally
#'     warns.
#'   }
#'   \item{\code{peek_prev()}}{
#'     Return the value of the node immediately before \code{current}. If
#'     \code{current} is unset, returns the tail node's value and optionally
#'     warns.
#'   }
#' }
#'
#' @section Iterator operations:
#' \describe{
#'   \item{\code{iter_reset(last = FALSE)}}{
#'     Reset the iterator and set \code{self$found} to \code{FALSE}.
#'     \itemize{
#'       \item \code{last = FALSE} (default) — positions \code{current} at the
#'         head node.
#'       \item \code{last = TRUE} — positions \code{current} at the tail node.
#'       \item \code{last = NULL} or \code{last = NA} — sets \code{current} to
#'         \code{NULL} (iterator unset).
#'     }
#'     Returns the object invisibly.
#'   }
#'   \item{\code{iter_next()}}{
#'     Advance \code{current} one step toward the tail. Sets
#'     \code{self$found} to \code{FALSE}. If \code{current} is unset, moves to
#'     the head node and optionally warns. When \code{current} reaches past the
#'     tail, it becomes \code{NULL} and \code{self$finished} returns
#'     \code{TRUE}. Returns the object invisibly.
#'   }
#'   \item{\code{iter_prev()}}{
#'     Move \code{current} one step toward the head. Sets \code{self$found} to
#'     \code{FALSE}. If \code{current} is unset, moves to the tail node and
#'     optionally warns. When \code{current} reaches past the head, it becomes
#'     \code{NULL} and \code{self$finished} returns \code{TRUE}. Returns the
#'     object invisibly.
#'   }
#'   \item{\code{iter_all(FUN = print, fromLast = FALSE)}}{
#'     Apply \code{FUN} to the value of every node in order. Set
#'     \code{fromLast = TRUE} to traverse from tail to head. The iterator's
#'     \code{current} position is saved before traversal and restored
#'     afterwards, so \code{iter_all()} does not disturb an in-progress
#'     iteration. Returns the object invisibly.
#'   }
#'   \item{\code{iter_find(FUN = function(x) TRUE, ..., fromLast = FALSE)}}{
#'     Scan forward (or backward when \code{fromLast = TRUE}) from
#'     \code{current} for the next node whose value satisfies the predicate
#'     \code{FUN(value, ...)}. Additional arguments (\code{...}) are forwarded
#'     to \code{FUN}. If \code{self$found} is already \code{TRUE} from a prior
#'     call, the search skips the current node and begins from the next (or
#'     previous) one, allowing repeated calls to iterate through all matching
#'     nodes. Sets \code{self$found = TRUE} on a successful match,
#'     \code{FALSE} if the iterator is exhausted without a match. Returns the
#'     object invisibly.
#'   }
#' }
#'
#' @section Active bindings:
#' \describe{
#'   \item{\code{finished}}{
#'     Read-only logical. Returns \code{TRUE} when \code{current} is
#'     \code{NULL}, indicating that the iterator has not been started or has
#'     walked off one end of the list. Returns \code{FALSE} while
#'     \code{current} points to a valid node. Typically used as the loop
#'     condition with \code{iter_next()} or \code{iter_prev()}.
#'   }
#' }
#'
#' @section Structural modification:
#' \describe{
#'   \item{\code{insert(x, loc = c("after", "before"))}}{
#'     Insert value \code{x} adjacent to the current node.
#'     \itemize{
#'       \item \code{loc = "after"} (default) — inserts immediately after
#'         \code{current}. If \code{current} is the tail, delegates to
#'         \code{push()}. If \code{current} is unset, inserts at the head and
#'         optionally warns.
#'       \item \code{loc = "before"} — inserts immediately before
#'         \code{current}. If \code{current} is the head, delegates to
#'         \code{unshift()}. If \code{current} is unset, appends to the tail
#'         and optionally warns.
#'     }
#'     Returns the object invisibly.
#'   }
#'   \item{\code{remove()}}{
#'     Remove the current node and return its value. After removal,
#'     \code{current} advances to the next node (or \code{NULL} when the
#'     removed node was the tail). Delegates to \code{shift()} or \code{pop()}
#'     when removing the head or tail, respectively. Returns \code{NULL} (with
#'     an optional warning) when \code{current} is unset.
#'   }
#'   \item{\code{swap(loc = c("after", "before"))}}{
#'     Swap the current node with an adjacent node in place.
#'     \itemize{
#'       \item \code{loc = "after"} (default) — swap \code{current} with the
#'         node immediately following it.
#'       \item \code{loc = "before"} — swap \code{current} with the node
#'         immediately preceding it.
#'     }
#'     The operation is a no-op (with optional warnings) when \code{current}
#'     is unset or when the requested neighbour does not exist (e.g. swapping
#'     "after" when \code{current} is the tail). Returns the object invisibly.
#'   }
#' }
#'
#' @section Conversion:
#' \describe{
#'   \item{\code{toList(FUN = I, fromLast = FALSE, simplify = TRUE)}}{
#'     Collect all node values into an R object. \code{FUN} is applied to each
#'     value before collection (default \code{I} is the identity function).
#'     Set \code{fromLast = TRUE} to collect from tail to head. The
#'     \code{simplify} argument controls the output type:
#'     \itemize{
#'       \item \code{TRUE} (default) — passes the collected list through
#'         \code{simplify2array()}.
#'       \item A function — that function is called via \code{do.call()} on
#'         the collected list.
#'       \item \code{FALSE} — returns a plain R \code{list}.
#'     }
#'     The iterator's \code{current} position is saved before traversal and
#'     restored afterwards.
#'   }
#' }
#'
#' @examples
#' dll <- R6DLL$new()
#'
#' # Build a list with push / unshift
#' dll$push(2)$push(3)$unshift(1)
#' dll$length          # 3
#' dll$peek_first()    # 1
#' dll$peek_last()     # 3
#'
#' # Forward iteration
#' dll$iter_reset()
#' while (!dll$finished) {
#'   cat(dll$peek_current(), "\n")
#'   dll$iter_next()
#' }
#'
#' # Search for a value
#' dll$iter_find(function(x) x == 2)
#' dll$found           # TRUE
#' dll$peek_current()  # 2
#'
#' # Convert to a vector
#' dll$toList()        # c(1, 2, 3)
#'
#' @export
#' @importFrom R6 R6Class
R6DLL <- R6Class("R6DLL",
                 public = list(length = 0, warn = FALSE,
                               found = FALSE),
                 private = list(first = NULL, last = NULL,
                                current = NULL),
                 cloneable = FALSE
)

## push ----

#' @rdname R6DLL
R6DLL$set("public", "push", function(x) {
  node <- new.env(hash = FALSE, parent = emptyenv())
  node$value <- x
  node$prv <- node$nxt <- NULL
  if (is.null(private$last)) {
    private$first <- private$last <- node
  } else {
    node$prv <- private$last
    private$last$nxt <- node
    private$last <- node
  }
  self$length <- self$length + 1
  return(invisible(self))
})

## pop ----

#' @rdname R6DLL
R6DLL$set("public", "pop", function() {
  if (is.null(private$last)) {
    if (self$warn) warning("DLL is empty, nothing to pop.")
    return(NULL)
  }
  node <- private$last
  if (is.null(node$prv)) {
    private$first <- private$last <- private$current <- NULL
  } else {
    if (identical(private$current, node)) {
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

#' @rdname R6DLL
R6DLL$set("public", "unshift", function(x, iter_reset = FALSE) {
  node <- new.env(hash = FALSE, parent = emptyenv())
  node$value <- x
  node$prv <- node$nxt <- NULL
  if (is.null(private$first)) {
    private$first <- private$last <- private$current <- node
  } else {
    node$nxt <- private$first
    private$first$prv <- node
    private$first <- node
  }
  if (iter_reset) {
    private$current <- node
  }
  self$length <- self$length + 1
  return(invisible(self))
})

## shift ----

#' @rdname R6DLL
R6DLL$set("public", "shift", function() {
  if (is.null(private$first)) {
    if (self$warn) warning("Empty DLL, nothing to shift.")
    return(NULL)
  }
  node <- private$first
  if (is.null(node$nxt)) {
    private$first <- private$last <- private$current <- NULL
  } else {
    if (identical(private$current, node)) {
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
  while (!is.null(private$last)) {
    self$pop()
  }
})

## peek_first ----

#' @rdname R6DLL
R6DLL$set("public", "peek_first", function() {
  if (is.null(private$first) && self$warn) warning("Empty DLL, nothing to peek.")
  private$first$value
})

## peek_last ----

#' @rdname R6DLL
R6DLL$set("public", "peek_last", function() {
  if (is.null(private$last) && self$warn) warning("Empty DLL, nothing to peek.")
  private$last$value
})

## peek_current ----

#' @rdname R6DLL
R6DLL$set("public", "peek_current", function() {
  if (is.null(private$current) && self$warn) warning("Current Node not set, nothing to peek.")
  private$current$value
})

## peek_next ----

#' @rdname R6DLL
R6DLL$set("public", "peek_next", function() {
  if (is.null(private$current)) {
    if (self$warn) warning("Current unset, peeking at first.")
    return(private$first$value)
  }
  private$current$nxt$value
})

## peek_prev ----

#' @rdname R6DLL
R6DLL$set("public", "peek_prev", function() {
  if (is.null(private$current)) {
    if (self$warn) warning("Current unset, peeking at last.")
    return(private$last$value)
  }
  private$current$prv$value
})

## iter_reset ----

#' @rdname R6DLL
R6DLL$set("public", "iter_reset", function(last = FALSE) {
  self$found <- FALSE
  if (is.null(last) || is.na(last)) {
    private$current <- NULL
  } else if (last) {
    private$current <- private$last
  } else {
    private$current <- private$first
  }
  return(invisible(self))
})

## iter_next ----

#' @rdname R6DLL
R6DLL$set("public", "iter_next", function() {
  self$found <- FALSE
  if (is.null(private$current)) {
    if (self$warn) warning('Current was unset, moving to first.')
    private$current <- private$first
  } else {
    private$current <- private$current$nxt
  }
  return(invisible(self))
})

## iter_prev ----

#' @rdname R6DLL
R6DLL$set("public", "iter_prev", function() {
  self$found <- FALSE
  if (is.null(private$current)) {
    if (self$warn) warning('Current was unset, moving to last.')
    private$current <- private$last
  } else {
    private$current <- private$current$prv
  }
  return(invisible(self))
})

## iter_all ----

#' @rdname R6DLL
R6DLL$set("public", "iter_all", function(FUN = print, fromLast = FALSE) {
  cn <- private$current
  if (fromLast) {
    self$iter_reset(last = TRUE)
    while (!self$finished) {
      FUN(private$current$value)
      self$iter_prev()
    }
  } else {
    self$iter_reset(last = FALSE)
    while (!self$finished) {
      FUN(private$current$value)
      self$iter_next()
    }
  }
  private$current <- cn
  return(invisible(self))
})

## iter_find ----

#' @rdname R6DLL
R6DLL$set("public", "iter_find", function(FUN = function(x) TRUE, ..., fromLast = FALSE) {
  if (is.null(private$current)) {
    self$iter_reset(last = fromLast)
  }
  if (self$found) {
    if (fromLast) {
      self$iter_prev()
    } else {
      self$iter_next()
    }
  }
  while (!self$finished) {
    if (FUN(private$current$value, ...)) {
      self$found <- TRUE
      break
    }
    if (fromLast) {
      self$iter_prev()
    } else {
      self$iter_next()
    }
  }
  return(invisible(self))
})

## finished ----

#' @rdname R6DLL
R6DLL$set("active", "finished", function() {
  is.null(private$current)
})

## insert ----

#' @rdname R6DLL
R6DLL$set("public", "insert", function(x, loc = c('after', 'before')) {
  loc <- match.arg(loc)
  if (is.null(private$current)) {
    if (self$warn) warning("No current node, inserting at ",
                           ifelse(loc == 'after', 'start', 'end'), ".")
    if (loc == 'after') {
      self$unshift(x)
    } else {
      self$push(x)
    }
  } else {
    if (loc == 'after') {
      if (is.null(private$current$nxt)) {
        self$push(x)
      } else {
        node <- new.env(hash = FALSE, parent = emptyenv())
        node$value <- x
        node$prv <- private$current
        node$nxt <- private$current$nxt
        private$current$nxt$prv <- node
        private$current$nxt <- node
        self$length <- self$length + 1
      }
    } else { # loc == 'before'
      if (is.null(private$current$prv)) {
        self$unshift(x)
      } else {
        node <- new.env(hash = FALSE, parent = emptyenv())
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

#' @rdname R6DLL
R6DLL$set("public", "remove", function() {
  if (is.null(private$current)) {
    if (self$warn) warning("No Current Node to remove.")
    return(NULL)
  }
  self$found <- FALSE
  if (identical(private$current, private$first)) {
    self$shift()
  } else if (identical(private$current, private$last)) {
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

#' @rdname R6DLL
R6DLL$set("public", "swap", function(loc = c('after', 'before')) {
  if (is.null(private$current)) {
    if (self$warn) warning("No current node, no swap is being made.")
    return(invisible(self))
  }
  loc <- match.arg(loc)
  if (loc == 'after') {
    if (is.null(private$current$nxt)) {
      if (self$warn) warning("No node after the current one, no swap is being made.")
      return(invisible(self))
    }
    tmpnode <- private$current$nxt
    tmpnode2 <- self$remove()
    private$current <- tmpnode
    self$insert(tmpnode2, 'after')
  } else { # before
    if (is.null(private$current$prv)) {
      if (self$warn) warning("No node before the current one, no swap is being made.")
      return(invisible(self))
    }
    tmpnode <- private$current$prv
    tmpnode2 <- self$remove()
    private$current <- tmpnode
    self$insert(tmpnode2, 'before')
  }
})

## toList ----

#' @rdname R6DLL
R6DLL$set("public", "toList", function(FUN = I, fromLast = FALSE,
                                       simplify = TRUE) {
  cn <- private$current
  out <- vector("list", self$length)
  tmp <- R6Counter$new()
  if (fromLast) {
    self$iter_reset(last = TRUE)
    while (!self$finished) {
      out[[tmp$pp]] <- FUN(private$current$value)
      self$iter_prev()
    }
  } else {
    self$iter_reset(last = FALSE)
    while (!self$finished) {
      out[[tmp$pp]] <- FUN(private$current$value)
      self$iter_next()
    }
  }
  private$current <- cn
  if (is.function(simplify)) {
    return(do.call(simplify, out))
  } else if (simplify) {
    return(simplify2array(out))
  } else {
    return(out)
  }
})


