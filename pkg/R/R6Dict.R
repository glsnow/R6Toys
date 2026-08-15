#' Dictionary (Hash Map)
#'
#' @description
#' An R6 class implementing a dictionary (also known as a hash map or
#' associative array). Key-value pairs are stored in an R \code{environment}
#' with \code{hash = TRUE}, giving average O(1) lookup, insertion, and
#' deletion regardless of the number of entries.
#'
#' @details
#' Keys are character strings; values may be any R object. Three calling
#' conventions are supported by \code{$add()}:
#' \itemize{
#'   \item A named \code{list} passed as the first argument.
#'   \item Explicit \code{keys} and \code{values} vectors / lists.
#'   \item Named arguments passed via \code{...} (note: values are captured
#'     unevaluated via \code{match.call()}, so this form is best suited to
#'     literal constants).
#' }
#'
#' The dictionary also provides two convenience methods for common compound
#' value types: \code{$counter()} returns an \code{R6Counter} for a key
#' (creating one if it does not yet exist), and \code{$DLL()} does the same
#' for an \code{R6DLL}. These are useful for building frequency tables or
#' grouping values under a key without manually checking for existence first.
#' These should not be mixed with each other, or other types of values.
#'
#' @examples
#' ## Basic key-value storage
#' d <- R6Dict$new()
#' d$add(list(a = 1, b = 2, c = 3))
#' d$add(d = 4, e = 5)
#' d$keys()           # c("a", "b", "c", "d", "e")
#' d$value("b")       # 2
#' d$contains("c")    # TRUE
#' d$contains("z")    # FALSE
#'
#' ## Add entries with keys= and values=
#' d$add(keys = c("x", "y"), values = c(10, 20))
#' d$toList()
#' sapply(sort(d$keys()), function(x) d$value(x))
#'
#' ## Remove an entry
#' d$remove("a")
#' d$keys()
#'
#' ## Initialise with entries directly
#' d2 <- R6Dict$new(list(alpha = "first", beta = "second"))
#' d2$value("alpha")  # "first"
#'
#' ## Counter dictionary:
#' tab <- R6Dict$new()
#' for(i in state.name) {
#'   tab$counter(substring(i, 1, 1))$pp
#' }
#' tab$toList()
#' 
#' ## DLL dictionary: group values by key
#' 
#' tab2 <- R6Dict$new()
#' for(i in state.name) {
#'   tab2$DLL(substring(i, 1, 1))$push(i)
#' }
#' tab2$toList()
#'
#' @export
R6Dict <- R6Class("R6Dict",
  public = list(

    #' @field dict An R \code{environment} (with hashing enabled) used as the
    #'   underlying storage for key-value pairs. Keys are the environment's
    #'   variable names; values are the bound objects.
    dict = NULL,

    #' @field hascounter Logical. Set to \code{TRUE} the first time
    #'   \code{$counter()} is called. When \code{TRUE}, the default
    #'   \code{$toList()} extractor unwraps each value by calling
    #'   \code{x$value}, returning a plain numeric vector of counts.
    hascounter = FALSE,

    #' @field hasDLL Logical. Set to \code{TRUE} the first time
    #'   \code{$DLL()} is called. When \code{TRUE}, the default
    #'   \code{$toList()} extractor calls \code{x$toList()} on each value,
    #'   returning a named list of vectors.
    hasDLL = FALSE,

    #' @description
    #' Create a new, empty \code{R6Dict} and optionally populate it. Any
    #' arguments accepted by \code{$add()} may be passed here.
    #' @param ... Optional initial entries, forwarded to \code{$add()}.
    #'   Typically a named list, e.g. \code{R6Dict$new(list(a=1, b=2))}.
    #' @return A new \code{R6Dict} object (invisibly).
    initialize = function(...) {
      self$dict <- new.env(parent = emptyenv())
      self$add(...)
      return(invisible(self))
    },

    #' @description
    #' Add one or more key-value pairs to the dictionary. Existing keys are
    #' silently overwritten. Three calling conventions are supported and may
    #' be combined in a single call:
    #' \itemize{
    #'   \item \code{list}: a named R list whose names become keys and whose
    #'     elements become values.
    #'   \item \code{keys} + \code{values}: parallel character vector of keys
    #'     and corresponding vector or list of values.
    #'   \item \code{...}: named arguments, e.g. \code{d$add(x=1, y=2)}.
    #' }
    #' @param list A named list of key-value pairs to add.
    #' @param ... Named arguments whose names are keys and whose values are
    #'   the corresponding dictionary values.
    #' @param keys Character vector of keys (used together with
    #'   \code{values}).
    #' @param values Vector or list of values parallel to \code{keys}.
    #' @return The \code{R6Dict} object (invisibly), enabling method chaining.
    add = function(list, ..., keys, values) {
      if (!missing(list)) {
        for (i in seq_along(list)) {
          self$dict[[names(list)[i]]] <- list[[i]]
        }
      }
      if (!missing(keys) && !missing(values)) {
        if (is.list(values)) {
          for (i in seq_along(keys)) {
            self$dict[[keys[i]]] <- values[[i]]
          }
        } else {
          for (i in seq_along(keys)) {
            self$dict[[keys[i]]] <- values[i]
          }
        }
      }
      if (...length()) {
        dots <- base::list(...)
        for (i in seq_along(dots)) {
          self$dict[[names(dots)[i]]] <- dots[[i]]
        }
      }
      return(invisible(self))
    },

    #' @description
    #' Remove one or more entries from the dictionary. Attempting to remove a
    #' key that does not exist raises an error.
    #' @param keys Character vector of keys to remove.
    #' @return The \code{R6Dict} object (invisibly), enabling method chaining.
    remove = function(keys) {
      rm(list = keys, pos = self$dict)
      return(invisible(self))
    },

    #' @description
    #' Return all keys currently stored in the dictionary. The order of keys
    #' is not guaranteed; sort the result if a specific order is needed.
    #' @return A character vector of key names.
    keys = function() {
      return(names(self$dict))
    },

    #' @description
    #' Retrieve the value associated with a single key.
    #' @param key Character scalar. The key to look up.
    #' @return The value stored under \code{key}, or \code{NULL} if the key
    #'   does not exist.
    value = function(key) {
      return(self$dict[[key]])
    },

    #' @description
    #' Test whether a key exists in the dictionary.
    #' @param key Character scalar. The key to test.
    #' @return \code{TRUE} if \code{key} is present; \code{FALSE} otherwise.
    contains = function(key) {
      return(exists(key, self$dict))
    },

    #' @description
    #' Collect all values into an R object. The default extraction function
    #' depends on how the dictionary has been used:
    #' \itemize{
    #'   \item If \code{hascounter} is \code{TRUE}, each value's
    #'     \code{$value} field is extracted (returning a numeric count vector).
    #'   \item If \code{hasDLL} is \code{TRUE}, each value's \code{$toList()}
    #'     method is called (returning a list of vectors).
    #'   \item Otherwise, values are returned as-is via \code{I}.
    #' }
    #' @param FUN Function applied to each value before collection. Overrides
    #'   the default extractor described above.
    #' @param sort Logical. If \code{TRUE} (default), entries are sorted
    #'   alphabetically by key before collection.
    #' @param simplify Logical or function. Controls the return type:
    #'   \code{TRUE} (default) uses \code{simplify2array()}; \code{FALSE}
    #'   returns a plain named \code{list}; a function is called via
    #'   \code{do.call()} on the collected list.
    #' @return A named vector, array, list, or the result of
    #'   \code{simplify()}, depending on \code{simplify}.
    toList = function(FUN, sort = TRUE, simplify = TRUE) {
      if (missing(FUN)) {
        FUN <- if (self$hascounter) {
          function(x) x$value
        } else if (self$hasDLL) {
          function(x) x$toList()
        } else {
          I
        }
      }
      out <- lapply(as.list(self$dict), FUN)
      if (sort) {
        out <- out[order(names(out))]
      }
      if (is.function(simplify)) {
        return(do.call(simplify, out))
      } else if (simplify) {
        return(simplify2array(out))
      } else {
        return(out)
      }
    },

    #' @description
    #' Return the \code{R6Counter} stored under \code{key}, creating a new
    #' one initialised to zero if the key does not yet exist. Also sets
    #' \code{self$hascounter = TRUE} so that \code{$toList()} uses the
    #' correct default extractor. Useful for building frequency tables
    #' without manually checking for key existence.
    #' @param key Character scalar. The key whose counter to retrieve or
    #'   create.
    #' @return The \code{R6Counter} object stored under \code{key}.
    counter = function(key) {
      self$hascounter <- TRUE
      if (!self$contains(key)) {
        self$add(keys = key, values = list(R6Counter$new()))
      }
      return(self$dict[[key]])
    },

    #' @description
    #' Return the \code{R6DLL} stored under \code{key}, creating a new empty
    #' doubly linked list if the key does not yet exist. Also sets
    #' \code{self$hasDLL = TRUE} so that \code{$toList()} uses the correct
    #' default extractor. Useful for grouping multiple values under a shared
    #' key without manually checking for key existence.
    #' @param key Character scalar. The key whose \code{R6DLL} to retrieve or
    #'   create.
    #' @return The \code{R6DLL} object stored under \code{key}.
    DLL = function(key) {
      self$hasDLL <- TRUE
      if (!self$contains(key)) {
        self$add(keys = key, values = list(R6DLL$new()))
      }
      return(self$dict[[key]])
    }
  )
)
