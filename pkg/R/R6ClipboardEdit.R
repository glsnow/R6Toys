#' Clipboard Text Editor
#'
#' @description
#' An R6 class that reads text from the system clipboard, applies a
#' transformation function, and writes the result back to the clipboard. The
#' intended workflow is to copy text from any application, call \code{$do()}
#' (or simply print the object), then paste the transformed result back into
#' the target application.
#'
#' @details
#' A transformation function is supplied at construction time via the
#' \code{FUN} argument, which may be:
#' \itemize{
#'   \item A plain R \code{function} that accepts a character vector of lines
#'     and returns a transformed character vector.
#'   \item A \strong{string} naming one of the built-in transformations (see
#'     the Built-in transformations section below).
#'   \item An \strong{integer} selecting a built-in transformation by position
#'     (1 = \code{"slash"}, 2 = \code{"copycode"}, 3 =
#'     \code{"copycodecommentoutput"}, 4 = \code{"tabletab"}, 5 =
#'     \code{"comment"}, 6 = \code{"wrapcomment"}, 7 = \code{"uncomment"},
#'     8 = \code{"dupinc"}).
#' }
#'
#' Default arguments for \code{FUN} can be stored in \code{self$args} and are
#' merged with any arguments passed directly to \code{$do(...)}; per-call
#' arguments take precedence.
#'
#' Each call to \code{$do()} increments an internal \code{R6Counter} stored
#' in \code{self$counter}, which is also used by the \code{"dupinc"}
#' built-in to produce incrementing numeric substitutions.
#'
#' @section Built-in transformations:
#' \describe{
#'   \item{\code{"slash"} (alias: none)}{Replace every Windows-style
#'     backslash (\code{\\}) with a forward slash (\code{/}). Useful for
#'     converting file paths copied from Windows Explorer.}
#'   \item{\code{"copycode"} (aliases: \code{"cc"}, \code{"cco"})}{Strip R
#'     console prompts (\code{> } and \code{+ }) from clipboard text that was
#'     copied from an R session, leaving only the code (any output is removed).
#'     Continuation-prompt
#'     lines are re-indented by \code{ns} spaces (default 2).}
#'   \item{\code{"copycodecommentoutput"} (aliases: \code{"ccc"},
#'     \code{"ccco"})}{Like \code{"copycode"}, but keeps output lines and
#'     prefixes them with \code{np} hash characters and \code{ncs} spaces
#'     (defaults: 2 and 2). Useful for preparing reproducible examples.}
#'   \item{\code{"tabletab"} (alias: \code{"tt"})}{Replace the common column
#'     separator positions in a space-aligned table with tab characters, making
#'     it easy to paste into a spreadsheet. \code{sep} (default \code{" "})
#'     is the input separator; \code{newsep} (default \code{"\\t"}) is the
#'     output separator.}
#'   \item{\code{"comment"}}{Prefix every line with \code{np} hash characters
#'     and \code{ncs} spaces (defaults: 2 and 2).}
#'   \item{\code{"wrapcomment"} (aliases: \code{"wc"}, \code{"cw"})}{Strip
#'     leading \code{#} and space characters from each line, join all lines
#'     into a single paragraph, and re-wrap as a comment block at \code{width}
#'     characters (default 60) with \code{np} hashes and \code{ncs} spaces
#'     per line (defaults: 1 and 2).}
#'   \item{\code{"uncomment"} (alias: \code{"uc"})}{Remove leading hash
#'     characters and up to \code{ns} spaces (default 2) from every line.}
#'   \item{\code{"dupinc"} (alias: \code{"di"})}{Replace every occurrence of
#'     the pattern \code{pat} (default \code{"<n>"}) in each line with the
#'     current value of the internal counter. Useful for duplicating a
#'     template line with an auto-incrementing number each time \code{$do()}
#'     is called.}
#' }
#'
#' @examples
#' \dontrun{
#' ## Example 1: Convert backslashes to forward slashes
#' ## In Windows Explorer, copy a folder path to the clipboard,
#' ## e.g. C:\Users\name\Documents\data
#' slash_fix <- R6ClipboardEdit$new("slash")
#' slash_fix$do()
#' ## Now paste — the clipboard holds C:/Users/name/Documents/data
#'
#'
#' ## Example 2: Extract just the code from an R console session
#' ## In your terminal or R console, select and copy a block of output
#' ## that contains both prompts (>) and results, e.g.:
#' ##   > x <- 1:5
#' ##   > mean(x)
#' ##   [1] 3
#' cc <- R6ClipboardEdit$new("copycode")
#' cc$do()
#' ## Now paste — the clipboard holds only:
#' ##   x <- 1:5
#' ##   mean(x)
#'
#'
#' ## Example 3: Code + commented output
#' ## Copy the same R console block as above.
#' ccc <- R6ClipboardEdit$new("copycodecommentoutput")
#' ccc$do()
#' ## Now paste — the clipboard holds:
#' ##   x <- 1:5
#' ##   mean(x)
#' ##   ## [1] 3
#'
#'
#' ## Example 4: Convert a space-aligned table to tab-separated
#' ## Copy a space-aligned table from R output or a text file, e.g.:
#' ##   Name   Score  Grade
#' ##   Alice  95     A
#' ##   Bob    82     B
#' tt <- R6ClipboardEdit$new("tabletab")
#' tt$do()
#' ## Now paste into a spreadsheet application — columns are tab-separated.
#'
#'
#' ## Example 5: Comment out a block of code
#' ## Copy several lines of R code to the clipboard.
#' cmnt <- R6ClipboardEdit$new("comment")
#' cmnt$do()
#' ## Now paste — every line is prefixed with "##  ".
#'
#'
#' ## Example 6: Re-wrap a comment block
#' ## Copy a roxygen or #-prefixed comment block that needs reformatting.
#' wc <- R6ClipboardEdit$new("wrapcomment")
#' wc$do()
#' ## Now paste — the text is re-wrapped as a comment at 60 characters.
#'
#'
#' ## Example 7: Remove comment markers
#' ## Copy some commented-out lines to the clipboard.
#' uc <- R6ClipboardEdit$new("uncomment")
#' uc$do()
#' ## Now paste — the leading "# " prefix has been stripped from each line.
#'
#'
#' ## Example 8: Duplicate a template line with an incrementing counter
#' ## Copy a template line containing the placeholder <n> to the clipboard,
#' ## e.g.:  item_<n> <- list()
#' di <- R6ClipboardEdit$new("dupinc")
#' di$do()  # counter = 1, clipboard holds: item_1 <- list()
#' ## Paste into your script, copy the template again (or use $undo()), then:
#' di$do()  # counter = 2, clipboard holds: item_2 <- list()
#'
#'
#' ## Example 9: Using a custom transformation function
#' ## Copy some text to the clipboard, e.g. a mixed-case word list.
#' upper <- R6ClipboardEdit$new(toupper)
#' upper$do()
#' ## Now paste — every line has been converted to upper case.
#'
#'
#' ## Example 10: Trigger the transformation by printing the object
#' ## Copy some text to the clipboard.
#' slash_fix <- R6ClipboardEdit$new("slash")
#' slash_fix   # printing the object calls $do() automatically
#' ## Now paste the transformed result.
#'
#'
#' ## Example 11: Undo the last transformation
#' ## If $do() produced an unwanted result, restore the previous clipboard:
#' slash_fix$undo()
#' ## Now paste or do another transformation — the original text is 
#' ## back on the clipboard.
#' }
#'
#' @export
R6ClipboardEdit <- R6Class("R6ClipboardEdit",
  public = list(

    #' @field prev Character vector. The clipboard contents from before the
    #'   most recent \code{$do()} call. Used by \code{$undo()} to restore the
    #'   previous clipboard state.
    prev = character(0),

    #' @field FUN Function or \code{NULL}. The transformation applied to the
    #'   clipboard text by \code{$do()}. Set at construction time via the
    #'   \code{FUN} argument to \code{$new()}.
    FUN = NULL,

    #' @field args Named list. Default arguments forwarded to \code{FUN} on
    #'   every \code{$do()} call. Per-call arguments passed to \code{$do(...)}
    #'   take precedence over values in \code{args}.
    args = list(),

    #' @field counter An \code{R6Counter} object. Incremented on every
    #'   \code{$do()} call. Also used by the \code{"dupinc"} built-in to
    #'   supply the substitution value for the \code{<n>} pattern.
    counter = NULL,

    #' @field echo Logical. If \code{TRUE}, the transformed text is also
    #'   printed to the R console by \code{$do()}. Default \code{FALSE}.
    echo = FALSE,

    #' @description
    #' Create a new \code{R6ClipboardEdit} object.
    #' @param FUN The transformation to apply. May be:
    #'   \itemize{
    #'     \item A \code{function} accepting a character vector of lines and
    #'       returning a character vector.
    #'     \item A character string naming a built-in transformation (see the
    #'       Built-in transformations section). Short aliases are accepted.
    #'     \item An integer selecting a built-in by position (1--8).
    #'     \item Omitted, in which case \code{self$FUN} remains \code{NULL}
    #'       and must be set before calling \code{$do()}.
    #'   }
    initialize = function(FUN) {
      self$counter <- R6Counter$new()
      if (missing(FUN)) return()
      if (is.function(FUN)) {
        self$FUN <- FUN
        return()
      }
      if (is.numeric(FUN)) {
        FUN <- c(
          "slash", "copycode",
          "copycodecommentoutput",
          "tabletab", "comment",
          "wrapcomment", "uncomment",
          "dupinc"
        )[FUN]
      }
      if (is.character(FUN)) {
        FUN <- tolower(FUN)
        self$FUN <- switch(FUN,
          slash = function(x) {
            gsub("\\\\", "/", x)
          },
          cc =,
          cco =,
          copycode = function(x, ns = 2) {
            prmpt <- getOption("prompt")
            cnt   <- getOption("continue")
            w <- (substr(x, 1, nchar(prmpt)) == prmpt) |
              (substr(x, 1, nchar(cnt)) == cnt)
            x <- x[w]
            x <- sub(prmpt, "", x, fixed = TRUE)
            x <- sub(cnt, strrep(" ", ns), x, fixed = TRUE)
            x
          },
          ccco =,
          ccc =,
          copycodecommentoutput = function(x, ns = 2, np = 2, ncs = 2) {
            prmpt <- getOption("prompt")
            cnt   <- getOption("continue")
            w <- (substr(x, 1, nchar(prmpt)) == prmpt) |
              (substr(x, 1, nchar(cnt)) == cnt)
            x[w]  <- sub(prmpt, "", x[w], fixed = TRUE)
            x[w]  <- sub(cnt, strrep(" ", ns), x[w], fixed = TRUE)
            x[!w] <- paste0(strrep("#", np), strrep(" ", ncs), x[!w])
            x
          },
          tt =,
          tabletab = function(x, sep = " ", newsep = "\t") {
            cols  <- gregexpr(sep, x)
            cols2 <- Reduce(intersect, cols)
            cols2 <- cols2[c(diff(cols2) > 1, TRUE)]
            for (i in cols2) {
              substr(x, i, i) <- newsep
            }
            gsub(paste0(sep, "*", newsep, sep, "*"), newsep, x)
          },
          comment = function(x, np = 2, ncs = 2) {
            paste0(strrep("#", np), strrep(" ", ncs), x)
          },
          wc =,
          cw =,
          wrapcomment = function(x, np = 1, ncs = 2, width = 60) {
            x <- sub("^[# ]+", "", x)
            x <- paste(x, collapse = "\n")
            strwrap(x, width = width,
                    prefix = paste0(strrep("#", np), strrep(" ", ncs)))
          },
          uc =,
          uncomment = function(x, ns = 2) {
            re <- paste0("^#+ {0,", ns, "}")
            sub(re, "", x)
          },
          di =,
          dupinc = function(x, pat = "<n>") {
            val <- self$counter$value
            gsub(pat, val, x)
          },
          NULL
        )
      }
    }, # end initialize

    #' @description
    #' Read the clipboard, apply \code{FUN}, and write the result back to the
    #' clipboard. The original clipboard contents are saved in \code{self$prev}
    #' so they can be restored with \code{$undo()}. The internal counter is
    #' incremented on every call. Any named arguments passed via \code{...}
    #' are merged with \code{self$args} and forwarded to \code{FUN}.
    #' @param ... Named arguments forwarded to \code{FUN}, overriding any
    #'   matching defaults stored in \code{self$args}.
    #' @return \code{invisible(NULL)}.
    do = function(...) {
      self$counter$pp
      dots <- list(...)
      x    <- readLines("clipboard", warn = FALSE)
      self$prev <- x
      args <- list(x)
      for (i in seq_along(self$args)) {
        args[[names(self$args)[i]]] <- self$args[[i]]
      }
      for (i in seq_along(dots)) {
        args[[names(dots)[i]]] <- dots[[i]]
      }
      x2 <- do.call(self$FUN, args)
      if (self$echo) {
        cat(x2, "\n\n", sep = "\n")
      }
      writeLines(x2, "clipboard")
      invisible()
    },

    #' @description
    #' Convenience method that calls \code{$do()}. Allows the transformation
    #' to be triggered simply by typing the object name at the R console.
    #' @return \code{invisible(NULL)}.
    print = function() {
      self$do()
      invisible()
    },

    #' @description
    #' Restore the clipboard to its contents from before the most recent
    #' \code{$do()} call.
    #' @return \code{invisible(NULL)}.
    undo = function() {
      writeLines(self$prev, "clipboard")
      invisible()
    }
  ) # end public
)
