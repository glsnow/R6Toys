#' Console Transcript Recorder
#'
#' @description
#' An R6 class that records a transcript of a console session —
#' input, output, and plots — to a file, using a task callback (see
#' \code{\link{addTaskCallback}}) together with \code{\link{sink}}.
#' Several built-in transcript styles are provided (plain markdown,
#' simulated prompts, or commented-out output), or a fully custom
#' style can be supplied via the \code{FUN} field.
#'
#' @details
#' Call \code{$start()} to begin recording; each subsequently
#' evaluated top-level expression, together with its printed output,
#' is appended to \code{file}. Call \code{$stop()} to remove the task
#' callback, stop the \code{\link{sink}}, and halt recording. Use
#' \code{$skip()} to evaluate an expression without it (or its
#' output) being written to the transcript while recording is
#' active. Use \code{$comment()} to insert a free-text note, and
#' \code{$plot()} to save the current plot to a graphics file and
#' insert a reference to it in the transcript.
#'
#' The \code{FUN} field holds the functions that format each piece of
#' recorded information before it is written to \code{file}:
#' \describe{
#'   \item{\code{input}}{Receives a character vector of the deparsed
#'     input expression, and should return a character vector with
#'     any formatting applied (e.g. adding prompt characters or
#'     markdown code fences). Each returned element is written to
#'     \code{file} on its own line. If this element of \code{FUN} is
#'     \code{NULL}, input expressions are not written to the
#'     transcript.}
#'   \item{\code{output}}{Receives a character vector of the printed
#'     output captured via \code{\link{sink}}, and should return a
#'     character vector with any formatting applied (e.g. a comment
#'     prefix or markdown code fences). If this element of \code{FUN}
#'     is \code{NULL}, output is not written to the transcript.}
#'   \item{\code{results}}{An alternative to \code{output}. Receives
#'     the value of the evaluated expression as its first argument
#'     and a logical indicating visibility as its second, and should
#'     return a character vector of how the result should be printed
#'     (for example, a wrapper around \code{pander_return} from the
#'     \code{pander} package). If both \code{output} and
#'     \code{results} are non-\code{NULL}, the transcript will contain
#'     two copies of the output, which is probably not desired.}
#'   \item{\code{plot}}{Receives the file name of a just-created
#'     graphics file and returns a character string to be written to
#'     the transcript indicating where the graphics file should be
#'     inserted. The default returns a markdown image-inclusion
#'     command.}
#' }
#'
#' See also the \code{knitr}, \code{quarto}, and \code{rmarkdown}
#' packages for a better way to produce a transcript if you have a
#' planned script, \code{\link{savehistory}} for just saving the
#' commands, and \code{\link{sink}} for just saving the output.
#'
#' Note that some GUIs intercept the task-callback mechanism before
#' \code{R6Transcript} can use it; this class has been tested with
#' RGui, but other GUIs may give unexpected results. Also, because of
#' the callback mechanism used, errors are not recorded in the
#' transcript; warnings and other messages sent to \code{stderr} are
#' currently not included either (use \code{$comment()} to add a note
#' about them). Both could potentially be added in the future if
#' there is interest.
#'
#' @examples
#' \dontrun{
#' t1 <- R6Transcript$new("RTrans.md", type = "markdown")
#' t1$start()
#' 1 + 2
#' x <- 3 * 4
#' x
#' plot(1:10)
#' t1$plot()
#' t1$comment("A note about what just happened.")
#' t1$skip(5 + 5) # evaluated but not recorded
#' t1$stop()
#'
#' # simulate a console transcript with prompts
#' t2 <- R6Transcript$new("RTrans_prompt.md", type = "prompt")
#' t2$start()
#' for(i in 1:3) {
#'   print(i)
#' }
#' t2$stop()
#'
#' # output commented out, e.g. for pasting into a script
#' t3 <- R6Transcript$new("RTrans_comment.R", type = "comment")
#' t3$start()
#' mean(1:10)
#' t3$stop()
#'
#' # custom formatting, with results via pander
#' t4 <- R6Transcript$new("RTrans_custom.md", type = "custom")
#' t4$FUN$output <- NULL
#' t4$FUN$results <- function(x, vis) {
#'   if(vis) {
#'     pander::pander_return(x)
#'   } else {
#'     ""
#'   }
#' }
#' t4$start()
#' summary(1:10)
#' t4$stop()
#' }
#'
#' @export
R6Transcript <- R6Class("R6Transcript",
                        cloneable = FALSE,
      public = list(

        #' @field prompt Character vector of length 2 giving the
        #' primary and continuation prompts (e.g. \code{"> "} and
        #' \code{"+ "}) used by the \code{"prompt"} transcript type.
        prompt = c("> ", "+ "),

        #' @field comment_char Character. Prefix prepended to each
        #' line of output by the \code{"comment"} transcript type.
        comment_char = "##  ",

        #' @field file Character. Path to the transcript file that
        #' recorded input, output, and plot references are appended
        #' to.
        file = "",

        #' @field plot_dir Character. Directory (created if
        #' necessary) that graphics files produced by \code{$plot()}
        #' are saved into.
        plot_dir = "graphs",

        #' @field plot_name Character. \code{\link{sprintf}} pattern
        #' used to name successive graphics files; should contain an
        #' integer placeholder such as \code{\%d} or \code{\%03d}.
        #' If it does not, \code{$plot()} will insert one before the
        #' file extension.
        plot_name = "Rplot%03d.png",

        #' @field plot_fun Character. Name of the graphics device
        #' function (e.g. \code{"png"}, \code{"pdf"}) used to save
        #' plots created by \code{$plot()}.
        plot_fun = "png",

        #' @field plot_args list. Additional arguments passed to
        #' \code{plot_fun} when saving a plot via \code{$plot()}.
        plot_args = list(),

        #' @field FUN list. Formatting functions \code{input},
        #' \code{output}, \code{results}, and \code{plot} used to
        #' format recorded information before it is written to
        #' \code{file}. See Details.
        FUN = list(
          input = function(x) {
            x
          },
          output = function(x) {
            x
          },
          results = NULL,
          plot = function(x) {
            paste0("![](", x, ")\n")
          }
        ),

        #' @description
        #' Create a new \code{R6Transcript} object.
        #' @param file Character. Path to the transcript file to
        #'   create (or append to). Default \code{"RTrans.md"}.
        #' @param type Character. One of \code{"markdown"} (code
        #'   fences, no prompts), \code{"prompt"} (simulated console
        #'   prompts, output uncommented), \code{"comment"} (no
        #'   prompts, output lines prefixed with \code{comment_char}),
        #'   or \code{"custom"}/unrecognized (leaves \code{FUN}
        #'   untouched; set its elements yourself). Default
        #'   \code{"markdown"}.
        #' @param append Logical. If \code{FALSE} (the default),
        #'   \code{file} is truncated before recording begins;
        #'   if \code{TRUE}, new content is appended to any existing
        #'   file.
        initialize = function(file="RTrans.md", 
                              type=c("markdown", "prompt",
                                     "comment", 
                                     "custom"),
                              append=FALSE) {
          self$file <- file
          if(!append) {
            cat("", file = self$file)
          }
          private$outfile <- file("")
          
          private$tcb_name = paste0(
            sample(letters, 10, replace=TRUE),
            collapse=''
          )
          
          type <- match.arg(type)
          if(type=='markdown') {
            self$comment_char <- ""
            self$FUN$input <- function(x) {
              return(
                c(
                  "```r",
                  x,
                  "```"
                )
              )
            }
            self$FUN$output <- function(x) {
              return(
                c(
                  "```",
                  x,
                  "```"
                )
              )
            }
            return(invisible(self))
          } # end markdown
          
          if(type=='prompt') { # insert prompts, out uncommented
            self$FUN$input <- function(x) {
              x <- sub("^    ", self$prompt[2], x)
              x <- sub("^}", paste0(self$prompt[2], "}"), x)
              x[1] <- paste0(self$prompt[1], x[1])
              return(x)
            }
            self$FUN$output <- function(x) {
              return(x)
            }
            return(invisible(self))
          } # end prompt
          
          if(type=='comment') { # no prompts, output commented
            self$FUN$input <- function(x) {
              return(x)
            }
            self$FUN$output <- function(x) {
              x <- paste0(self$comment_char, x)
              return(x)
            }
            return(invisible(self))
          } # end comment
          
          # custom or unrecognized, not meant to be default
          cat(strwrap("You need to set the elements of the 
          FUN list."), "\n")
          return(invisible(self))
        }, # end initialize

        #' @description
        #' Begin recording by registering a task callback and
        #' \code{\link{sink}}-ing console output, so that
        #' subsequently evaluated top-level expressions are appended
        #' to \code{file}. Has no effect if already recording.
        #' @return The \code{R6Transcript} object (invisibly),
        #'   enabling method chaining.
        start = function() {
          if(private$running) {
            return(invisible(self))
          }
          private$running <- TRUE
          addTaskCallback(private$save, name=private$tcb_name)
          sink(private$outfile, split=TRUE)
          return(invisible(self))
        },

        #' @description
        #' Stop recording by removing the task callback and ending
        #' the \code{\link{sink}} started by \code{$start()}. Has no
        #' effect if not currently recording.
        #' @return The \code{R6Transcript} object (invisibly),
        #'   enabling method chaining.
        stop = function() {
          if(private$running) {
            removeTaskCallback(private$tcb_name)
            sink(NULL)
            private$running <- FALSE
          }
          return(invisible(self))
        },

        #' @description
        #' Evaluate an expression without recording it (or its
        #' output), even while recording is active.
        #' @param expr An expression to evaluate without recording.
        #' @return The value of evaluating \code{expr}.
        skip = function(expr) {
          private$record <- FALSE
          eval(expr)
        },

        #' @description
        #' Append a free-text comment to the transcript file,
        #' without it being recorded as console input/output.
        #' @param txt Character vector. Text to write to the
        #'   transcript, one element per line.
        #' @return The \code{R6Transcript} object (invisibly),
        #'   enabling method chaining.
        comment = function(txt) {
          private$record <- FALSE
          cat(
            paste0(self$coment_char, txt), 
            sep="\n", file=self$file,
              append=TRUE)
          cat("\n", append=TRUE, file=self$file)
          return(invisible(self))
        },
        #' @description
        #' Save the current plot to a graphics file in
        #' \code{plot_dir} (created if necessary) using
        #' \code{plot_fun}, and append a reference to it in the
        #' transcript via \code{FUN$plot}.
        #' @param ... Additional arguments passed to \code{plot_fun}
        #'   (e.g. \code{width}, \code{height}), overriding any of
        #'   the same name in \code{plot_args}.
        #' @return The \code{R6Transcript} object (invisibly),
        #'   enabling method chaining.
        plot = function(...) {
          private$record <- FALSE
          if(!grepl("%[-+ .0-9]*[dioxXf]", self$plot_name)) {
            self$plot_name <- sub("[.]", "%d.", self$plot_name)
          }
          if(!dir.exists(self$plot_dir)) {
            dir.create(self$plot_dir, recursive = TRUE)
          }
          while(file.exists( fname <- 
            sprintf(paste0(self$plot_dir, "/", self$plot_name),
                    private$plot_number)
          )) {
            private$plot_number <- private$plot_number + 1
          }
          tmp.args <- self$plot_args
          tmp.args$filename <- fname
          dots <- list(...)
          for(i in seq_along(dots)) {
            tmp.args[[names(dots)[i]]] <- dots[[i]]
          }
          tmp <- recordPlot()
          do.call(self$plot_fun, tmp.args)
          replayPlot(tmp)
          dev.off()
          cat(
            self$FUN$plot(fname), "\n", 
            file = self$file, append=TRUE
          )
          return(invisible(self))
        }
      ),
      private = list(

        # Task-callback handler that formats and appends an
        # evaluated top-level expression's input, output, and/or
        # results to the transcript file; not normally called
        # directly. `ok` is currently unused: due to the callback
        # mechanism used, errors are not recorded in the transcript.
        save = function(expr, value, ok, visible) {
          out <- readLines(private$outfile, 
                           warn=FALSE)
          if(private$record) {
            if(!is.null(self$FUN$input)) {
              cat(self$FUN$input(
                deparse(expr)
              ), "\n", file=self$file,
              sep="\n", append=TRUE
              )
            }
            if(!is.null(self$FUN$output)) {
              cat(self$FUN$output(
                out
              ), "\n", file=self$file,
              sep="\n", append=TRUE
              )
            }
            if(!is.null(self$FUN$results)) {
              cat(self$FUN$results(
                value, visible
              ), "\n", file=self$file,
              sep="\n", append=TRUE
              )
            }
          } else {
            private$record <- TRUE
          }
          return(TRUE)
        },
        record = FALSE,
        running = FALSE,
        outfile = NULL,
        tcb_name = "",
        plot_number = 1,
        finalize = function() {
          if(private$running) {
            self$stop()
          }
          close(private$outfile)
        }
      )
                        )