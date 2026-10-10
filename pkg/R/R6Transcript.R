

R6Transcript <- R6Class("R6Transcript",
                        cloneable = FALSE,
      public = list(
        prompt = c("> ", "+ "),
        comment_char = "##  ",
        file = "",
        plot_dir = "graphs",
        plot_name = "Rplot%03d.png",
        plot_fun = "png",
        plot_args = list(),
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
        start = function() {
          if(private$running) {
            return(invisible(self))
          }
          private$running <- TRUE
          addTaskCallback(private$save, name=private$tcb_name)
          sink(private$outfile, split=TRUE)
          return(invisible(self))
        },
        stop = function() {
          if(private$running) {
            removeTaskCallback(private$tcb_name)
            sink(NULL)
            private$running <- FALSE
          }
          return(invisible(self))
        },
        skip = function(expr) {
          private$record <- FALSE
          eval(expr)
        },
        comment = function(txt) {
          private$record <- FALSE
          cat(
            paste0(self$coment_char, txt), 
            sep="\n", file=self$file,
              append=TRUE)
          cat("\n", append=TRUE, file=self$file)
          return(invisible(self))
        },
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
        save = function(expr, value, ok, visible) {
          # currently ok is always TRUE, so we can ignore it
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



# in the FUN slot there should be functions for 
# processing input/output/plot_names

# The input function will receive a character vector
# representing the commands or input, the function should
# return a character vector with any formatting applied
# e.g. add prompt characters or markdown code indicators
# the result will be written to the transcript file 
# with each line separated by a newline character.
# If this element of FUN does not exist or is NULL
# then the commands will not be written to the transcript
# file.

# The output function receives a single vector of 
# character strings (the output via sink).  The function
# then adds any formatting and returns a character
# vector that will be written to the transcript file
# separated by newlines.

# The results function should accept an R object as
# the first argument and a logical indicating visibility
# as the second.  This is an alternative to the output
# function.  This function should return a character
# vector with how you want the results printed.  One
# option would be a wrapper around pander_return from
# the pander package.

# If you have the pander package loaded you can set
# the output function to NULL and set results to a
# function like:
# function(x, vis) {
#   if(vis) {
#     pander_return(x)
#   } else {
#     ""
#   }
# }

# if both output and results are non-null then the 
# file will have 2 copies of the output, which is probably
# not what you want.

# The plot function takes the name of the just created
# graphics file and returns a character string that
# will be included in the transcript file to indicate
# the graphics file should be inserted here.  The 
# default is the markdown command for including an
# image.

# See also the knitr, quarto and rmarkdown packages for 
# a better way if you have a planned script.  The 
# savehistory function for just saving the commands, 
# and sink for just saving the output.

# Note: that some GUIs intercept the task callback 
# mechanism before R6Transcript.  This has been tested
# with RGui, but you may get strange results with other
# Guis.

# Due to the callback mechanism used, errors will not
# be recorded in the transcript.  Currently warnings
# and any other messages sent to stderr will not be
# included in the transcript (use the comment method
# to include a note).  There are possibilities of adding
# both of these in the future if there is interest.