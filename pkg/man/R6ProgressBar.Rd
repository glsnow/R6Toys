\name{R6ProgressBar}
\alias{R6ProgressBar}
\title{
Create an R6 reference class to wrap the progress bar functions
}
\description{
This function provides an R6 wrapper around the built-in progress bar 
functions.  It also inherits from the R6Counter objects in this same
package.  This allows you to update the progress bar by incrementing 
rather than having to keep a separate counter or querying the progress
bar before updating it.
}
\usage{R6ProgressBar$new(max, min=0, initial=0, 
type=c("auto", "txt", "tk", "win"), style=3, label="\%d", 
cycle=FALSE, ...)}

\section{Methods}{

\describe{
\item{\code{new}, \code{initialize}}{Creates a new progress bar.

You need to specify the maximum value (an integer).  If 
\code{type="auto", then it will try the different types in 
the order \code{"win", "tk", "txt"}}.

\code{style} is only used for text progress bars.  The 
\code{...} variables will be passed on to the appropriate
progress bar creation function (so can be used to set the 
window title or other options).

\code{cylce} is a logical value indicating if the progress
bar should reset to 1 when it goes beyond \code{max}.

\code{label} is either a single format string that will be
passed to \code{sprintf} for formatting the current value, or
a vector of strings of length \code{max} that will be used
to label the progress bar.
}

\item{\code{inc(x=1)}}{

}

}
}

\section{Active Bindings}{

Active bindings allow quick access to members of the class
and the ability to update the progress bar without \code{()}.



}

\examples{
pb1 <- R6ProgressBar$new(100)

while(!pb1$finished) {
  pb1$inc()
  Sys.sleep(0.1)
}

pb1$value

pb1$reset

out1 <- replicate(100, {pb1$pp; Sys.sleep(0.1)})
out2 <- replicate(100, {pb1$mm; Sys.sleep(0.1)})

rm(pb1)
gc()
}
\keyword{R6}
\keyword{progress bar}


