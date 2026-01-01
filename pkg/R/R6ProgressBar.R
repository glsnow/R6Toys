R6ProgressBar <- R6Class("R6ProgressBar", 
                         inherit = R6Counter,
                         public = list(
                           value=0, 
                           type='',
                           label='',
                           max=0,
                           pb=NULL,
                           cycle=FALSE
                         ))

## initialize ----

R6ProgressBar$set("public", "initialize", function(max, min=0, 
                                                   initial=0,
                                                   type=c("auto", "txt", "tk", "win"),
                                                   style=3, label="%d",
                                                   cycle=FALSE,
                                                   ...
                                                   ) {
  self$value <- initial
  type <- match.arg(type)
  if(type == 'auto') {
    if(exists("winProgressBar", mode='function')) {
      type <- "win"
    } else if(require(tcltk) && .TkUp) {
      type <- "tk"
    } else {
      type <- "txt"
    }
  }
  if(type=='tk') require('tcltk')
  self$type <- type
  self$label <- label
  self$max <- max
  self$cycle <- cycle
  if(length(label) > 1) {
    lab <- label[1]
  } else {
    lab <- sprintf(label, initial)
  }
  self$pb <- switch(type,
                    txt=txtProgressBar(min=min, max=max, initial=initial, 
                                       label=lab, 
                                       style=style, ...),
                    tk=tkProgressBar(min=min, max=max, initial=initial, 
                                     label=lab, ...) ,
                    win=winProgressBar(min=min, max=max, initial=initial, 
                                       label=lab, ...)
                    )
  invisible(self)
})

## finalize ----

R6ProgressBar$set("private", "finalize", function(){
  close(self$pb)
})

## inc ----

R6ProgressBar$set("public", "inc", function(x=1) {
  super$inc(x)
  if(self$cycle) {
    super$set( ((self$value - 1) %% self$max ) + 1 )
  }
  if(length(self$label) > 1) {
    lab <- self$label[self$value]
  } else {
    lab <- sprintf(self$label, self$value)
  }
  switch(self$type,
         txt= setTxtProgressBar(self$pb, self$value, 
                                label=lab),
         tk = setTkProgressBar(self$pb, self$value, 
                               label=lab),
         win= setWinProgressBar(self$pb, self$value, label=
                                  lab)
           )
})

## dec ----

R6ProgressBar$set("public", "dec", function(x=1) {
  super$dec(x)
  if(length(self$label) > 1) {
    if(self$value) {
      lab <- self$label[self$value]
    } else {
      lab <- self$label[1]
    }
  } else {
    lab <- sprintf(self$label, self$value)
  }
  switch(self$type,
         txt= setTxtProgressBar(self$pb, self$value, 
                                label=lab),
         tk = setTkProgressBar(self$pb, self$value, 
                               label=lab),
         win= setWinProgressBar(self$pb, self$value, label=
                                  lab)
           )
})

## set ----

R6ProgressBar$set("public", "set", function(x=0) {
  super$set(x)
  if(length(self$label) > 1) {
    if(self$value) {
      lab <- self$label[self$value]
    } else {
      lab <- self$label[1]
    }
  } else {
    lab <- sprintf(self$label, self$value)
  }
  switch(self$type,
         txt= setTxtProgressBar(self$pb, self$value, 
                                label=lab),
         tk = setTkProgressBar(self$pb, self$value, 
                               label=lab),
         win= setWinProgressBar(self$pb, self$value, label=
                                  lab)
  )
  return(invisible(self))
})

## reset ----

R6ProgressBar$set("active", "reset", function(x) {
  if(missing(x)) {
    x <- 0
  }
  self$set(x)
})


## finished ----

R6ProgressBar$set("active", "finished", function(...) {
  self$value >= self$max
})


## run ----

R6ProgressBar$set("public", "run", function(FUN=I) {
  function(...) {
    self$inc()
    FUN(...)
  }
})

