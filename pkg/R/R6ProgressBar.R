R6ProgressBar <- R6Class("R6ProgressBar", 
                         inherit=R6Counter,
                         public=list(
                           value=0, 
                           type='',
                           label='',
                           max=0,
                           pb=NULL
                         ))

## initialize ----

R6ProgressBar$set("public", "initialize", function(max, min=0, 
                                                   initial=0,
                                                   type=c("txt", "tk", "win"),
                                                   style=3, label='%d',
                                                   ...
                                                   ) {
  self$value <- initial
  type <- match.arg(type)
  if(type=='tk') require('tcltk')
  self$type <- type
  self$label <- label
  self$max <- max
  self$pb <- switch(type,
                    txt=txtProgressBar(min=min, max=max, initial=initial, 
                                       label=sprintf(label, initial), 
                                       style=style, ...),
                    tk=tkProgressBar(min=min, max=max, initial=initial, 
                                     label=sprintf(label, initial), ...) ,
                    win=winProgressBar(min=min, max=max, initial=initial, 
                                       label=sprintf(label, initial), ...)
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
  switch(self$type,
         txt= setTxtProgressBar(self$pb, self$value, 
                                label=sprintf(self$label, self$value)),
         tk = setTkProgressBar(self$pb, self$value, 
                               label=sprintf(self$label, self$value)),
         win= setWinProgressBar(self$pb, self$value, label=
                                  sprintf(self$label, self$value))
           )
})

## dec ----

R6ProgressBar$set("public", "dec", function(x=1) {
  super$dec(x)
  switch(self$type,
         txt= setTxtProgressBar(self$pb, self$value, 
                                label=sprintf(self$label, self$value)),
         tk = setTkProgressBar(self$pb, self$value, 
                               label=sprintf(self$label, self$value)),
         win= setWinProgressBar(self$pb, self$value, label=
                                  sprintf(self$label, self$value))
           )
})

## set ----

R6ProgressBar$set("public", "set", function(x=0) {
  super$set(x)
  switch(self$type,
         txt= setTxtProgressBar(self$pb, self$value, 
                                label=sprintf(self$label, self$value)),
         tk = setTkProgressBar(self$pb, self$value, 
                               label=sprintf(self$label, self$value)),
         win= setWinProgressBar(self$pb, self$value, label=
                                  sprintf(self$label, self$value))
  )
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




