

R6Counter <- R6Class("R6Counter",
                     public=list(value=0)
                   )

R6Counter$set("public", "initialize", function(x, ...) {
  self$value <- x
  return(self)
})

R6Counter$set("public", "set", function(x=0){
  self$value <- x
  return(self)
})

R6Counter$set("public", "print", function() {
  print(self$value)
})

R6Counter$set("public", "inc", function(x=1) {
  self$value <- self$value + x
  return(self)
})

R6Counter$set("public", "dec", function(x= 1) {
  self$value <- self$value - x
  return(self)
})


R6Counter$set("active", "pp", function(x) {
  if(missing(x)) {
    x <- 1
  }
  self$inc(x)
  return(self$value)
})

R6Counter$set("active", "mm", function(x) {
  if(missing(x)) {
    x <- 1
  }
  self$dec(x)
  return(self$value)
})

R6Counter$set("active", "ppp", function(x) {
  if(missing(x)) {
    x <- 1
  }
  tmp <- self$value
  self$inc(x)
  return(tmp)
})

R6Counter$set("active", "mmm", function(x) {
  if(missing(x)) {
    x <- 1
  }
  tmp <- self$value
  self$dec(x)
  return(tmp)
})




y <- R6Counter$new(5)
y$inc()

y$dec()$dec()$dec()

y$inc(5)

y$dec(3)

y$inc( -2)


y$pp
y$pp <- 3
y

y$set(5)

while(y$pp < 10) {
  print(y)
}

y$set(5)

while(y$mm) {
  print(y)
}

y$set(5)

while(y$ppp < 10) {
  print(y)
}

y$set(5)

while(y$mmm) {
  print(y)
}

y$set(5)

while(y$value) {
  print(y$mm)
}

y$set(5)

while(y$value) {
  print(y$mmm)
}

pb <- txtProgressBar(min=0, max=26, style=3)
y$set(0)

for(i in LETTERS) {
  Sys.sleep(0.5)
  setTxtProgressBar(pb, y$pp)
  cat("\n", i, "\n")
  flush.console()
}

close(pb)
