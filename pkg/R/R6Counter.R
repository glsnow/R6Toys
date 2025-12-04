

R6Counter <- R6Class("R6Counter",
                     public=list(value=0)
                   )

## initialize ----

R6Counter$set("public", "initialize", function(x=0, ...) {
  self$value <- x
  return(self)
})

## set ----

R6Counter$set("public", "set", function(x=0){
  self$value <- x
  return(self)
})


# print ----

R6Counter$set("public", "print", function() {
  print(self$value)
})


## inc ----

R6Counter$set("public", "inc", function(x=1) {
  self$value <- self$value + x
  return(self)
})


## dec ----

R6Counter$set("public", "dec", function(x= 1) {
  self$value <- self$value - x
  return(self)
})


## pp ----

R6Counter$set("active", "pp", function(x) {
  if(missing(x)) {
    x <- 1
  }
  self$inc(x)
  return(self$value)
})


## mm ----

R6Counter$set("active", "mm", function(x) {
  if(missing(x)) {
    x <- 1
  }
  self$dec(x)
  return(self$value)
})


## ppp ----

R6Counter$set("active", "ppp", function(x) {
  if(missing(x)) {
    x <- 1
  }
  tmp <- self$value
  self$inc(x)
  return(tmp)
})


## mmm ----

R6Counter$set("active", "mmm", function(x) {
  if(missing(x)) {
    x <- 1
  }
  tmp <- self$value
  self$dec(x)
  return(tmp)
})


