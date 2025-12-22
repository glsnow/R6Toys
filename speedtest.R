library(microbenchmark)



dll <- function(){
  s1 <- R6DLL$new()
  for(i in 1:10000) {
    s1$push(c(i,i))
  }
  for(i in 1:10000) s1$pop()
}

ps <- function() {
  s2 <- PointStack$new()
  for(i in 1:10000) {
    s2$push(i, i)
  }
  for(i in 1:10000) s2$pop()
}

system.time(dll())
system.time(ps())


mb <- microbenchmark(
  dll(),
  ps()
)

mb
plot(mb)

ggplot2::autoplot(mb)


## first part of a vector, seq_len vs. head ----


h1 <- function() {
  x <- 1:10000
  for(i in 1:10000) {
    y <- head(x,i)
  }
}

sl1 <- function() {
  x <- 1:10000
  for(i in 1:10000) {
    y <- x[seq_len(i)]
  }
}

mb <- microbenchmark(
  h1(),
  sl1()
)

mb
plot(mb)
ggplot2::autoplot(mb)
