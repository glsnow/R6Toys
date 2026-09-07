## tests R6DLL ----

source("pkg/R/R6DLL.R")

dll <- R6DLL$new()
dll$push(1)$push(2)$push("c")
dll$pop()
dll$pop()
dll$pop()
dll$pop()

for(i in 1:10) dll$push(letters[i])
dll$shift()
dll$shift()
dll$pop()
dll$unshift(2)
dll$unshift(1)

dll$toList()

dll$iter_reset()
while(!dll$finished) {
  print( dll$peek_current() )
  dll$iter_next()
}

dll$iter_reset(last=TRUE)$iter_prev()$iter_prev()$iter_prev()
dll$peek_current()
dll$peek_next()
dll$peek_prev()

dll$iter_reset()
while(is.numeric(dll$peek_current())) {
  dll$iter_next()
}
dll$peek_current()

dll$insert("Something New")
dll$insert("Something old", "before")

dll$iter_all()

dll$iter_next()$iter_next()
dll$peek_current()

while(dll$peek_current() < "g") {
  dll$iter_next()
}

dll$peek_current()

dll$remove()

dll$iter_reset()
repeat {
  print(dll$peek_current())
  if (dll$iter_next()$finished)
    break
}

dll$iter_find(function(x) grepl("[A-Z]", x))
dll$peek_current()
dll$iter_find(function(x) grepl("[A-Z]", x))
dll$peek_current()
dll$iter_find(function(x) grepl("[A-Z]", x))
dll$peek_current()



state.dll <- R6DLL$new()

# insertion sort
for(i in seq_along(state.name)) {
  state.dll$iter_reset()
  state.dll$iter_find(
    function(x) x$stats['Population'] > state.x77[i,'Population']
  )
  state.dll$insert(
    list(name=state.name[i], abb=state.abb[i], 
         stats=state.x77[i,]),
    loc= "before"
  )
}

state.dll$toList()
state.dll$toList(\(x){data.frame(State=x$abb, Pop=x$stats[1])}, 
                 simplify=rbind) 

state.dll$iter_all(function(x) cat(x$name, ": ", x$stats[['Population']], "\n"))

state.dll$iter_reset()
state.dll$swap(loc='after')
state.dll$iter_reset(last = TRUE)
state.dll$swap(loc='before')

state.dll$iter_prev()$iter_prev()$swap(loc='before')

state.dll$iter_all(function(x) cat(x$name, ": ", x$stats[['Population']], "\n"))


gc()
pryr::mem_used()
test.dll <- R6DLL$new()

for(i in 1:10000) test.dll$push(i)
gc()
pryr::mem_used()
rm(test.dll)
gc()
pryr::mem_used()

pryr::mem_change({
  test.dll <- R6DLL$new()
  for(i in 1:10000) test.dll$push(i)
})

pryr::mem_change( {
  rm(test.dll) 
  gc()
})

## tests R6Counter ----

source("pkg/R/R6Counter.R")

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


## tests R6ProgressBar ----

source("pkg/R/R6ProgressBar.R")

pb1 <- R6ProgressBar$new(100)

while(!pb1$finished) {
  pb1$inc()
  Sys.sleep(0.1)
}

rm(pb1)
gc()

pb2 <- R6ProgressBar$new(100, type='tk')

while(!pb2$finished) {
  pb2$inc()
  Sys.sleep(0.1)
}

rm(pb2)
gc()

pb3 <- R6ProgressBar$new(100, type='win')

while(!pb3$finished) {
  pb3$inc()
  Sys.sleep(0.1)
}

rm(pb3)
gc()

pb4 <- R6ProgressBar$new(100, type='tk')

out <- replicate(100, {pb4$pp; Sys.sleep(0.1)})
out <- replicate(100, {pb4$mm; Sys.sleep(0.1)})

pb4$set(50)
pb4$reset

pb4$reset <- 20

rm(pb4)
gc()


pb5 <- R6ProgressBar$new(26, label=LETTERS)
replicate(26, {pb5$pp; Sys.sleep(0.1); pb5$value})

pb5$reset

sapply(1:26, pb5$run(\(x){cat(letters[x], "\n"); Sys.sleep(0.1); x*10}))

pb5$reset
pb5$cycle <- TRUE
replicate(100, {pb5$pp; Sys.sleep(0.1); pb5$value})

rm(pb5)
gc()



## Robots ----

r <- Robots$new()
r$plot()
r$print()

## interactive graphics ----

# open a graphics device that works with
# getGraphicsEvent
dev.new(width=6, height=3, noRStudioGD = TRUE) 
r <- Robots$new(30)
getGraphicsEvent(
  prompt='Ready',
  onKeybd=function(A){
    switch(A,
           q=r$move('nw'),
           w=r$move('n'),
           e=r$move('ne'),
           a=r$move('w'),
           s=r$move('.'),
           d=r$move('e'),
           z=r$move('sw'),
           x=r$move('s'),
           c=r$move('se'),
           t=r$move('t'),
           ' '=r$move('.'))
  }
)

dev.new(width=6, height=3, noRStudioGD = TRUE)
r <- Robots$new(30)
getGraphicsEvent(
  prompt='Ready',
  onKeybd=function(A){
    switch(A,
           "'"=r$move('nw'),
           ","=r$move('n'),
           "."=r$move('ne'),
           a=r$move('w'),
           o=r$move('.'),
           e=r$move('e'),
           ";"=r$move('sw'),
           q=r$move('s'),
           j=r$move('se'),
           t=r$move('t'),
           y=r$move('t'),
           ' '=r$move('.'))
  }
)


## test auto timing not working with onIdle ----

lstm <- Sys.time() + 1000
dev.new(width=6, height=3, noRStudioGD = TRUE)
r <- Robots$new(30)

tclAfter(10*1000, function() r$move('.'))
getGraphicsEvent(
  prompt='Ready',
  onKeybd=function(A){
    lstm <<- Sys.time()
    switch(A,
           "'"=r$move('nw'),
           ","=r$move('n'),
           "."=r$move('ne'),
           a=r$move('w'),
           o=r$move('.'),
           e=r$move('e'),
           ";"=r$move('sw'),
           q=r$move('s'),
           j=r$move('se'),
           t=r$move('t'),
           y=r$move('t'),
           ' '=r$move('.'))
  },
  onIdle = function() {
    if(as.numeric(difftime(Sys.time(), lstm, units='secs')) > 10) {
      lstm <<- Sys.time()
      r$move('.')
    }
  }
)

## list stuff -----

tmplst <- list(a=1, b=2)

tmpfun <- function(...) {
  a <- tmplst
  b <- list(...)
  nms <- names(b)
  for(i in nms) {
    a[[i]] <- b[[i]]
  }
  a
}

tmpfun( c=3)
tmpfun( d=5, a=NULL)

## TurtleGraphics ----

t1 <- R6TurtleGraphics$new()
t1$PenDown()

nsides <- 7
len <- 200/nsides
for(i in 1:nsides) {
  t1$move(len)
  t1$turn(360/nsides)
  if(interactive()) Sys.sleep(0.5)
}

t2 <- R6TurtleGraphics$new()
t2$PenDown()

nsides <- 4
len <- 200/nsides
npoly <- 13
cols <- rainbow(npoly)
for(i in 1:npoly) {
  t2$NewPen(col=cols[i])
  for(j in 1:nsides) {
    t2$move(len)
    t2$turn(360/nsides)
    if(interactive()) Sys.sleep(0.2)
  }
  t2$turn(360/npoly)
}

# dance of the turtles

tmpfun <- function(i) {
  tmp <- R6TurtleGraphics$new(add = i != 1,
    autoplot = FALSE)
  tmp$turn(360/npoly*i)
  tmp$NewPen(col=cols[i])
  tmp$PenDown()
  tmp$turtle.size <- 4
  tmp$pen.cex <- 1
  tmp
}

nsides <- 300
len <- 200/nsides

turtles <- lapply(1:npoly, tmpfun)

turtles[[1]]$plot()

for(j in 1:nsides) {
  for(i in 1:npoly) {
    turtles[[i]]$move(len)
    turtles[[i]]$turn(360/nsides)
    turtles[[i]]$plot()
  }
  if(interactive()) Sys.sleep(0.05)
}

t1 <- R6TurtleGraphics$new()
t1$NewPen(col=rainbow(1, start=0))
t1$PenDown()
t1$autoplot <- FALSE
for(i in seq_len(175)) {
  t1$forward(i)
  t1$turn(80)
  if(i%%4 == 0) t1$NewPen(col=rainbow(1, start=i/176))
}
t1$plot()



## clipboard editor ----

sl <- R6ClipboardEdit$new('slash')

#  C:\Users\gls33\Box\Statistics-GLS33\Statistics-GLS33\JSM
#  C:/Users/gls33/Box/Statistics-GLS33/Statistics-GLS33/JSM

custom <- R6ClipboardEdit$new(\(x){gsub('[0-9]', '?', x)})
# mean: 123.45, sd: 5.4
# mean: ???.??, sd: ?.?

cc <- R6ClipboardEdit$new(2)

x <- rnorm(1000)
mean(x)
sd(x)
quantile(x)

mean(
  x
)
sd(
  x
)

ccco <- R6ClipboardEdit$new('ccco')

x <- rnorm(1000)
mean(x)
##  [1] 0.01287882
sd(x)
##  [1] 0.9976802
quantile(x)
##            0%          25%          50%          75%         100% 
##  -2.954844152 -0.648035221  0.007866633  0.674765782  3.179929403 

mean(
  x
)
##  [1] 0.01287882
sd(
  x
)
##  [1] 0.9976802

tt <- R6ClipboardEdit$new("tt")
summary(lm(mpg ~ ., data=mtcars))

com <- R6ClipboardEdit$new("comment")
ucom <- R6ClipboardEdit$new("uncomment")

##  x <- rnorm(1000)
##  mean(x)
##  sd(x)
##  quantile(x)

wc <- R6ClipboardEdit$new("wc")

#  one upon a time the quick brown fox jumped over the lazy
#  dogs back and forth and again and again and again

di <- R6ClipboardEdit$new('di')

fit<n> <- lm(a ~ b, data=c)
fit<n>.0 <- update(fit<n>, .~1)
anova(fit<n>, fit<n>.0)
summary(fit<n>)

fit1 <- lm(a ~ b, data=c)
fit1.0 <- update(fit1, .~1)
anova(fit1, fit1.0)
summary(fit1)

fit1 <- lm(a ~ b, data=c)
fit1.0 <- update(fit1, .~1)
anova(fit1, fit1.0)
summary(fit1)

fit3 <- lm(a ~ b, data=c)
fit3.0 <- update(fit3, .~1)
anova(fit3, fit3.0)
summary(fit3)

## environments (hash) ----

e <- new.env(parent=emptyenv())
e$a <- 1
e$b <- 2
names(e)
ls(e)

e[['a']]
e[['5']] <- 'five'
names(e)
e[[5]]

e[['.._%^*']] <- TRUE
names(e)

## R6Dict ----

d <- R6Dict$new(keys=letters[1:5], values=LETTERS[1:5])
d$keys()
d$remove('c')
d$add(z=1, y=2)
d$add(list(x=3, w=4))
for(i in d$keys()) cat(i, ": ", d$value(i), "\n")
d$toList()
d$toList(simplify=FALSE)
sapply(sort(d$keys()), function(x) d$value(x))

tab <- R6Dict$new()
for(i in state.name) {
  tab$counter(substring(i, 1, 1))$pp
}
tab$toList()

tab2 <- R6Dict$new()
for(i in state.name) {
  tab2$DLL(substring(i, 1, 1))$push(i)
}
tab2$toList()


## associate ----

library(harrypotter)
library(tidytext)
library(tidyverse)
#library(fuzzyjoin)

word_pos <- tibble(text = philosophers_stone[2]) |>
  unnest_tokens(word, text) |>
  anti_join(stop_words, by='word') |>
  mutate(word_id = row_number())

word_pairs <- word_pos |>
  difference_inner_join(
    word_pos, 
    by="word_id",
    max_dist = 10
  ) |>
  filter(word.x < word.y) |>
  count(word.x, word.y, sort=TRUE) |>
  rename(word1=word.x, word2=word.y, count=n)


head(word_pairs)

library(igraph)

hp <- as.matrix(word_pairs[1:100, 1:2]) |>
  t() |>
  as.vector() |>
  make_graph(directed=FALSE)

plot(hp)
hp2 <- largest_component(hp)
length(V(hp2))
length(E(hp2))
edge_connectivity(hp2)
plot(hp2, )
tkplot(hp2)



regexpr("^s(t(a(r(t)?)?)?)?", c(tmp,'cat'))

tmp <- data.frame(txt=paste(letters, collapse=' '))
unnest_skip_ngrams(tmp, word, txt, n=2, n_min=2, k=5)

sw <- function(x) {
   sapply(strsplit(x, ' '), function(y) {
     if(y[1] == y[2]) return(NA)
     paste(sort(y), collapse=' ')
   }
   )
}

tmp1 <- tibble(text = philosophers_stone[4]) |>
  unnest_tokens(word, text) |>
  anti_join(stop_words, by='word')
tmp2 <- paste(tmp1$word, collapse=" ")
tmp3 <- tibble(txt=tmp2) |>
  unnest_skip_ngrams(pair, txt, n=2, n_min=2, k=10) |>
  mutate(pair=sw(pair)) |>
  count(pair, sort=TRUE) |>
  na.omit()

hp1 <- strsplit(tmp3$pair[1:50], ' ') |>
  unlist() |>
  make_graph(directed=FALSE) |>
  largest_component()

length(V(hp1))
length(E(hp1))  
plot(hp1)

tmp4 <- strsplit(tmp3$pair[tmp3$n > 2], ' ')
tmp_v <- names(V(hp1))
for(i in seq_along(tmp4)) {
  if(all(tmp4[[i]] %in% tmp_v)) {
    cat('(', i, ') ', tmp4[[i]], "\n")
    hp1 <- add_edges(hp1, match(tmp4[[i]], tmp_v))
  }
}
hp1 <- simplify(hp1)
length(E(hp1))
plot(hp1)

plot(hp1)



lm1 <- c(
  "lm", "coef",
  "lm", "effects", 
  "lm", "residuals",
  "lm", "fitted",
  "lm", "vcov",
  "coef", "fitted",
  "coef", "residuals",
  "coef", "glm",
  "glm", "fitted",
  "glm", "residuals",
  "glm", "esoph",
  "glm", "infert",
  "residuals", "resid",
  "fitted", "fitted.values"
) |>
  make_graph(directed=FALSE)
V(lm1)$name <- gsub("[a-z]", "-", V(lm1)$name)
lay1 <- layout_nicely(lm1)
plot(lm1, vertex.frame.color=NA, vertex.shape = 'rectangle',
     vertex.size=10*nchar(names(V(lm1))), vertex.color='lightblue',
     layout=lay1)




## text connection inside R6 ----

R6t <- R6Class("R6t", 
               public=list(
                 con = NULL,
                 c.dll = NULL,
                 r.dll = NULL,
                 o.dll = NULL,
                 cbm = NULL,
                 initialize=function() {
                   self$c.dll <- R6DLL$new()
                   self$r.dll <- R6DLL$new()
                   self$o.dll <- R6DLL$new()
                   self$con <- file()
                   self$cbm <- taskCallbackManager()
                 },
                 start = function() {
                   sink(file=self$con, split=TRUE)
                   self$cbm$add(private$record, name='trns')
                 },
                 end=function() {
                   sink()
                   self$cbm$remove("trns")
                 }
               ),
               private = list(
                 record = function(cmd, result, success, visible) {
                   self$c.dll$push(cmd)
                   self$r.dll$push(result)
                   self$o.dll$push(readLines(self$con))
                   return(TRUE)
                 },
                 finalize = function() {
                   close(self$con)
                 }
               )
               )
tst <- R6t$new()
tst$start()
1 + 2
log(-1)
3 + 4
x <- rnorm(10)
tst$end()
tst$c.dll$peek_first()
tst$c.dll$toList()
tst$r.dll$toList()
tst$o.dll$toList()



