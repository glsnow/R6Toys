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

dev.new()
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


