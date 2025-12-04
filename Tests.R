## tests R6DLL ----

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





