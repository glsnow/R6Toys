t1 <- TurtleGraphics$new()
t1$down
t1$Forward(20)
t2 <- t1$clone()
t2$Left(90)
t1$Right(90)
t2$cur.color <- 'red'
t1$Forward(25)
t2$Forward(25)
t3 <- TurtleGraphics$new()
t3$down
for(i in 1:10){
  t3$Forward(25)
  t3$Right(360/10*3)
}

t1$Right(90)
t2$Left(90)
t1$Forward(25)
t2$Forward(25)

