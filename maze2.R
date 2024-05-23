library(igraph)

n.row <- 10
n.col <- 20

from.1 <- from.2 <- to.1 <- to.2 <- seq_len(n.row*n.col)
from.1 <- head(from.1, -n.row)
to.1 <- tail(to.1, -n.row)
from.2 <- from.2[(from.2 %% n.row) != 0]
to.2 <- to.2[(to.2 %% n.row) !=1]


m1 <- graph_from_data_frame(
  data.frame(from=c(from.1, from.2), to=c(to.1, to.2),
             weight = runif(length(from.1)+length(from.2))),
  directed=FALSE
)

m2 <- mst(m1)

m3 <- matrix(0, nrow=n.row*2+1, ncol=n.col*2+1)
m3[as.matrix(expand.grid(2*seq_len(n.row), 2*seq_len(n.col)))] <- 1

tmp <- as_data_frame(m2, what='edges')
tmp$from <- as.numeric(tmp$from)
tmp$to <- as.numeric(tmp$to)
tmp2 <- matrix(nrow=n.row, ncol=n.col)
tmp3 <- row(tmp2)[tmp$from] + row(tmp2)[tmp$to]
tmp4 <- col(tmp2)[tmp$from] + col(tmp2)[tmp$to]

m3[cbind(tmp3,tmp4)] <- 1

x <- seq(0, 2*n.col+1) + c(1/3, -1/3)
y <- seq(0, 2*n.row+1) + c(1/3, -1/3)

image(z=t(1-m3), asp=1, axes=FALSE, 
      x=x, y=y,
      xlab='', ylab='')

edge_attr(m2, 'weight') <- 1
dia <- get_diameter(m2)
lines(
  col(tmp2)[dia]*2-1/2,
  row(tmp2)[dia]*2-1/2,
  col='blue'
)
