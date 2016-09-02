library(data.table)

corr.matched <- function(x, group.name, pre.measure.name, post.measure.name) {
  
}

x.in <- confusion
group.names <- c('subject', 'atom')
pre.measure.name <- 'control'
post.measure.name <- 'treatment'

durkalski.test <- function (x.in, group.names, pre.measure.name, post.measure.name) {
  x <- as.data.table(x.in)
  pre.measure  <- x[[ pre.measure.name]]
  post.measure <- x[[post.measure.name]]

  groups <- as.list(data.frame(sapply(group.names, function(a) x[[a]])))
  
  nk <- x[, .N, by=groups]$N
  
  x[, bs := pre.measure & !post.measure]
  x[, cs := !pre.measure & post.measure]
  
  x[, sum(cs),by=groups]
  
  bk <- x[, sum(bs),by=groups]
  ck <- x[, sum(cs),by=groups]
  
  X2v <- sum( (1/nk)*(bk-ck) )^2/sum(((bk - ck) / nk)^2)
  
  X2v
}

durkalski.test(confusion, group.names, pre.measure.name, post.measure.name)
