corr.matched <- function(x, group.name, pre.measure.name, post.measure.name) {
  
}

x <- confusion
group.name <- 'subject'
pre.measure.name <- 'control'
post.measure.name <- 'treatment'

durkalski.test <- function (x, group.name, pre.measure.name, post.measure.name) {
  # nk <- nrow(x[pre.measure.name])+ nrow(x[post.measure.name])
  # group(x, group.name)
  # 
  # aggregate(x, by=list(x$subject), FUN=sum)
  nk <- aggregate(x, by=list(x[[group.name]]), FUN=function(a)2*length(a))[,2]
  
  #nk <- Reduce("+", abcd)
  
  bk <- abcd$TF
  ck <- abcd$FT
  
  X2v <- sum( (1/nk)*(bk-ck) )^2/sum(((bk - ck) / nk)^2)
  
  X2v
}

durkalski.test(confusion, group.name, pre.measure.name, post.measure.name)
