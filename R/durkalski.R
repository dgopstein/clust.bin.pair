# Durkalski 2003

durkalski.test <- function (x, group.names, pre.measure.name, post.measure.name) {
  z <- results.to.contingency.cols(x, group.names, pre.measure.name, post.measure.name)
  durkalski.impl(z$nk, z$bk, z$ck)
}

durkalski.impl <- function(nk, bk, ck) {    
  sum( (1/nk)*(bk-ck) )^2/sum(((bk - ck) / nk)^2 )
}
