corr.matched <- function(x, group.name, pre.measure.name, post.measure.name) {
  
}

mcnemars.clustered.durkalski <- function (x, group.name, pre.measure.name, post.measure.name) {
  
    nk <- Reduce("+", abcd)
  
  bk <- abcd$TF
  ck <- abcd$FT
  
  X2v <- sum( (1/nk)*(bk-ck) )^2/sum(((bk - ck) / nk)^2)
  
  X2v
}