# An implementation of Yang et al. 2010

.yang.test <- function(ak, bk, ck, dk)
  .yang.impl(ak+bk+ck+dk, bk, ck)

.yang.impl <- function(nk, bk, ck) {
  N <- sum(nk)
  K <- length(nk)
  
  p1.tilde <- (1 / N) * sum(bk)
  p2.tilde <- (1 / N) * sum(ck)
  
  X2mo <-
    ((K - 1) / K) *
      sum(bk - ck)^2 /
        ( (1/2) * sum( ((bk - ck) - nk*(p1.tilde - p2.tilde))^2  + (bk - ck)^2 ) )
  
  X2mo
}