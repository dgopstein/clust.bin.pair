# Yang 2010
library(assertthat)

yang.obuchowski.impl <- function (ak, bk, ck, dk) {
  nk <- ak + bk + ck + dk
  N <- sum(nk)
  
  x1k <- ak + bk
  x2k <- ak + ck
  
  p1.hat <- (1/N) * sum(ak + bk)
  p2.hat <- (1/N) * sum(ak + ck)
  
  assert_that(are_equal(p1.hat, sum(x1k) / sum(nk)))
  assert_that(are_equal(p2.hat, sum(x2k) / sum(nk)))
  
  p.bar <- (p1.hat + p2.hat) / 2
  
  assert_that(are_equal(p.bar, (1/(2*N)) * sum(2*ak + bk + ck) ))
  
  p1.tilde <- (1 / N) * sum(bk)
  p2.tilde <- (1 / N) * sum(ck)
  p.tilde <- (1 / (2 * N)) * sum(bk + ck)
  
  assert_that(are_equal( p1.hat - p2.hat, p1.tilde - p2.tilde) )
  assert_that(are_equal( p1.hat - p2.hat, (1 / N) * sum(bk - ck) ))
  
  cov.hat <- (K / (K - 1)) * ( (1 / N^2) * sum((x1k - (nk * p.bar)) * (x2k - (nk * p.bar))) )
  
  var.hat1 <- (K / (K - 1)) * ( (1 / N^2) * sum( (x1k - (nk * p.bar))^2 ) )
  var.hat2 <- (K / (K - 1)) * ( (1 / N^2) * sum( (x2k - (nk * p.bar))^2 ) )
  
  var.hat.diff <- var.hat1 + var.hat2 - 2*cov.hat
  
  X2o.original <- (p1.hat - p2.hat)^2 / var.hat.diff
  X2o.simplified <- ((K -1) / K) * sum(bk - ck)^2 / sum((bk - ck)^2)
  
  assert_that(are_equal(X2o.original, X2o.simplified))
  
  X2o.simplified
}



ak <- abcd$ak
bk <- abcd$bk
ck <- abcd$ck
dk <- abcd$dk


yang.obuchowski.impl(abcd$ak, abcd$bk, abcd$ck, abcd$dk)

