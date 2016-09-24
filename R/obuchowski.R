# An implementation of Obuchowski 1998, using of Yang 2010's simplification

#' @export
obuchowski.test <- function(ak, bk, ck, dk, alternative = "2") {
  nk <- ak+bk+ck+dk
  
  ifelse(alternative == "1", 
    .obuchowski.impl1(ak, bk, ck, dk),
    .obuchowski.impl2(nk, bk, ck))
}

.obuchowski.impl2 <- function (nk, bk, ck) {
  K <- length(nk)

  ((K - 1) / K) * sum(bk - ck)^2 / sum((bk - ck)^2)
}

.obuchowski.impl1 <- function(ak, bk, ck, dk) {

  x1 <- ak + bk
  x2 <- ak + ck
  m <- length(ak)
  nk <- ak+bk+ck+dk
  
  # Eqn (1)
  p.hat1 <- sum(x1) / sum(nk)
  p.hat2 <- sum(x2) / sum(nk)
  
  # Eqn (2)
  var.hat1 <- m * ((m - 1)^-1) * ( sum((x1 - nk*p.hat1)^2) ) / (sum(nk) ^ 2)
  var.hat2 <- m * ((m - 1)^-1) * ( sum((x2 - nk*p.hat2)^2) ) / (sum(nk) ^ 2)
  
  # Eqn (3)
  cov.hat <- m * ((m - 1)^-1) * ( sum( (x1 - nk*p.hat1)*(x2 - nk*p.hat2) ) ) / (sum(nk) ^ 2)
  
  # Eqn (4)
  var.hat.diff <- var.hat1 + var.hat2 - 2 * cov.hat
  
  # Eqn (5)
  (p.hat2 - 1) * ((cov.hat) ^ -1) * (p.hat1 - 1)
}


.obuchowski.impl1.orig <- function(ak, bk, ck, dk) {
  x1 <- ak + bk
  x2 <- ak + ck
  m <- length(ak)
  nk <- ak+bk+ck+dk
  # N <- sum(nk)
  
  # Eqn (1)
  p.hat <- function(x, nk) sum(x) / sum(nk)
  
  # Eqn (2)
  var.hat <- function(x, nk, m)
    m * (m-1)^-1 * sum( (x - (nk * p.hat(x, nk)))^2 ) / (sum(nk)^2)
  #var.hat1 <- (K / (K - 1)) * ( (1 / N^2) * sum( (x1k - (nk * p.bar))^2 ) )
  
  # (x   - (nk * p.hat(x, N)))^2
  # (x1k - (nk * p.bar))^2
  
  # Eqn (3)
  cov.hat <- function(x, x1, nk, m)
    m * (m-1)^-1 * sum( (x - (nk * p.hat(x, nk))) * (x1 - (nk * p.hat(x1, nk))) ) / (sum(nk)^2)
  
  # Eqn (4)
  var.hat.diff <- function(x, x1, nk, m)
    var.hat(x, nk, m) + var.hat(x1, nk, m) - 2 * cov.hat(x, x1, nk, m)
  
  # print("p.hat1")
  # str(p.hat(x1, N))
  # print("p.hat2")
  # str(p.hat(x2, N))
  # print("var.hat.diff")
  # str(var.hat.diff(x1, x2, N, m))
  # print("var.hat.x1")
  # str(var.hat(x1, N, m))
  # print("m * (m-1)^-1")
  # str(m * (m-1)^-1)
  # print("(sum(N)^2)")
  # str((sum(N)^2))
  # print("sum( (x - (N * p.hat(x, N)))^2 )")
  # str(sum( (x1 - (N * p.hat(x1, N)))^2 ))
  # print("p.hat(x1, N)")
  # str(p.hat(x1, N))
  print("sum( (x - (nk * p.hat(x, N)))^2 )")
  str(sum( (x1 - (nk * p.hat(x1, nk)))^2 ))
  
  
  
  # Eqn (6)
  ((p.hat(x1, nk) - p.hat(x2, nk))^2) / var.hat.diff(x1, x2, nk, m)
}

.obuchowski.impl.yang <- function (ak, bk, ck, dk) {
  nk <- ak + bk + ck + dk
  N <- sum(nk)
  K <- length(ak)
  
  x1k <- ak + bk
  x2k <- ak + ck
  
  p.hat1 <- (1/N) * sum(ak + bk)
  p2.hat <- (1/N) * sum(ak + ck)
  
  assertthat::assert_that(assertthat::are_equal(p1.hat, sum(x1k) / sum(nk)))
  assertthat::assert_that(assertthat::are_equal(p2.hat, sum(x2k) / sum(nk)))
  
  p.bar <- (p1.hat + p2.hat) / 2
  
  assertthat::assert_that(assertthat::are_equal(p.bar, (1/(2*N)) * sum(2*ak + bk + ck) ))
  
  p1.tilde <- (1 / N) * sum(bk)
  p2.tilde <- (1 / N) * sum(ck)
  p.tilde <- (1 / (2 * N)) * sum(bk + ck)
  
  assertthat::assert_that(assertthat::are_equal( p1.hat - p2.hat, p1.tilde - p2.tilde) )
  assertthat::assert_that(assertthat::are_equal( p1.hat - p2.hat, (1 / N) * sum(bk - ck) ))
  
  cov.hat <- (K / (K - 1)) * ( (1 / N^2) * sum((x1k - (nk * p.bar)) * (x2k - (nk * p.bar))) )
  
  var.hat1 <- (K / (K - 1)) * ( (1 / N^2) * sum( (x1k - (nk * p.bar))^2 ) )
  var.hat2 <- (K / (K - 1)) * ( (1 / N^2) * sum( (x2k - (nk * p.bar))^2 ) )
  
  var.hat.diff <- var.hat1 + var.hat2 - 2*cov.hat
  
  X2o.original <- (p1.hat - p2.hat)^2 / var.hat.diff
  X2o.simplified <- ((K -1) / K) * sum(bk - ck)^2 / sum((bk - ck)^2)
  
  assertthat::assert_that(assertthat::are_equal(X2o.simplified, X2o.original))
  
  # print("p.hat1") # Good
  # str(p1.hat)
  # print("p.hat2") # Good
  # str(p2.hat)
  # print("var.hat.diff") # Bad
  # str(var.hat.diff)
  # print("var.hat.x1") # Bad
  # str(var.hat1)
  # print("(K / (K - 1))") # Good
  # str((K / (K - 1)))
  # print("(N^2)") # Good
  # str((N^2))
  print("sum( (x1k - (nk * p.bar))^2 )") # Bad
  str(sum( (x1k - (nk * p.bar))^2 ))
  # print("p.bar")
  # str(p.bar)
  
  X2o.simplified
}

.obuchowski.impl1(tc$ak, tc$bk, tc$ck, tc$dk)
.obuchowski.impl.yang(tc$ak, tc$bk, tc$ck, tc$dk)