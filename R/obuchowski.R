# An implementation of Obuchowski 1998

obuchowski.test <- function(x, group.names, pre.measure.name, post.measure.name) {
  z <- results.to.contingency.cols(x, group.names, pre.measure.name, post.measure.name)
  obuchowski.impl(z$bk, z$ck, z$nk, nrow(z))
}

x1 <- z$bk
x2 <- z$ck
N  <- z$nk
m  <- nrow(z)

obuchowski.impl <- function(x1, x2, N, m) {
  # Eqn (1)
  p.hat <- function(x, N) sum(x) / sum(N)
  
  # Eqn (2)
  var.hat <- function(x, N, m)
    m * (m-1)^-1 * sum( (x - (N * p.hat(x, N)))^2 ) / sum(N)^2
  
  # Eqn (3)
  cov.hat <- function(x, x1, N, m)
    m * (m-1)^-1 * sum( (x - (N * p.hat(x, N))) * (x1 - (N * p.hat(x1, N))) ) / sum(N)^2
  
  # Eqn (4)
  var.hat.diff <- function(x, x1, N, m)
    var.hat(x, N, m) + var.hat(x1, N, m) - 2 * cov.hat(x, x1, N, m)
  
  # Eqn (6)
  X2o.1 <- ((p.hat(x1, N) - p.hat(x2, N))^2) / var.hat.diff(x1, x2, N, m)
  
  # Yang Equivalency
  # K <- m
  # bk <- x1 / N
  # ck <- x2 / N
  # X2o.2 <- ((K - 1) / K) * (sum(bk - ck)^2) / sum((bk - ck)^2)
  
  X2o.1
}