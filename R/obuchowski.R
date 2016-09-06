# An implementation of Obuchowski 1998

obuchowski.test <- function(x, group.names, pre.measure.name, post.measure.name) {
  z <- results.to.contingency.cols(x, group.names, pre.measure.name, post.measure.name)
  obuchowski.impl(z$ak + z$bk, z$ak + z$ck, z$nk)
}

obuchowski.impl <- function(x1, x2, N) {
  m <- length(x1)

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
  ((p.hat(x1, N) - p.hat(x2, N))^2) / var.hat.diff(x1, x2, N, m)
}