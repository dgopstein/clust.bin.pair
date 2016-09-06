# An implementation of Obuchowski 1998

obuchowski.test <- function(x, group.names, pre.measure.name, post.measure.name) {
  z <- results.to.contingency.cols(x, group.names, pre.measure.name, post.measure.name)
  obuchowski.impl(z$ak + z$bk, z$ak + z$ck, z$nk)
}

# x1 <- z$ak + z$bk
# x2 <- z$ak + z$ck
# N <- z$nk

obuchowski.impl <- function(x1, x2, N) {
  m <- length(x1)

  # Eqn (1)
  p.hat <- function(x, N) sum(x) / sum(N)
  
  # Eqn (2)
  var.hat <- function(x, N, m)
    m * (m-1)^-1 * sum( (x - (N * p.hat(x, N)))^2 ) / sum(N)^2
  
  # # Eqn (3)
  # cov.hat <- function(x1, x2, N, m)
  #   m * (m-1)^-1 * sum( (x1 - (N * p.hat(x1, N))) * (x2 - (N * p.hat(x2, N))) ) / sum(N)^2
  
  p.bar <- function(x1, x2, N)
    (p.hat(x1, N) + p.hat(x2, N)) / 2
  
  # Eqn (7)
  cov.hat.p.bar <- function (x1, x2, N, m)
    m * (m-1)^-1 * sum( (x1 - (N * p.bar(x1, x2, N))) * (x2 - (N * p.bar(x1, x2, N))) ) / sum(N)^2
  
  # Eqn (4)
  var.hat.diff <- function(x1, x2, N, m)
    var.hat(x1, N, m) + var.hat(x2, N, m) - 2 * cov.hat.p.bar(x1, x2, N, m)
  
  # Eqn (6)
  ((p.hat(x1, N) - p.hat(x2, N))^2) / var.hat.diff(x1, x2, N, m)
}
