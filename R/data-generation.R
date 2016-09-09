generate.clusters <- function(num.clust, clust.size, icc) {
  
}

generate.cluster <- function(clust.size, correlation) {
  
}

r1.r2 <- c(0.0, 0.1, 0.4, 0.8)
r3.r4 <- (1/2) * r1.r2

r4 <- r3 <- 0.5 * (r2 <- r1 <- 0.4)

lower.to.symmetric <- function(m) {
  m[upper.tri(m)] = t(m)[upper.tri(m)]
  m
}

cor.structure <- function(r1, r2, r3, r4) {
  lower.to.symmetric(suppressWarnings(rbind(
    c(1),
    c(r1   , 1),
    c(r1   , r1   , 1),
    c(r3   , r4   , r4   , 1),
    c(r4   , r3   , r4   , r2   , 1),
    c(r4   , r4   , r3   , r2   , r2   , 1)
  )))
}

cor.mats <- mapply(cor.structure, r1.r2, r1.r2, r3.r4, r3.r4, SIMPLIFY = FALSE) 

# as long as the standard deviation of each element is 1
# the correlation and covariance matrices are identical
cov.mats <- cor.mats

library(MASS)

mvrnorm(1e5, mu=rep(0, ncol(cov.mats[[1]])), Sigma=cov.mats[[1]])

