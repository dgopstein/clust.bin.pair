library(MASS)
library(data.table)
library(ICC)

generate.clusters <- function(num.clust, clust.size, icc) {
  
}

generate.cluster <- function(clust.size, correlation) {
  
}





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

# r4 <- r3 <- 0.5 * (r2 <- r1 <- 0.9)
r1.r2 <- c(0.0, 0.1, 0.4, 0.8)
r3.r4 <- (1/2) * r1.r2
cor.mats <- mapply(cor.structure, r1.r2, r1.r2, r3.r4, r3.r4, SIMPLIFY = FALSE) 

# as long as the standard deviation of each element is 1
# the correlation and covariance matrices are identical
cov.mats <- cor.mats


rtrials <- mvrnorm(1e4, mu=rep(0, ncol(cov.mats[[1]])), Sigma=cov.mats[[3]])

dichotomize <- function(values, p) {
  cutpoint <- qnorm(p)
  (values < cutpoint) * 1
}



trials <- data.table(dichotomize(rtrials, 0.05))
trials[, id := .I]

trials.ungrouped <- melt(trials, id.vars = c("id"))
trials.ungrouped[, group := as.factor(paste(id, ifelse(variable %in% c("V1", "V2", "V3"), 1, 2)))]


system.time(icc <- ICCbare(trials.ungrouped$group, trials.ungrouped$value))
icc

K <- c(15, 25, 50, 100) # Number of clusters

# values from Yang 2010
# scenario 1: Values constant across all clusters
p1s <- seq(from=0.05, by=0.05, to=0.85)
p2s <- p1s + 0.10

# scenario 2: Values vary every K/3 of the clusters
pi1s <- seq(from=0.05, by=0.05, length.out=9)
pi2s <- seq(from=0.15, by=0.05, length.out=9)
pi3s <- seq(from=0.45, by=0.05, length.out=9)

empirical.power <- function(chi2s) length(which(chi2s > 3.84))

nks <- c(function() 2, function() floor(runif(n=1, min=1, max=5.9999)), function() floor(runif(n=1, min=1, max=10.9999)))