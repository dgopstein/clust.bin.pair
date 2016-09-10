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

# cor.structure <- function(nk, r1, r2, r3, r4) {
#   paste(lower.to.symmetric(suppressWarnings(rbind(
#     c(1),
#     c(r1   , 1),
#     c(r1   , r1   , 1),
#     c(r3   , r4   , r4   , 1),
#     c(r4   , r3   , r4   , r2   , 1),
#     c(r4   , r4   , r3   , r2   , r2   , 1)
#   ))), collapse=",")
# }
# r1 <- 10
# paste(c(lower.to.symmetric(suppressWarnings(rbind(
#   c(1),
#   c(r1   , 1),
#   c(r1   , r1   , 1),
#   c(r1   , r1   , r1   , 1),
#   c(r3   , r4   , r4   , r4   , 1),
#   c(r4   , r3   , r4   , r4   , r2   , 1),
#   c(r4   , r4   , r3   , r4   , r2   , r2   , 1),
#   c(r4   , r4   , r4   , r3   , r2   , r2   , r2   , 1)
# )))), collapse=",")

cor.structure <- function(nk, r1, r2, r3, r4) {
  n2 <- nk/2
  m <- matrix(0, nrow = nk, ncol=nk)
  for (i in 1:nk) {
    for (j in 1:nk) {
      v <-
        if (i == j) 1
        else if (i<=n2 & j<=n2) r1
        else if (i>n2 & j>n2) r2
        else if (i-n2 == j) r3
        else { r4 }
      m[i, j] <- v
    }
  }

  lower.to.symmetric(m)
}


nk.funs <- c(function() 2,
             function() sample(1:5, 1),
             function() sample(1:10, 1))

#cor.structure(8, 1, 2, 3, 4)
# r4 <- r3 <- 0.5 * (r2 <- r1 <- 0.9)
r1.r2 <- c(0.0, 0.1, 0.4, 0.8)
r3.r4 <- (1/2) * r1.r2
nks <- sapply(rep(nk.funs, length(r1.r2)), function(f) do.call(f, list()))
cor.mats <- mapply(cor.structure, nks, r1.r2, r1.r2, r3.r4, r3.r4, SIMPLIFY = FALSE) 

# as long as the standard deviation of each element is 1
# the correlation and covariance matrices are identical
cov.mats <- cor.mats

rtrials <- mvrnorm(1e4, mu=rep(0, ncol(cov.mats[[1]])), Sigma=cov.mats[[4]])

dichotomize <- function(values, p) {
  cutpoint <- qnorm(p)
  (values < cutpoint) * 1
}



# trials <- data.table(dichotomize(rtrials, 0.5))
# trials[, id := .I]
# 
# trials.ungrouped <- melt(trials, id.vars = c("id"))
# trials.ungrouped[, group := as.factor(paste(id, ifelse(variable %in% c("V1", "V2", "V3"), 1, 2)))]
# 
# 
# system.time(icc <- ICCbare(trials.ungrouped$group, trials.ungrouped$value))
# icc

K <- c(15, 25, 50, 100) # Number of clusters

# values from Yang 2010
# scenario 1: Values constant across all clusters
p1s <- seq(from=0.05, by=0.05, to=0.85)
p2s <- p1s + 0.10

# scenario 2: Values vary every K/3 of the clusters
pi1s <- seq(from=0.05, by=0.05, length.out=9)
pi2s <- seq(from=0.15, by=0.05, length.out=9)
pi3s <- seq(from=0.45, by=0.05, length.out=9)

empirical.power <- function(chi2s) length(which(chi2s > 3.84)) / length(chi2s)


