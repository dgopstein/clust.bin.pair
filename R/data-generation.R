library(MASS)
library(data.table)
library(ICC)


lower.to.symmetric <- function(m) {
  m[upper.tri(m)] = t(m)[upper.tri(m)]
  m
}

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

# Take floating point values, normally distributed about 0 and convert
# them to 1/0 with p being the probability of the value being 1
dichotomize <- function(values, p) {
  cutpoint <- qnorm(p)
  (values < cutpoint) * 1
}

# r4 <- r3 <- 0.5 * (r2 <- r1 <- 0.9)
nk <- 8 # cluster size
p <- 0.45 # probability of getting a success (cutpoint)
generate.clusters <- function(K, nk, p, r1, r2, r3, r4) {
  cor.mat <- cor.structure(nk, r1, r2, r3, r4)
  
  # as long as the standard deviation of each element is 1
  # the correlation and covariance matrices are identical
  cov.mat <- cor.mat
  
  # generate random data with the correlations defined by the r-variables
  rtrials <- mvrnorm(K, mu=rep(0, ncol(cov.mat)), Sigma=cov.mat)
  
  # coerce each variable to binary with success probability p
  dtrials <- dichotomize(rtrials, p)
  
  # add group names
  gtrials <- data.frame(cbind(cluster = 1:nrow(dtrials)), dtrials)
  
  # convert the data into matched-pair format
  trials <- reshape(gtrials, direction="long",
          idvar = "cluster",
          varying = list(2:(1+nk/2), (2+nk/2):(nk+1))
          ,v.names = c("t1", "t2")
          , times = 1:(nk/2)
          )
  
  trials
}

clusters <- generate.clusters(K=1e4, nk=6, p=0.5, r1=.9, r2=.9, r3=.4, r4=.4)

cnt <- results.to.contingency.cols(x = clusters, group.names = "cluster", pre.measure.name = "t1", post.measure.name = "t2")
bk <- cnt$bk
ck <- cnt$ck
nk <- cnt$nk
mcnemar(bk, ck)
eliasziw.test(clusters, "cluster", "t1", "t2")
obuchowski.test(clusters, "cluster", "t1", "t2")
durkalski.test(clusters, "cluster", "t1", "t2")
yang.test(clusters, "cluster", "t1", "t2")


nk.funs <- c(function() 2,
             function() sample(1:5, 1),
             function() sample(1:10, 1))

r1.r2 <- c(0.0, 0.1, 0.4, 0.8)
r3.r4 <- (1/2) * r1.r2
nks <- sapply(rep(nk.funs, length(r1.r2)), function(f) do.call(f, list()))
cor.mats <- mapply(cor.structure, nks, r1.r2, r1.r2, r3.r4, r3.r4, SIMPLIFY = FALSE) 



K <- c(15, 25, 50, 100) # Number of clusters

# values from Yang 2010
# scenario 1: Values constant across all clusters
p1s <- seq(from=0.05, by=0.05, to=0.85)
p2s <- p1s + 0.10

# scenario 2: Values vary every K/3 of the clusters
pi1s <- seq(from=0.05, by=0.05, length.out=9)
pi2s <- seq(from=0.15, by=0.05, length.out=9)
pi3s <- seq(from=0.45, by=0.05, length.out=9)

empirical.power <- function(chi2s) length(which(chi2s > qchisq(.95, df=1))) / length(chi2s)
