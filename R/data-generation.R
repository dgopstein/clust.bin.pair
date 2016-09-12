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

same.length <- function(...) {
  lens <- sapply(list(...), length)
  all(lens == lens[1])
}

generate.clusters <- function(K, nk, p1s, p2s, r1s, r2s, r3s, r4s) {
  if (!same.length(p1s, p2s)) stop("p1s and p2s must be of the same length")
  if (!same.length(r1s, r2s, r3s, r4s)) stop("r1s, r2s, r3s, and r4s must be of the same length")
  
  grid <- expand.grid(1:length(p1s), 1:length(r1s))
  
  params.grid <- cbind(K=K, nk=nk, p1 = p1s[grid[[1]]], p2 = p2s[grid[[1]]],
                       r1 = r1s[grid[[2]]], r2 = r2s[grid[[2]]],
                       r3 = r3s[grid[[2]]], r4 = r4s[grid[[2]]])
  
  apply(params.grid, 1, function(params) {do.call(generate.cluster, as.list(params))})
}
  

generate.cluster <- function(K, nk, p1, p2, r1, r2, r3, r4) {
  n2 <- nk*2
  
  cor.mat <- cor.structure(n2, r1, r2, r3, r4)
  
  # as long as the standard deviation of each element is 1
  # the correlation and covariance matrices are identical
  cov.mat <- cor.mat
  
  # generate random data with the correlations defined by the r-variables
  rtrials <- mvrnorm(K, mu=rep(0, ncol(cov.mat)), Sigma=cov.mat)
  
  # coerce each variable to binary with success probability p
  dtrials <- cbind(dichotomize(rtrials[,1:nk], p1), dichotomize(rtrials[,(nk+1):n2], p2))
  
  # add group names
  gtrials <- data.frame(cbind(cluster = 1:nrow(dtrials)), dtrials)
  
  # convert the data into matched-pair format
  trials <- reshape(gtrials, direction="long",
          idvar = "cluster",
          varying = list(2:(1+nk), (2+nk):(n2+1))
          ,v.names = c("t1", "t2")
          , times = 1:(nk)
          )
  
  trials
}


apply.tests <- function (clusters) {
  tests <- c(.mcnemar.wrapper, eliasziw.test, obuchowski.test, durkalski.test, yang.test)
  res <- sapply(tests, function(t) do.call(t,  list(clusters, "cluster", "t1", "t2")))
  names(res) <- c("mcnemar", "eliasziw", "obuchowski", "durkalski", "yang")
  res
}

empirical.power <- function(chi2s) length(which(chi2s > qchisq(.95, df=1))) / length(chi2s)

flatten <- function(x) {
  y <- list()
  lapply(x, function(x) y <<- c(y,x))
  y
}

# Yang scenario 1 params
K <- 15; nk <- 2; p1k <- p2k <- seq(from=0.05, by=0.05, to=0.85); r1 <- r2 <- c(0.1, 0.4, 0.8); r3 <- 0.5; r4 <- r1 / 2

# Other parameters of Yang trials

# Size of cluster
nk.funs <- c(function() 2,
             function() sample(1:5, 1),
             function() sample(1:10, 1))

K <- c(15, 25, 50, 100) # Number of clusters

# scenario 2: Values vary every K/3 of the clusters
pi1s <- seq(from=0.05, by=0.05, length.out=9)
pi2s <- seq(from=0.15, by=0.05, length.out=9)
pi3s <- seq(from=0.45, by=0.05, length.out=9)
p1k2 <- p1k
p2k2 <- p1k2 + 0.10

# datasets <- flatten(replicate(20, generate.clusters(K=15, nk=2, p1=p1k, p2=p2k, r1=r1, r2=r2, r3=r3, r4=r4), simplify=FALSE))
# system.time(chisq.statistics <- sapply(datasets, apply.tests))
# apply(chisq.statistics, 1, empirical.power)

