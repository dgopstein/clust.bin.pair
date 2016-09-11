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
  cutpoint <- qnorm(p1)
  (values < cutpoint) * 1
}

# r4 <- r3 <- 0.5 * (r2 <- r1 <- 0.9)
nk <- 8 # cluster size
p1 <- 0.5 # probability of getting a success (cutpoint)
p2 <- 0.6

generate.clusters <- function(K, nk, p1s, p2s, r1s, r2s, r3s, r4s) {
  ps <- cbind(p1 = p1s, p2 = p2s)
  rs <- cbind(r1 = r1s, r2 = r2s, r3 = r3s, r4 = r4s)
  
  grid <- expand.grid(1:nrow(ps), 1:nrow(rs))
  
  params.grid <- cbind(K=K, nk=nk, ps[grid[[1]],], rs[grid[[2]],])
  
  apply(params.grid, 1, function(params) do.call(generate.cluster, as.list(params)))
}
  

generate.cluster <- function(K, nk, p1, p2, r1, r2, r3, r4) {
  n2 <- nk/2
  
  cor.mat <- cor.structure(nk, r1, r2, r3, r4)
  
  # as long as the standard deviation of each element is 1
  # the correlation and covariance matrices are identical
  cov.mat <- cor.mat
  
  # generate random data with the correlations defined by the r-variables
  rtrials <- mvrnorm(K, mu=rep(0, ncol(cov.mat)), Sigma=cov.mat)
  
  # coerce each variable to binary with success probability p
  dtrials <- cbind(dichotomize(rtrials[,1:n2], p1), dichotomize(rtrials[,(n2+1):nk], p2))
  
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


apply.tests <- function (clusters) {
  tests <- c(mcnemar.wrapper, eliasziw.test, obuchowski.test, durkalski.test, yang.test)
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

#datasets <- replicate(10, generate.clusters(K=15, nk=2, p1=0.4, p2=0.7, r1=.9, r2=.9, r3=.4, r4=.4), simplify=FALSE)
datasets <- flatten(replicate(20, generate.clusters(K=15, nk=2, p1=p1k, p2=p2k, r1=r1, r2=r2, r3=r3, r4=r4), simplify=FALSE))
system.time(chisq.statistics <- sapply(datasets, apply.tests))
apply(chisq.statistics, 1, empirical.power)

# Yang scenario 1 params
K <- 15; nk <- 2; p1k <- p2k <- seq(from=0.05, by=0.05, to=0.85); r1 <- r2 <- c(0.1, 0.4, 0.8); r3 <- 0.5; r4 <- r1 / 2


# Parameters of Yang trials

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

# scenario 2: Values vary every K/3 of the clusters
pi1s <- seq(from=0.05, by=0.05, length.out=9)
pi2s <- seq(from=0.15, by=0.05, length.out=9)
pi3s <- seq(from=0.45, by=0.05, length.out=9)
p2s <- p1s + 0.10


