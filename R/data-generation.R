generate.clusters <- function(num.clust, clust.size, icc) {
  
}

generate.cluster <- function(clust.size, correlation) {
  
}

# library(clusterGeneration)
# #genRandomClust(numClust = 5, outputDatFlag = FALSE, outputLogFlag = FALSE)
# genRandomClust(numClust = 5, numReplicate = 1, clustszind = 2, rangeN = c(2,4))
# rcorrmatrix(3,alphad=1)
# 
# 
# library(clusterSim)
# cluster.Gen(numObjects=5, model=3, dataType = "o", numCategories=2)
# 
# 
# help("genRandomClust")
# 
# 
# x <- seq(0, 1, length = 21)
# dbeta(x, 1, 1)
# pbeta(x, 1, 1)
# beta(c(0,1), c(0,1))

# source("https://bioconductor.org/biocLite.R")
# biocLite("Biobase")
# install.packages("TailRank", repos="http://R-Forge.R-project.org")

# library('TailRank')

# w <- 10
# u <- 0.3*w
# v <- 0.7*w
# N <- 12
# # generate random values from the beta-binomial
# x <- rbb(1000, N, u, v)

# install.packages('emdbook')
# library(emdbook)
# rbetabinom(n = 8, prob = 0.9, size = 1, theta = 4)#, shape1, shape2)
# dbetabinom(x = c(8, 7, 6), prob = 0.9, size = 1, theta = 4)#, shape1, shape2)
# # dbetabinom(x = c(10, 9, 8), prob = c(0.5, 0.1, 0.9), size = c(1, 1, 1), theta = c(4, 4, 4))#, shape1, shape2)
# dbetabinom(0:5,shape1=5,shape2=5,size=5)




# install.packages("rmutil")
# library("rmutil")
# rbetabinom(n = 5, size = 10, m = c(0.8, 0.3))#, s = c(0.1, 0.2))
# rbetabinom(10,10,0.5,1.1)
# sum(dbetabinom(46:54, 100, 0.5, 1.1))
# devtools::install_git("https://github.com/cran/rmutil")
# 
# install.packages("VGAM")
# library(VGAM)
# library(Betabinom)
# rbetabinom(n, size, prob, rho = 0)
# rbetabinom.ab(n = 10000, size = N, shape1 = s1, shape2 = s2)
# rbetabinom(10, 1, prob = 0.5)
# rbinom(10, 1, prob = 0.5)

# mean(rbeta(1000000, shape1 = .2, shape2 = .2))
# 
# help(gl)

## generate clustered data
# gendat <- function(ncl, clsz) {
#   ## ncl: number of clusters
#   ## clsz: cluster size (all equal)
#   id <- rep(1:ncl, each = clsz)
#   visit <- rep(1:clsz, ncl)
#   n <- ncl * clsz
#   x1 <- rbinom(n, 1, 0.5) ## within cluster varying binary covariate
#   x2 <- runif(n, 0, 1) ## within cluster varying continuous covariate
#   ## the true correlation coefficient rho for an ar(1)
#   ## correlation structure is 2/3
#   rho <- 2/3 # [Pearson correlation coefficient?]
#   rhomat <- rho ^ outer(1:4, 1:4, function(x, y) abs(x - y))
#   chol.u <- chol(rhomat) # [matrix decomposition for efficiency]
#   noise <- as.vector(sapply(1:ncl, function(x) chol.u %*% rnorm(clsz)))
#   y <- 1 + 3 * x1 - 2 * x2 + noise
#   dat <- data.frame(y, id, visit, x1, x2)
#   dat
# }
# gendat(100, 4)


# n <- 1e4
# 
# myData   <- rnorm(n, 5, sd=1)
# yourData <- myData  + rnorm(n, 8, .25)
# hisData  <- myData  + rnorm(n, 6, .4)
# herData  <- myData  + rnorm(n, 5, .6)
# 
# ourData <- data.frame(myData, yourData, hisData, herData)
# 
# cov(ourData)
# 
# myDraws <- mvrnorm(1e5, mu=mean(ourData),
#                    Sigma=cov(ourData)
# )
# myDraws <- data.frame(myDraws)

cor.mat <- rbind(
  c(A1.A1),
  c(B1.A1, B1.B1),
  c(C1.A1, C1.B1, C1.C1),
  c(A2.A1, A2.B1, A2.C1, A2.A2),
  c(B2.A1, B2.B1, B2.C1, B2.A2, B2.B2),
  c(C2.A1, C2.B1, C2.C1, C2.A2, C2.B2, C2.C2),
)

cor.mat <- rbind(
  c(A1.A1),
  c(r1   , B1.B1),
  c(r1   , r1    , C1.C1),
  c(r3   , r4   , r4   , A2.A2),
  c(r4   , r3   , r4   , r2   , B2.B2),
  c(r4   , r4   , r3   , r2   , r2   , C2.C2),
)

s<-matrix(cor.mat, nrow=2, ncol=2)
s[upper.tri(s)] = t(s)[upper.tri(s)]



