library(CorrBinPaired)

context("Monte Carlo Simulations")

test_that("Correlation matrix generation works", {
  cor6 <- matrix(c(1,10,10,3,4,4,10,1,10,4,3,4,10,10,1,4,4,3,3,4,4,1,2,2,4,3,4,2,1,2,4,4,3,2,2,1), nrow=6, ncol=6)
  cor8 <- matrix(c(1,10,10,10,3,4,4,4,10,1,10,10,4,3,4,4,10,10,1,10,4,4,3,4,10,10,10,1,4,4,4,3,3,4,4,4,1,2,2,2,4,3,4,4,2,1,2,2,4,4,3,4,2,2,1,2,4,4,4,3,2,2,2,1), nrow=8, ncol=8)
  
  expect_equal(cor6, cor.structure(6, 10, 2, 3, 4))
  expect_equal(cor8, cor.structure(8, 10, 2, 3, 4))
})

test_that("Utility functions works", {
  expect_true(same.length(c(1, 2), c(3, 4)))
  expect_false(same.length(c(1), c(3, 4)))
  expect_false(same.length(c(), c(3)))
  expect_true(same.length(c(), c()))
})

test_that("Data is generated with proper parameters", {
  K <- 15;
  nk <- 2;
  p1k <- p2k <- seq(from=0.05, by=0.05, to=0.85);
  r1s <- r2s <- c(0.1, 0.4, 0.8);
  r3s <- rep(0.5, length(r1s));
  r4s <- r1s / 2
  
  clusters <- generate.clusters(K=K, nk=nk, p1s=p1k, p2s=p2k, r1s=r1s, r2s=r2s, r3s=r3s, r4s=r4s)
  
  n.scenarios <- length(p1k) * length(r1)
  expect_equal(n.scenarios, length(clusters))
  
  n.samples <- K * nk
  expect_equal(n.samples, nrow(clusters[[1]]))
  
  # each cluster has the appropriate icc
  # each cluster is independent from one another
})