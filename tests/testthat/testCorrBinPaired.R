library(CorrBinPaired)

context("Top Level Functions")

group.names <- c('subject', 'atom')
pre.measure.name <- 'control'
post.measure.name <- 'treatment'

abcd.data <- results.to.contingency.cols(confusion, group.names, pre.measure.name, post.measure.name)

test_that("McNemar scores datasets correctly", { 
  expect_equal(.mcnemar(disagreements$bh, disagreements$ch), 11.85, tolerance=.1, scale=NULL, "disagreements")
}) 

test_that("clustered methods are same order as McNemar", {
  mcnemar.chi2 <- .mcnemar(abcd.data$bk, abcd.data$ck)
  durkalski.chi2  <-  durkalski.test(confusion, group.names, pre.measure.name, post.measure.name)
  obuchowski.chi2 <- obuchowski.test(confusion, group.names, pre.measure.name, post.measure.name)
  eliasziw.chi2   <-   eliasziw.test(confusion, group.names, pre.measure.name, post.measure.name)
  yang.chi2       <-       yang.test(confusion, group.names, pre.measure.name, post.measure.name)

  expect_equal(mcnemar.chi2, durkalski.chi2,  tolerance = .2, scale = NULL, "Durkalski and McNemar are relatively close")
  expect_equal(mcnemar.chi2, obuchowski.chi2, tolerance = .2, scale = NULL, "Obuchowski and McNemar are relatively close")
  expect_equal(mcnemar.chi2, eliasziw.chi2,   tolerance = .2, scale = NULL, "Eliasziw and McNemar are relatively close")
  expect_equal(mcnemar.chi2, yang.chi2,       tolerance = .2, scale = NULL, "Yang and McNemar are relatively close")
})

test_that("durkalski works with various input", {
  durkalski.res <- 10.125
  
  durkalski.chi2.ungrouped <- durkalski.test(confusion, group.names, pre.measure.name, post.measure.name)
  durkalski.chi2.abcd      <- durkalski.impl(abcd.data$nk, abcd.data$bk, abcd.data$ck)
  
  expect_equal(durkalski.chi2.ungrouped, durkalski.res, tolerance = 0.01, scale = NULL, "ungrouped column input")
  expect_equal(durkalski.chi2.abcd,      durkalski.res, tolerance = 0.01, scale = NULL, "abcd column input")
})

test_that("obuchowski works with various input", {
  obuchowski.res <- 9.627329
  
  obuchowski.chi2.ungrouped <- obuchowski.test(confusion, group.names, pre.measure.name, post.measure.name)
  obuchowski.chi2.abcd      <- obuchowski.impl(abcd.data$nk, abcd.data$bk, abcd.data$ck)
  
  expect_equal(obuchowski.chi2.ungrouped, obuchowski.res, tolerance = 0.01, scale = NULL, "ungrouped column input")
  expect_equal(obuchowski.chi2.abcd,      obuchowski.res, tolerance = 0.01, scale = NULL, "abcd column input")
})

test_that("eliasziw works with various input", {
  eliasziw.res <- 8.757071

  eliasziw.chi2.ungrouped <- eliasziw.test(confusion, group.names, pre.measure.name, post.measure.name)
  eliasziw.chi2.abcd      <- eliasziw.impl(abcd.data[,c("ak", "bk", "ck", "dk")])

  expect_equal(eliasziw.chi2.ungrouped, eliasziw.res, tolerance = 0.01, scale = NULL, "ungrouped column input")
  expect_equal(eliasziw.chi2.abcd,      eliasziw.res, tolerance = 0.01, scale = NULL, "abcd column input")
})

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