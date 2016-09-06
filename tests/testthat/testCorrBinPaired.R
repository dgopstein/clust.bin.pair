library(CorrBinPaired)

context("Top Level Functions")

group.names <- c('subject', 'atom')
pre.measure.name <- 'control'
post.measure.name <- 'treatment'

abcd.data <- results.to.contingency.cols(confusion, group.names, pre.measure.name, post.measure.name)

test_that("McNemar scores datasets correctly", { 
  expect_equal(mcnemar(disagreements$bh, disagreements$ch), 11.85, tolerance=.1, scale=NULL, "disagreements")
}) 

test_that("clustered methods are same order as McNemar", {
  mcnemar.chi2 <- mcnemar(abcd.data$bk, abcd.data$ck)
  durkalski.chi2  <-  durkalski.test(confusion, group.names, pre.measure.name, post.measure.name)
  obuchowski.chi2 <- obuchowski.test(confusion, group.names, pre.measure.name, post.measure.name)
  eliasziw.chi2   <-   eliasziw.test(confusion, group.names, pre.measure.name, post.measure.name)
  
  d.diff  <- abs(mcnemar.chi2 - durkalski.chi2 ) / mcnemar.chi2
  o.diff  <- abs(mcnemar.chi2 - obuchowski.chi2) / mcnemar.chi2
  e.diff  <- abs(mcnemar.chi2 - eliasziw.chi2  ) / mcnemar.chi2
  
  expect_lt(d.diff,  .2, "Durkalski and McNemar are relatively close")
  expect_lt(o.diff,  .2, "Obuchowski and McNemar are relatively close")
  expect_lt(e.diff,  .2, "Eliasziw and McNemar are relatively close")
})

test_that("durkalski works with various input", {
  durkalski.res <- 10.125
  
  durkalski.chi2.ungrouped <- durkalski.test(confusion, group.names, pre.measure.name, post.measure.name)
  durkalski.chi2.abcd      <- durkalski.impl(abcd.data$nk, abcd.data$bk, abcd.data$ck)
  
  expect_equal(durkalski.chi2.ungrouped, durkalski.res, tolerance = 0.01, scale = NULL, "ungrouped column input")
  expect_equal(durkalski.chi2.abcd,      durkalski.res, tolerance = 0.01, scale = NULL, "abcd column input")
})

test_that("obuchowski works with various input", {
  obuchowski.res <- 11.27761
  
  obuchowski.chi2.ungrouped <- obuchowski.test(confusion, group.names, pre.measure.name, post.measure.name)
  obuchowski.chi2.abcd      <- obuchowski.impl(abcd.data$ak + abcd.data$bk, abcd.data$ak + abcd.data$ck, abcd.data$nk)
  
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