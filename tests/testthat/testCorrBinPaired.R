library(CorrBinPaired)

context("Top Level Functions")

group.names <- c('subject', 'atom')
pre.measure.name <- 'control'
post.measure.name <- 'treatment'

abcd.data <- results.to.contingency.cols(confusion, group.names, pre.measure.name, post.measure.name)

test_that("durkalski and obuchowski report similar values", {
  durkalski.chi2 <- durkalski.test(confusion, group.names, pre.measure.name, post.measure.name)
  obuchowski.chi2 <- obuchowski.test(confusion, group.names, pre.measure.name, post.measure.name)
  
  chi2.diff <- abs(durkalski.chi2 - obuchowski.chi2) / durkalski.chi2
  
  expect_lt(chi2.diff, .2, "Durkalski and Obuchowski are relatively close")
})

test_that("durkalski works with various input", {
  durkalski.res <- 10.125
  
  durkalski.chi2.ungrouped <- durkalski.test(confusion, group.names, pre.measure.name, post.measure.name)
  durkalski.chi2.abcd      <- durkalski.test(confusion, group.names, pre.measure.name, post.measure.name)
  
  expect_equal(durkalski.chi2.ungrouped, durkalski.res, tolerance = 0.01, scale = 1, "ungrouped column input")
  expect_equal(durkalski.chi2.abcd,      durkalski.res, tolerance = 0.01, scale = 1, "abcd column input")
})

test_that("obuchowski works with various input", {
  obuchowski.res <- 11.27761
  
  obuchowski.chi2.ungrouped <- obuchowski.test(confusion, group.names, pre.measure.name, post.measure.name)
  obuchowski.chi2.abcd      <- obuchowski.test(confusion, group.names, pre.measure.name, post.measure.name)
  
  expect_equal(obuchowski.chi2.ungrouped, obuchowski.res, tolerance = 0.01, scale = 1, "ungrouped column input")
  expect_equal(obuchowski.chi2.abcd,      obuchowski.res, tolerance = 0.01, scale = 1, "abcd column input")
})