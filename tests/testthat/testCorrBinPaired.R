library(CorrBinPaired)

context("Top Level Functions")

test_that("durkalski and obuchowski report similar values", {
  group.names <- c('subject', 'atom')
  pre.measure.name <- 'control'
  post.measure.name <- 'treatment'
  
  durkalski.chi2 <- durkalski.test(confusion, group.names, pre.measure.name, post.measure.name)
  obuchowski.chi2 <- obuchowski.test(confusion, group.names, pre.measure.name, post.measure.name)
  
  chi2.diff <- abs(durkalski.chi2 - obuchowski.chi2) / durkalski.chi2
  
  expect_lt(chi2.diff, .2, "Durkalski and Obuchowski are relatively close")
})