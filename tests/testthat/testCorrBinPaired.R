library(CorrBinPaired)

context("Top Level Functions")

group.names <- c('subject', 'atom')
pre.measure.name <- 'control'
post.measure.name <- 'treatment'

confusion.contingencies <- results.to.contingency.cols(confusion, group.names, pre.measure.name, post.measure.name)

test_that("McNemar scores datasets correctly", { 
  expect_equal(.mcnemar.impl(disagreements$bh, disagreements$ch), 11.85, tolerance=.1, scale=NULL, "disagreements")
}) 

apply.tests <- function (x, x.name) {
  tests <- c(.mcnemar.test, eliasziw.test, obuchowski.test, durkalski.test, yang.test)
  res <- sapply(tests, function(t) do.call(t, list(ak=x$ak, bk=x$bk, ck=x$ck, dk=x$dk)))
  names(res) <- c("mcnemar", "eliasziw", "obuchowski", "durkalski", "yang")

  sapply(res, function(chi2) {
    expect_equal(res[["mcnemar"]], chi2,  tolerance = .2, scale = NULL,
                 paste(name(chi2), " and McNemar are relatively close on dataset ", x.name))
  })
}

test_that("All tests work with all datasets", {
  apply.tests(confusion.contingencies, "Confusion")
})
