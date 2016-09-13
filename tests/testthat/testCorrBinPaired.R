library(CorrBinPaired)

context("Top Level Functions")

test_that("McNemar scores datasets correctly", { 
  expect_equal(.mcnemar.impl(psychiatry$bh, psychiatry$ch), 11.85, tolerance=.1, scale=NULL, "psychiatry")
}) 

apply.tests <- function (x, x.name) {
  tests <- c(.mcnemar.test, eliasziw.test, obuchowski.test, durkalski.test, yang.test)
  res <- sapply(tests, function(t) do.call(t, list(ak=x$ak, bk=x$bk, ck=x$ck, dk=x$dk)))
  names(res) <- c("mcnemar", "eliasziw", "obuchowski", "durkalski", "yang")

  # All adjusted methods should be less than mcnemars
  # sapply(names(res), function(name) {
  #   expect_gte(res[["mcnemar"]], res[[name]]) #, info = paste(name, "is less than McNemar on dataset", x.name))
  # })
  
  # Skip mcnemars and compare all the rest to their mean
  sapply(names(res[-1]), function(name) {
    expect_equal(mean(res), res[[name]],  tolerance = .3, scale = NULL,
               info = paste(name, "and the mean of the others on dataset", x.name))
  })
}

test_that("All tests work with all datasets", {

  # obfuscation
  obfuscation.contingencies <- paired.to.contingency(obfuscation,
    group.names = c('subject', 'atom'), pre.measure.name = 'control', post.measure.name = 'treatment')
  
  apply.tests(obfuscation.contingencies, "obfuscation")
  
  # psychiatry
  psychiatry.contingencies <- psychiatry[, c("ah", "bh", "ch", "dh")]
  names(psychiatry.contingencies) <- c("ak", "bk", "ck", "dk")

  apply.tests(psychiatry.contingencies, "psychiatry")
  
  # psychiatry
  thyroids.contingencies <- nested.to.contingency(thyroids, "patient", "x.pet", "x.spect")
  apply.tests(thyroids.contingencies, "thyroids")
})
