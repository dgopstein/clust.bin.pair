library(clust.bin.pair)

context("Top Level Functions")

test_that("McNemar scores datasets correctly", { 
  expect_equal(.mcnemar.impl(psychiatry$bh, psychiatry$ch), 11.85, tolerance=.1, scale=NULL, "psychiatry")
}) 

apply.tests <- function (x, x.name) {
  tests <- c(.mcnemar.test, eliasziw.test, obuchowski.test, durkalski.test, yang.test)
  res <- sapply(tests, function(t) do.call(t, list(ak=x$ak, bk=x$bk, ck=x$ck, dk=x$dk)))
  names(res) <- c("mcnemar", "eliasziw", "obuchowski", "durkalski", "yang")
  
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

test_that("Contingency generation functions work", {
  nested.list  <-  list(id = c(1, 2, 3), t1 = list(c(0, 0), c(1, 0, 0), c(1, 1)), t2 = list(c(0, 1), c(1, 1, 0), c(0, 0)))
  nested.cbind <- cbind(id = c(1, 2, 3), t1 = list(c(0, 0), c(1, 0, 0), c(1, 1)), t2 = list(c(0, 1), c(1, 1, 0), c(0, 0)))
  nested.df <- as.data.frame(nested.cbind)
  nested.df.w.c.id <- nested.df
  nested.df.w.c.id$id <- unlist(nested.df.w.c.id$id)

  expect_error(nested.to.contingency(thyroids, "id", "t1", "t2"), "column.*id")
  
  thyroid.contingency <-
    data.frame(patient = 1:6, nk = c(3,3,3,1,3,4), ak=c(0,2,3,1,2,4), bk=rep(0,times=6), ck=c(2,1,0,0,1,0), dk=c(1,0,0,0,0,0))
  expect_true(all(thyroid.contingency == head(nested.to.contingency(thyroids, 'patient', 'x.pet', 'x.spect'))))
  
  nested.to.contingency(nested.df.w.c.id, id.name='id', response1.name='t1', response2.name='t2')
  nested.to.contingency(nested.df, id.name='id', response1.name='t1', response2.name='t2')
  
  
  
  str(nested.df)
  str(thyroids)
})
