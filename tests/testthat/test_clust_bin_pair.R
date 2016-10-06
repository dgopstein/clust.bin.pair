library(clust.bin.pair)

context("Top Level Functions")

pc <- psychiatry[, c('ah', 'bh', 'ch', 'dh')]
names(pc) <- c('ak', 'bk', 'ck', 'dk')

tc <- data.frame(nested.to.contingency(thyroids$x.pet, thyroids$x.spect))

test_that("McNemar scores datasets correctly", { 
  expect_equal(.mcnemar.impl(psychiatry$bh, psychiatry$ch), 11.85, tolerance=.1, scale=NULL, "psychiatry")
}) 

apply.tests <- function (x, x.name) {
  tests <- c(.mcnemar.test, .eliasziw.test, .obuchowski.test, .durkalski.test, .yang.test)
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
  obfuscation.contingencies <- paired.to.contingency(obfuscation[,c('subject', 'atom')], obfuscation$control, obfuscation$treatment)
  
  apply.tests(obfuscation.contingencies, "obfuscation")
  
  # psychiatry
  psychiatry.contingencies <- psychiatry[, c("ah", "bh", "ch", "dh")]
  names(psychiatry.contingencies) <- c("ak", "bk", "ck", "dk")

  apply.tests(psychiatry.contingencies, "psychiatry")
  
  # psychiatry
  thyroids.contingencies <- nested.to.contingency(thyroids$x.pet, thyroids$x.spect)
  apply.tests(data.frame(thyroids.contingencies), "thyroids")
})

test_that("Thyroid chi-square statistics match up with published values", {
  expect_equal(3.66, round(do.call(.eliasziw.test,   tc), 2)) # reported by Durkalski (2003)
# expect_equal(2.88, round(do.call(.obuchowski.test, tc), 2)) # reported by Obuchowski (1998)
  expect_equal(2.86, round(do.call(.obuchowski.test, tc), 2)) # reported by Durkalski (2003)
  expect_equal(2.32, round(do.call(.durkalski.test,  tc), 2)) # reported by Durkalski (2003)
  expect_equal(3.13, round(do.call(.yang.test,       tc), 2)) # reported by Yang (2010)
  expect_equal(4.5,        do.call(.mcnemar.test,   tc))     # reported by Durkalski (2003)
})

test_that("Pyschiatry chi-square statistics match up with published values", {
  expect_equal(10.23, round(do.call(.eliasziw.test,  pc),  2)) # reported by Durkalski (2003)
  expect_equal(7.19,  round(do.call(.obuchowski.test, pc), 2)) # reported by Durkalski (2003)
  expect_equal(7.542, round(do.call(.durkalski.test,  pc), 3)) # reported by Durkalski (2003)
  expect_equal(8.43,  round(do.call(.yang.test,       pc), 2)) # reported by Yang (2010)
  expect_equal(11.8451,     do.call(.mcnemar.test,   pc), 4)  # reported by Durkalski (2003)
})

test_that(".count.contingency", {
  expect_equal(4, which(1 == .count.contingency.pair(0, 0))[[1]])
  expect_equal(3, which(1 == .count.contingency.pair(0, 1))[[1]])
  expect_equal(2, which(1 == .count.contingency.pair(1, 0))[[1]])
  expect_equal(1, which(1 == .count.contingency.pair(1, 1))[[1]])
  
  expect_equal(4, which(1 == .count.contingency.pair(FALSE, FALSE))[[1]])
  expect_equal(3, which(1 == .count.contingency.pair(FALSE, TRUE))[[1]])
  expect_equal(2, which(1 == .count.contingency.pair(TRUE, FALSE))[[1]])
  expect_equal(1, which(1 == .count.contingency.pair(TRUE, TRUE))[[1]])
  
  df <- data.frame(t1 = c(0, 0, 1, 1), t2 = c(0, 1, 0, 1))
  all(.count.contingency(df[1], df[2]) == c(1, 1, 1, 1))
  
  df.rep <- df[rep(1:4, times=1:4),]
  all(.count.contingency(t1 = df.rep[1], t2 = df.rep[2]) == 4:1)
})

test_that("nested.to.contingency", {
  thyroid.expected <- data.frame(ak=c(0,2,3,1,2,4), bk=rep(0,times=6), ck=c(2,1,0,0,1,0), dk=c(1,0,0,0,0,0))
  thyroid.unnested <- nested.to.contingency(thyroids$x.pet, thyroids$x.spect)
  expect_true(all(thyroid.expected == head(thyroid.unnested)), info = "nested.to.contingency works for thyroids")
  
  nested.list  <-  list(id = c(1, 2, 3), t1 = list(c(0, 0), c(1, 0, 0), c(1, 1)), t2 = list(c(0, 1), c(1, 1, 0), c(0, 0)))
  nested.cbind <- cbind(id = c(1, 2, 3), t1 = list(c(0, 0), c(1, 0, 0), c(1, 1)), t2 = list(c(0, 1), c(1, 1, 0), c(0, 0)))
  nested.df <- as.data.frame(nested.cbind)
  nested.df.w.c.id <- nested.df
  nested.df.w.c.id$id <- unlist(nested.df.w.c.id$id)
  
  nested.res <- data.frame(ak = c(0,1,0), bk = c(0,0,2), ck = c(1,1,0), dk = c(1,1,0))
  
  expect_true(all(nested.res == nested.to.contingency(nested.df.w.c.id$t1, nested.df.w.c.id$t2)))
  expect_true(all(nested.res == nested.to.contingency(nested.df$t1, nested.df$t2)))
  expect_true(all(nested.res == nested.to.contingency(nested.cbind[,"t1"], nested.cbind[,"t2"])))
  expect_true(all(nested.res == nested.to.contingency(nested.list$t1, nested.list$t2)))
})

test_that("paired.to.contingency", {
  
  obfuscation.expected <- data.frame(subject = 1:3, atom = c("CONDITION","PARENTHESIS","POST_INC_DEC"),
                                     ak = c(0,1,0), bk = c(0,0,0), ck = c(1,0,2), dk = c(1,1,0), stringsAsFactors=FALSE)

  obfuscation.unpaired <- paired.to.contingency(obfuscation[, c("subject", "atom")], obfuscation$control, obfuscation$treatment)
  
  obfuscation.res <-
    obfuscation.unpaired[(obfuscation.unpaired$subject == 1 & obfuscation.unpaired$atom == "CONDITION") |
                         (obfuscation.unpaired$subject == 2 & obfuscation.unpaired$atom == "PARENTHESIS") |
                         (obfuscation.unpaired$subject == 3 & obfuscation.unpaired$atom == "POST_INC_DEC"),]
  
  expect_true(all(obfuscation.expected == obfuscation.res), info = "paired.to.contingency works for obfuscation")
})

test_that("is.whole", {
  expect_true( .is.whole(c(1, 2, 3, 4)))
  expect_false(.is.whole(c(1, 2, 3, 4.5)))
  expect_false(.is.whole(c(1.2, 2, 3, 4)))
})

test_that("clust.bin.pair", {
  tests <- c("yang", "durkalski", "obuchowski", "eliasziw", "mcnemar")
  
  tc.stats <- sapply(tests, function(test) clust.bin.pair(tc$ak, tc$bk, tc$ck, tc$dk, method=test)$statistic)
  expect_equal(c(3.13, 2.32, 2.86, 3.66, 4.5), unname(round(tc.stats, 2)))

  pc.stats <- sapply(tests, function(test) clust.bin.pair(pc$ak, pc$bk, pc$ck,pc$dk, method=test)$statistic)
  expect_equal(c(8.43, 7.54, 7.19, 10.23, 11.85), unname(round(pc.stats, 2)))
  
  expect_error(clust.bin.pair(1, 2, 3, 4, method="xxx"), "method")
  expect_error(clust.bin.pair(1, 2, 3, c(4, 5)), "length")
  expect_error(clust.bin.pair(1, 2, 3, 4.5), "integer")
})