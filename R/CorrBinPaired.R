library(dplyr)
library(lazyeval)
library(testthat)

corr.matched <- function(x, group.name, pre.measure.name, post.measure.name) {
  
}

results.to.contingency.cols <- function(x, group.names, pre.measure.name, post.measure.name) {
  x %>%
    group_by_(.dots = group.names) %>%
    summarize_(
      nk = ~n(),
      ak = interp(~sum( pre &  post), pre = as.name(pre.measure.name), post = as.name(post.measure.name)),
      bk = interp(~sum( pre & !post), pre = as.name(pre.measure.name), post = as.name(post.measure.name)),
      ck = interp(~sum(!pre &  post), pre = as.name(pre.measure.name), post = as.name(post.measure.name)),
      dk = interp(~sum(!pre & !post), pre = as.name(pre.measure.name), post = as.name(post.measure.name)))
}

# devtools::test()

