library(dplyr)
library(lazyeval)

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

.mcnemar <- function(bk, ck) {
  b <- sum(bk)
  c <- sum(ck)
  (b - c)^2/(b + c)
}

.mcnemar.wrapper <- function (x, group.names, pre.measure.name, post.measure.name) {
  z <- results.to.contingency.cols(x, group.names, pre.measure.name, post.measure.name)
  mcnemar(z$bk, z$ck)
}