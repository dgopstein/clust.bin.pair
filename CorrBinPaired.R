library(dplyr)
library(lazyeval)

corr.matched <- function(x, group.name, pre.measure.name, post.measure.name) {
  
}

x <- confusion
group.names <- c('subject', 'atom')
pre.measure.name <- 'control'
post.measure.name <- 'treatment'

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

durkalski.test <- function (x, group.names, pre.measure.name, post.measure.name) {
  z <- results.to.contingency.cols(x, group.names, pre.measure.name, post.measure.name)
  
  X2v <- sum( (1/z$nk)*(z$bk-x1$ck) )^2/sum(((z$bk - z$ck) / z$nk)^2 )
  
  X2v
}

durkalski.test(confusion, group.names, pre.measure.name, post.measure.name)
