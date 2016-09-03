library(dplyr)
library(lazyeval)

corr.matched <- function(x, group.name, pre.measure.name, post.measure.name) {
  
}

# x <- confusion
# group.names <- c('subject', 'atom')
# pre.measure.name <- 'control'
# post.measure.name <- 'treatment'

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

###########################################################
#                        Durkalski
###########################################################

durkalski.test <- function (x, group.names, pre.measure.name, post.measure.name) {
  z <- results.to.contingency.cols(x, group.names, pre.measure.name, post.measure.name)
  durkalski.impl(z$nk, z$bk, z$ck)
}

durkalski.impl <- function(nk, bk, ck) {    
  sum( (1/nk)*(bk-ck) )^2/sum(((bk - ck) / nk)^2 )
}

###########################################################
#                        Obuchowski
###########################################################

obuchowski.test <- function(x, group.names, pre.measure.name, post.measure.name) {
  z <- results.to.contingency.cols(x, group.names, pre.measure.name, post.measure.name)
  obuchowski.impl(z$bk, z$ck, z$nk, nrow(z))
}

# x1 <- z$bk
# x2 <- z$ck
# N  <- z$nk
# m <- nrow(z)

obuchowski.impl <- function(x1, x2, N, m) {
  # Eqn (1)
  p.hat <- function(x, N) sum(x) / sum(N)
  
  # Eqn (2)
  var.hat <- function(x, N, m)
    m * (m-1)^-1 * sum( (x - (N * p.hat(x, N)))^2 ) / sum(N)^2
  
  # Eqn (3)
  cov.hat <- function(x, x1, N, m)
    m * (m-1)^-1 * sum( (x - (N * p.hat(x, N))) * (x1 - (N * p.hat(x1, N))) ) / sum(N)^2
  
  # Eqn (4)
  var.hat.diff <- function(x, x1, N, m)
    var.hat(x, N, m) + var.hat(x1, N, m) - 2 * cov.hat(x, x1, N, m)
  
  # Eqn (6)
  ((p.hat(x1, N) - p.hat(x2, N))^2) / var.hat.diff(x1, x2, N, m)
}

durkalski.test(confusion, group.names, pre.measure.name, post.measure.name)
obuchowski.test(confusion, group.names, pre.measure.name, post.measure.name)
