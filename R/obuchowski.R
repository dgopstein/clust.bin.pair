# An implementation of Obuchowski 1998, using of Yang 2010's simplification

obuchowski.test <- function(x, group.names, pre.measure.name, post.measure.name) {
  z <- results.to.contingency.cols(x, group.names, pre.measure.name, post.measure.name)
  obuchowski.impl(z$nk, z$bk, z$ck)
}

obuchowski.impl <- function (nk, bk, ck) {
  K <- length(nk)

  ((K - 1) / K) * sum(bk - ck)^2 / sum((bk - ck)^2)
}
