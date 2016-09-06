# An implementation of Obuchowski 1998, using of Yang 2010's simplification

obuchowski.test <- function(x, group.names, pre.measure.name, post.measure.name) {
  z <- results.to.contingency.cols(x, group.names, pre.measure.name, post.measure.name)
  obuchowski.impl(z$ak, z$bk, z$ck, z$dk)
}

obuchowski.impl <- function (ak, bk, ck, dk) {
  K <- length(nk)

  ((K - 1) / K) * sum(bk - ck)^2 / sum((bk - ck)^2)
}
