# An implementation of Obuchowski 1998, using of Yang 2010's simplification

obuchowski.test <- function(ak, bk, ck, dk)
  .obuchowski.impl(ak+bk+ck+dk, bk, ck)

.obuchowski.impl <- function (nk, bk, ck) {
  K <- length(nk)

  ((K - 1) / K) * sum(bk - ck)^2 / sum((bk - ck)^2)
}
