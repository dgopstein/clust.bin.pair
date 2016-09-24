# An implementation of Obuchowski 1998, using of Yang 2010's simplification

#' @export
obuchowski.test <- function(ak, bk, ck, dk) .obuchowski.impl(bk, ck)

.obuchowski.impl <- function (bk, ck) {
  K <- length(bk)
  
  ((K - 1) / K) * sum(bk - ck)^2 / sum((bk - ck)^2)
}
