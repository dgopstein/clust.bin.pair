# Durkalski 2003

#' @export
durkalski.test <- function(ak, bk, ck, dk)
  .durkalski.impl(ak+bk+ck+dk, bk, ck)

.durkalski.impl <- function(nk, bk, ck)
  sum( (1/nk)*(bk-ck) )^2/sum(((bk - ck) / nk)^2 )
