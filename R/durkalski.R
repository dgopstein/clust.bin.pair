# An implementation of Durkalski et al. 2003

.durkalski.test <- function(ak, bk, ck, dk)
  .durkalski.impl(ak+bk+ck+dk, bk, ck)

.durkalski.impl <- function(nk, bk, ck)
  sum( (bk-ck)/nk )^2 / sum( ((bk-ck)/nk)^2 )
