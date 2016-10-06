# An implementation of McNemar 1945

.mcnemar.test <- function(ak, bk, ck, dk) .mcnemar.impl(bk,ck)

.mcnemar.impl <- function(bk, ck) {
  b <- sum(bk)
  c <- sum(ck)
  (b - c)^2/(b + c)
}