# An implementation of Obuchowski 1998

.obuchowski.test <- function(ak, bk, ck, dk) .obuchowski.impl(bk, ck)

# Notation is Yang et al. (2010)'s simplification
.obuchowski.impl <- function (bk, ck) {
  K <- length(bk)
  
  ((K - 1) / K) * sum(bk - ck)^2 / sum((bk - ck)^2)
}
