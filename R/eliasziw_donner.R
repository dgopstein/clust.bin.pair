# Eliasziw & Donner 1991

mcnemars <- function(bk, ck) {
  b <- sum(bk)
  c <- sum(ck)
  (b - c)^2/(b + c)
}

eliasziw1.test <- function (x, group.names, pre.measure.name, post.measure.name) {
  z <- results.to.contingency.cols(x, group.names, pre.measure.name, post.measure.name)
  eliasziw1.impl(z$bk, z$ck)
}

eliasziw1.impl <-function (bk, ck) {
  # Number of discordant answers per subject
  Sk <- bk + ck
  
  # Number of subjects with discordant answer
  Kd <- sum(Sk >= 1)
  
  # Number of subjects
  #K <- nrow(Sk)
  K <- length(Sk)
  
  p.bar <- sum(bk) / sum(Sk)
  
  S.bar <- (1/Kd) * sum(Sk)
  
  S0 <- S.bar - (sum((Sk - S.bar)^2) - (K - Kd)*(S.bar^2)) / (Kd * (Kd - 1) * S.bar)
  
  BMS <- (1/Kd) * sum( ifelse(Sk >= 1, ((( bk - Sk*p.bar )^2) / Sk), 0) )
  WMS <- (1/(Kd * (S.bar - 1))) * sum( ifelse(Sk >= 1, (( bk * (Sk - bk) ) / Sk), 0) )
  
  rho.hat <- (BMS - WMS) / (BMS + (S0 - 1)*WMS)
  
  nc <- S0 + Kd*(S.bar - S0)
  
  X2m <- mcnemars(bk, ck)
  
  C.hat <- (1 + (nc - 1) * rho.hat)
  
  assertthat::are_equal((1 + (nc - 1) * rho.hat), sum(Sk*(1+(Sk - 1)*rho.hat)) / sum(Sk))
  
  X2ma <- X2m / C.hat
  
  X2ma
}


eliasziw2 <- function (abcd) {
  bk <- abcd$TF
  ck <- abcd$FT
  
  # Row-wise sum [2, 2, 2, ...]
  nk <- Reduce("+", abcd)

  # Number of discordant answers per subject
  Sk <- bk + ck
  
  # Number of subjects with discordant answer
  Kd <- sum(Sk >= 1)
  
  # Number of subjects
  K <- length(nk)
  
  n.bar <- (1 / K) * sum(nk)
  
  n0 <- n.bar - ( sum( (nk - n.bar)^2 ) ) / (K * (K - 1) * n.bar )
  
  # Total number of responses
  N = sum(nk)
  
  abcd.mat <- data.matrix(abcd)
  
  # Column-wise sum: TT:1678, TF:168, FT:845, FF:342
  abcd.sum <- sapply(abcd, sum)
  
  P.hat <- abcd.sum / N

  nk_X_P.hat <-t(sapply(nk, function(x) x * P.hat))
  dput(abcd.mat)
  dput(nk_X_P.hat)

  BMSpooled <- (1 / K) * sum( (abcd.mat - nk_X_P.hat)^2 / nk )
  WMSpooled <- (1 / (K * (n.bar - 1))) * sum( ( abcd.mat *  as.vector(nk - abcd.mat)) / nk )
  
  rho.tilde.star <- (BMSpooled - WMSpooled) / (BMSpooled + (n0 - 1)*WMSpooled)
  
  rho.tilde <- 1 / (1 + P.hat[['TF']]*(1 - rho.tilde.star)/rho.tilde.star
                      + P.hat[['FT']]*(1 - rho.tilde.star)/rho.tilde.star)
  
  S.bar <- (1/Kd) * sum(Sk)
  
  S0 <- S.bar - sum((Sk - S.bar)^2 - (K - Kd)*(S.bar^2)) / (Kd * (Kd - 1) * S.bar)
  
  nc <- S0 + Kd*(S.bar - S0)

  C.hat <- 1 + (nc - 1) * rho.tilde
  
  X2di <- mcnemars(abcd$TF, abcd$FT) / C.hat
  
  X2di
}

abcd.nk <- Reduce('+', abcd)
mcnemars(abcd$TF, abcd$FT)
durkalski.impl(abcd.nk, abcd$TF, abcd$FT)
eliasziw1.impl(abcd$TF, abcd$FT)
eliasziw2(abcd)
