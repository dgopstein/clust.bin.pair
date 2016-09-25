#' An implementation of Eliasziw and Donner 1991
#'
#' An adjustment to mcnemar's test for maginal homogeneity based on the
#' intracluster correlation coe cient (ICC).
#'
#' @param ak Vector containing counts per group of Success/Success results.
#' @param bk Vector containing counts per group of Success/Fail results.
#' @param ck Vector containing counts per group of Fail/Success results.
#' @param dk Vector containing counts per group of Fail/Fail results.
#' 
#' @return Chi-square statistic
#'
#' @examples
#' 
#' eliasziw.test(c(1,3,0,1,2), c(20,47,39,30,28), c(18,45,23,21,29), c(0,2,1,3,4))
#' 
#' pc <- psychiatry[, c('ah', 'bh', 'ch', 'dh')]
#' eliasziw.test(pc$ah, pc$bh, pc$ch, pc$dh)
#' 
#' tc <- data.frame(nested.to.contingency(thyroids$x.pet, thyroids$x.spect))
#' eliasziw.test(tc$ak, tc$bk, tc$ck, tc$dk)
#'
#' oc <- paired.to.contingency(list(obfuscation$subject, obfuscation$atom),
#'                             obfuscation$control, obfuscation$treatment)
#' eliasziw.test(oc$ak, oc$bk, oc$ck, oc$dk)
#' 
#' @export
eliasziw.test <- function(ak, bk, ck, dk)
  .eliasziw.impl(data.frame(ak=ak,bk=bk,ck=ck,dk=dk))

.eliasziw.impl <- function (abcd) {
  bk <- abcd$bk
  ck <- abcd$ck
  
  # Row-wise sum
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
  
  # Column-wise sum
  abcd.sum <- sapply(abcd, sum)

  P.hat <- abcd.sum / N
  
  nk_X_P.hat <-t(sapply(nk, function(x) x * P.hat))

  BMSpooled <- (1 / K) * sum( (abcd.mat - nk_X_P.hat)^2 / nk )
  WMSpooled <- (1 / (K * (n.bar - 1))) * sum( ( abcd.mat *  as.vector(nk - abcd.mat)) / nk )

  rho.tilde.star <- (BMSpooled - WMSpooled) / (BMSpooled + (n0 - 1)*WMSpooled)
  
  rho.tilde <- 1 / (1 + P.hat[['bk']]*(1 - rho.tilde.star)/rho.tilde.star
                      + P.hat[['ck']]*(1 - rho.tilde.star)/rho.tilde.star)
  
  S.bar <- (1/Kd) * sum(Sk)
  
  S0 <- S.bar - sum((Sk - S.bar)^2 - (K - Kd)*(S.bar^2)) / (Kd * (Kd - 1) * S.bar)
  S0 <- S.bar - (sum((Sk - S.bar)^2) - (K - Kd)*(S.bar^2)) / (Kd * (Kd - 1) * S.bar)
  
  nc <- S0 + Kd*(S.bar - S0)

  C.hat <- 1 + (nc - 1) * rho.tilde
  
  X2di <- .mcnemar.impl(bk, ck) / C.hat
  
  X2di
}
