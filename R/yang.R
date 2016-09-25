#' An implementation of Yang et al. 2010
#'
#' An adjustment to mcnemar's test for maginal homogeneity, extended from
#' Obuchowski 1998.
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
#' yang.test(c(3,13,2,1,2), c(16,63,15,15,79), c(7,11,12,3,9), c(12,32,3,23,3))
#'
#' pc <- psychiatry[, c('ah', 'bh', 'ch', 'dh')]
#' yang.test(pc$ah, pc$bh, pc$ch, pc$dh)
#' 
#' tc <- data.frame(nested.to.contingency(thyroids$x.pet, thyroids$x.spect))
#' yang.test(tc$ak, tc$bk, tc$ck, tc$dk)
#'
#' oc <- paired.to.contingency(list(obfuscation$subject, obfuscation$atom),
#'                             obfuscation$control, obfuscation$treatment)
#' yang.test(oc$ak, oc$bk, oc$ck, oc$dk)
#'
#' @export
yang.test <- function(ak, bk, ck, dk)
  .yang.impl(ak+bk+ck+dk, bk, ck)

.yang.impl <- function(nk, bk, ck) {
  N <- sum(nk)
  K <- length(nk)
  
  p1.tilde <- (1 / N) * sum(bk)
  p2.tilde <- (1 / N) * sum(ck)
  
  X2mo <-
    ((K - 1) / K) *
      sum(bk - ck)^2 /
        ( (1/2) * sum( ((bk - ck) - nk*(p1.tilde - p2.tilde))^2  + (bk - ck)^2 ) )
  
  X2mo
}