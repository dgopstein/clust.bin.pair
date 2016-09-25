#' An implementation of Obuchowski 1998
#'
#' An adjustment to mcnemar's test for maginal homogeneity.
#'
#' @param ak vector containing counts per group of Success/Success results.
#' @param bk vector containing counts per group of Success/Failure results.
#' @param ck vector containing counts per group of Failure/Success results.
#' @param dk vector containing counts per group of Failure/Failure results.
#' 
#' @return Chi-square statistic
#'
#' @examples
#' 
#' obuchowski.test(c(1,3,0,1,2), c(56,64,75,95,87), c(7,1,2,3,9), c(0,2,1,3,3))
#'
#' pc <- psychiatry[, c('ah', 'bh', 'ch', 'dh')]
#' obuchowski.test(pc$ah, pc$bh, pc$ch, pc$dh)
#' 
#' tc <- data.frame(nested.to.contingency(thyroids$x.pet, thyroids$x.spect))
#' obuchowski.test(tc$ak, tc$bk, tc$ck, tc$dk)
#'
#' oc <- paired.to.contingency(list(obfuscation$subject, obfuscation$atom),
#'                             obfuscation$control, obfuscation$treatment)
#' obuchowski.test(oc$ak, oc$bk, oc$ck, oc$dk)
#' 
#' @export
obuchowski.test <- function(ak, bk, ck, dk) .obuchowski.impl(bk, ck)

# Notation is Yang et al. (2010)'s simplification
.obuchowski.impl <- function (bk, ck) {
  K <- length(bk)
  
  ((K - 1) / K) * sum(bk - ck)^2 / sum((bk - ck)^2)
}
