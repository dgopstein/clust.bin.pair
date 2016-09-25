#' An implementation of Durkalski et al. 2003
#'
#' An adjustment to mcnemar's test for maginal homogeneity based on the method
#' of moments
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
#' durkalski.test(c(1,3,0,1,2), c(51,67,44,58,47), c(18,26,43,21,28), c(0,2,1,3,4))
#'
#' pc <- psychiatry[, c('ah', 'bh', 'ch', 'dh')]
#' durkalski.test(pc$ah, pc$bh, pc$ch, pc$dh)
#' 
#' tc <- data.frame(nested.to.contingency(thyroids$x.pet, thyroids$x.spect))
#' durkalski.test(tc$ak, tc$bk, tc$ck, tc$dk)
#'
#' oc <- paired.to.contingency(list(obfuscation$subject, obfuscation$atom),
#'                             obfuscation$control, obfuscation$treatment)
#' durkalski.test(oc$ak, oc$bk, oc$ck, oc$dk)
#'
#' @export
durkalski.test <- function(ak, bk, ck, dk)
  .durkalski.impl(ak+bk+ck+dk, bk, ck)

.durkalski.impl <- function(nk, bk, ck)
  sum( (bk-ck)/nk )^2 / sum( ((bk-ck)/nk)^2 )
