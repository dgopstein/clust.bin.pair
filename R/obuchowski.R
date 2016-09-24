#' An implementation of Obuchowski 1998, using of Yang 2010's simplification
#'
#' An adjustment to mcnemar's test for maginal homogeneity.
#'
#' @param ak Vector containing counts per group of Success/Success results.
#' @param bk Vector containing counts per group of Success/Fail results.
#' @param ck Vector containing counts per group of Fail/Success results.
#' @param dk Vector containing counts per group of Fail/Fail results.
#' 
#' @return The chi-square statistic
#'
#' @examples
#' 
#' obuchowski.test(c(1,3,0,1,2), c(56,64,75,95,87), c(7,1,2,3,9), c(0,2,1,3,3))
#'
#' tc <- data.frame(nested.to.contingency(thyroids$x.pet, thyroids$x.spect))
#' obuchowski.test(tc$ak, tc$bk, tc$ck, tc$dk)
#'
#' @export
obuchowski.test <- function(ak, bk, ck, dk) .obuchowski.impl(bk, ck)

.obuchowski.impl <- function (bk, ck) {
  K <- length(bk)
  
  ((K - 1) / K) * sum(bk - ck)^2 / sum((bk - ck)^2)
}
