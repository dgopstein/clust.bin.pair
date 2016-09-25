#' clust.bin.pair: Statistical tests for clustered binary matched pair data
#'
#' The clust.bin.pair package privides statistical tests and utilities
#' for dealing with clustered binary matched pair data.
#'
#' @docType package
#' @name clust.bin.pair
NULL

#' Statistical test for clustered matched pair data
#'
#' A single interface for several adjustements to the mcnemar test for marginal
#' homogeneity that correct for clustered data.
#'
#' @param ak Vector containing counts per group of Success/Success results.
#' @param bk Vector containing counts per group of Success/Fail results.
#' @param ck Vector containing counts per group of Fail/Success results.
#' @param dk Vector containing counts per group of Fail/Fail results.
#' @param method A character string specifying the method to calculate the
#' statistic. Must be one of "yang" (default), "durkalski", "obuchowski", "eliasziw".
#' 
#' @return
#' A list with class "htest" containing the following components:
#'
#' @field statistic the value of the test statistic with a name describing it.
#' @field p.value the p-value for the test.
#' @field method the type of test applied.
#' @field data.name a character string giving the names of the data.
#' 
#' @examples
#' tc <- data.frame(nested.to.contingency(thyroids$x.pet, thyroids$x.spect))
#' clust.bin.pair.test(tc$ak, tc$bk, tc$ck, tc$dk, method="obuchowski")
#' 
#' pc <- psychiatry[, c('ah', 'bh', 'ch', 'dh')]
#' clust.bin.pair.test(pc$ah, pc$bh, pc$ch, pc$dh, method="eliasziw")
#'
#' @export
clust.bin.pair.test <- function(ak, bk, ck, dk, method="yang") {
  data.name <-
    paste(list(deparse(substitute(ak)), deparse(substitute(bk)),
               deparse(substitute(ck)), deparse(substitute(dk))), collapse=", ")
  
  choises <- c("yang", "durkalski", "obuchowski", "eliasziw")
  method <- choises[pmatch(method, choises)]
  
  if (length(method) > 1 || is.na(method)) 
    stop(paste0("method must be one of ", deparse(choises)))
  
  test <- switch (method,
    yang       = yang.test,
    durkalski  = durkalski.test,
    obuchowski = obuchowski.test,
    eliasziw   = eliasziw.test)
  
  method.capitalized <- paste0(toupper(substr(method, 1, 1)), substring(method, 2))
  method.name <- paste0(method.capitalized, "'s Chi-square test")
  
  statistic <- test(ak, bk, ck, dk)
  names(statistic) <- paste0(method.capitalized, " chi-square")
  
  p.value <- 1 - stats::pchisq(statistic, 1)  
  
  rval <- list(statistic = statistic,
               p.value = p.value,
               method = method.name,
               data.name = data.name)
  class(rval) <- "htest"

  rval
}