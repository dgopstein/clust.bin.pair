#' clust.bin.pair: A package for clustered binary matched pair data
#'
#' The clust.bin.pair package privides statistical tests, and several utilities
#' for dealing with clustered binary matched pair data.
#'
#' @docType package
#' @name clust.bin.pair
NULL
#> NULL

#' #' An implementation of Durkalski et al. 2003
#'
#' An adjustment to mcnemar's test for maginal homogeneity based on the method
#' of moments
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
#' @example
#' clust.bin.pair(tc$ak, tc$bk, tc$ck, tc$dk, method="durkalski")
#'
#' @usage
#' clust.bin.pair(ak, bk, ck, dk, method = c("yang", "durkalski", "obuchowski", "eliasziw"))
#' 
#' @export
clust.bin.pair <- function(ak, bk, ck, dk, method="yang") {
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
  
  p.value <- 1 - pchisq(statistic, 1)  
  
  rval <- list(statistic = statistic,
               p.value = p.value,
               method = method.name,
               data.name = data.name)
  class(rval) <- "htest"

  rval
}