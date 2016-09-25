#' Statistical test for clustered matched pair data
#'
#' A single interface for several adjustements to the mcnemar test for marginal
#' homogeneity that correct for clustered data.
#'
#' @param ak vector containing counts per group of Success/Success results.
#' @param bk vector containing counts per group of Success/Failure results.
#' @param ck vector containing counts per group of Failure/Success results.
#' @param dk vector containing counts per group of Failure/Failure results.
#' @param method a character string specifying the method to calculate the
#' statistic. Must be one of "yang" (default), "durkalski", "obuchowski", "eliasziw".
#' 
#' @return
#' A list with class "htest" containing the following components:
#'
#' \item{statistic}{the value of the test statistic.}
#' \item{p.value}{the p-value for the test.}
#' \item{method}{the type of test applied.}
#' \item{data.name}{a character string giving the names of the data.}
#' 
#' @examples
#' 
#' tc <- data.frame(nested.to.contingency(thyroids$x.pet, thyroids$x.spect))
#' clust.bin.pair(tc$ak, tc$bk, tc$ck, tc$dk, method="obuchowski")
#' 
#' pc <- psychiatry[, c('ah', 'bh', 'ch', 'dh')]
#' clust.bin.pair(pc$ah, pc$bh, pc$ch, pc$dh, method="eliasziw")
#'
#' @export
clust.bin.pair <- function(ak, bk, ck, dk, method="yang") {
  choises <- c("yang", "durkalski", "obuchowski", "eliasziw")
  method <- choises[pmatch(method, choises)]
  
  if (length(method) > 1 || is.na(method)) 
    stop(paste0("method must be one of ", deparse(choises)))
  
  if (length(unique(sapply(list(ak, bk, ck, dk), length))) != 1)
    stop("ak, bk, ck, and dk must all be vectors of identical length")
  
  if (!is.integer(ak, bk, ck, dk)) # is integer
    stop("ak, bk, ck, and dk must all be integer vectors")
    
  test <- switch (method,
    yang       = yang.test,
    durkalski  = durkalski.test,
    obuchowski = obuchowski.test,
    eliasziw   = eliasziw.test)
  
  data.name <-
    paste(list(deparse(substitute(ak)), deparse(substitute(bk)),
               deparse(substitute(ck)), deparse(substitute(dk))), collapse=", ")
  
  method.capitalized <- paste0(toupper(substr(method, 1, 1)), substring(method, 2))
  method.name <- paste0(method.capitalized, "'s Chi-square test")
  
  statistic <- test(ak, bk, ck, dk)
  names(statistic) <- "chi-square"
  
  parameter <- 1
  names(parameter) <- "df"
  
  p.value <- 1 - stats::pchisq(statistic, 1)  
  
 rval <- list(statistic = statistic,
               parameter = parameter, 
               p.value = p.value,
               method = method.name,
               data.name = data.name)
  class(rval) <- "htest"

  rval
}