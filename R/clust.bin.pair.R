#' Statistical test for clustered binary matched pair data
#'
#' A single interface for several adjustments to the mcnemar test for marginal
#' homogeneity that correct for clustered data.
#'
#' @param ak vector containing counts per group of Success/Success results.
#' @param bk vector containing counts per group of Success/Failure results.
#' @param ck vector containing counts per group of Failure/Success results.
#' @param dk vector containing counts per group of Failure/Failure results.
#' @param method a character string specifying the method to calculate the
#' statistic. Must be one of "yang" (default), "durkalski", "obuchowski", "eliasziw".
#' A value of "mcnemar" can also be supplied for comparison.
#'
#' @return
#' A list with class "htest" containing the following components:
#'
#' \item{statistic}{the value of the test statistic.}
#' \item{p.value}{the p-value for the test.}
#' \item{method}{the type of test applied.}
#' \item{data.name}{a character string giving the names of the data.}
#'
#' @references
#'
#' McNemar, Q. (1947). \emph{Note on the sampling error of the difference between correlated proportions or percentages}. Psychometrika, 12(2), 153-157.
#'
#' Eliasziw, M., & Donner, A. (1991). \emph{Application of the McNemar test to non-independent matched pair data}. Statistics in medicine, 10(12), 1981-1991.
#'
#' Obuchowski, N. A. (1998). \emph{On the comparison of correlated proportions for clustered data}. Statistics in medicine, 17(13), 1495-1507.
#'
#' Durkalski, V. L., Palesch, Y. Y., Lipsitz, S. R., & Rust, P. F. (2003). \emph{Analysis of clustered matched-pair data}. Statistics in medicine, 22(15), 2417-2428.
#'
#' Yang, Z., Sun, X., & Hardin, J. W. (2010). \emph{A note on the tests for clustered matched-pair binary data}. Biometrical journal, 52(5), 638-652.
#'
#' @examples
#'
#' with(psychiatry, clust.bin.pair(ah, bh, ch, dh, method="eliasziw"))
#'
#' tc <- nested.to.contingency(thyroids$x.pet, thyroids$x.spect)
#' clust.bin.pair(tc$ak, tc$bk, tc$ck, tc$dk, method="obuchowski")
#'
#' oc <- with(obfuscation, paired.to.contingency(group = list(subject, atom),
#'                                               t1 = control, t2 = treatment))
#' clust.bin.pair(oc$ak, oc$bk, oc$ck, oc$dk, method="durkalski")
#'
#' @export
clust.bin.pair <- function(ak, bk, ck, dk, method="yang") {
  choices <- c("yang", "durkalski", "obuchowski", "eliasziw", "mcnemar")
  method <- choices[pmatch(method, choices)]

  if (length(method) > 1 || is.na(method))
    stop(paste0("method must be one of ", deparse(choices)))

  if (length(unique(sapply(list(ak, bk, ck, dk), length))) != 1)
    stop("ak, bk, ck, and dk must all be vectors of identical length")

  if (!.is.whole(c(ak, bk, ck, dk))) # is integer
    stop("ak, bk, ck, and dk must all be integer vectors")

  test <- switch (method,
    yang       = .yang.test,
    durkalski  = .durkalski.test,
    obuchowski = .obuchowski.test,
    eliasziw   = .eliasziw.test,
    mcnemar    = .mcnemar.test)

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
