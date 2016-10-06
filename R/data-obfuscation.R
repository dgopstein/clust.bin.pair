#' Obfuscated C code misinterpretation data
#'
#' Data from Gopstein et. al.'s experiment on the misinterpretation of C code.
#' Subjects were asked to hand evaluate pairs of functionally equivalent code.
#' Half of the questions were intentionally obfuscated to elicit confusion.
#'
#' @docType data
#'
#' @usage data(obfuscation)
#'
#' @format A data frame with 57 rows and 4 variables:
#' \describe{
#'   \item{subject}{the ID of the study participant}
#'   \item{atom}{the type of obfuscation being evaluated}
#'   \item{control}{whether the subject answered the un-obfuscated question correctly}
#'   \item{treatment}{whether the subject answered the obfuscated question correctly}
#' }
#'
#' @keywords datasets
#'
#' @source \href{http://atomsofconfusion.com}{Atoms of Confusion}
#'
#' @examples
#' data(obfuscation)
#' 
#' oc <- paired.to.contingency(group = obfuscation[,c("subject", "atom")],
#'                             t1    = obfuscation$control,
#'                             t2    = obfuscation$treatment)
#' 
#' clust.bin.pair(oc$ak, oc$bk, oc$ck, oc$dk, method="durkalski")

"obfuscation"
