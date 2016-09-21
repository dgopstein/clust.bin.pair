#' Obfuscated C code misinterpretation data
#'
#' Data from Gopstein et. al.'s experiment on the misinterpretation of C code.
#' Subjects were asked to hand evaluate pairs of functionally equivalent code.
#' Half of the questions was intentionally obfuscated to elicit confusion.
#'
#' @docType data
#'
#' @usage data(obfuscation)
#'
#' @format A data frame with 57 rows and 4 variables:
#' \describe{
#'   \item{subject}{the ID of the study participant}
#'   \item{question}{the ID of the question being answered}
#'   \item{control}{whether the subject answered the un-obfuscated question correctly}
#'   \item{treatment}{whether the subject answered the obfuscated question correctly}
#' }
#'
#' @keywords datasets
#'
#' @references Gopstein et al. (2016)
#'
#' @source \href{http://atomsofconfusion.com}{Atoms of Confusion}
#'
#' @examples
#' data(obfuscation)
#' 
#' oc <- paired.to.contingency(obfuscation, c("subject", "atom"), "control", "treatment")
#' durkalski.test(oc$ak, oc$bk, oc$ck, oc$dk)

"obfuscation"
