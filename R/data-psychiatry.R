#' Psychiatrist and patient disagreement data
#'
#' Psychiatrists and their patients were surveyed in pairs regarding patient
#' concerns and treatment. Each psychiatrist was asked whether each question
#' item was relevant to their patient and each of their patients were asked the
#' same. The data can be evaluated to answer the question of whether there was
#' patient/doctor agreement in each item. The sample was 29 psychiatrists, each
#' with 1-8 patients, for a total of N = 135 matched pairs.
#'
#' @docType data
#'
#' @usage data(psychiatry)
#'
#' @format A data frame with 29 rows and 7 variables:
#' \describe{
#'   \item{psychiatrist}{the ID of the psychiatrist}
#'   \item{Nh}{the number of the psychiatrist's patients participating in the experiment}
#'   \item{ah}{both participants answered 1}
#'   \item{bh}{patient answered 1, psychiatrist answered 0}
#'   \item{ch}{patient answered 0, psychiatrist answered 1}
#'   \item{dh}{both participants answered 0}
#'   \item{Wh}{Normalized difference: (bh - ch) / Nh}
#' }
#'
#' @keywords datasets
#'
#' @references
#' 
#' Eliasziw, M., & Donner, A. (1991). \emph{Application of the McNemar test to non-independent matched pair data}. Statistics in medicine, 10(12), 1981-1991.
#'
#' @source
#' 
#' Donner, A., & Petryshen, P. (1989). \emph{The statistical analysis of matched data in psychiatric research}. Psychiatry research, 28(1), 41-46.
#'
#' @examples
#' data(psychiatry)
#' 
#' psychiatry$Wh == round((psychiatry$bh - psychiatry$ch) / psychiatry$Nh, 2)
#' 
#' clust.bin.pair(psychiatry$ah, psychiatry$bh, psychiatry$ch, psychiatry$dh, method="eliasziw")

"psychiatry"
