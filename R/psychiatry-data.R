#' Pyschiatrist and patient disagreement data
#'
#' Reported by Petryshen, described by Eliasziw as follows:
#' Disagreements between patients and psychiatrists on check-listed items
#' that relate to patient concerns and treatment (Petryshen, et. al). One
#' of these items was the perceived relevance to treatment of the patient’s
#' inability to tolerate certain medications. A sample of 29 psychiatrists,
#' who each treated from one to eight patients, were asked to identify whether
#' in their opinion this item was relevant to each of their patients. In turn,
#' the 135 patients who received treatment from these psychiatrists were also
#' asked this question, as it applied to themselves. The aim of the study was
#' to investigate if patients and psychiatrists would equally likely identify
#' a given item as relevant. Since each patient matches with one of the
#' psychiatrists in the study, this suggested constructing N = 135 matched pairs
#' of responses ( Yli, Y,,), i = 1,2, . . . , 135, where YIi = 1 or 0 is the
#' patient response (identifies the item or not) and Y,i = 1 or 0 is the
#' psychiatrist response.
#'
#' @docType data
#'
#' @usage data(psychiatry)
#'
#' @format A data frame with 29 rows and 7 variables:
#' \describe{
#'   \item{psychiatrist}{the ID of the pyschiatrist}
#'   \item{Nh}{the number of the psychiatrist's patients participating in the experiment}
#'   \item{ah}{both participants answered 1}
#'   \item{bh}{patient answered 1, psychiatrist answered 0}
#'   \item{bc}{patient answered 0, psychiatrist answered 1}
#'   \item{dh}{both participants answered 0}
#'   \item{Wh}{Normalized difference: (bh - ch) / Nh}
#' }
#'
#' @keywords datasets
#'
#' @references Petryshen
#' @references Eliasziw 1991
#'
#' @source Petryshen
#'
#' @examples
#' data(psychiatry)
#' psychiatry$Wh == round((psychiatry$bh - psychiatry$ch) / psychiatry$Nh, 2)
"psychiatry"
