#' Convert between paired results and the canonical contingency tables
#'
#' Group results by common clustering then tally the concordant and discordant
#' pairs.
#'
#' @param group List of grouping values
#' @param t1 pre-treatment measures
#' @param t2 post-treatment measures
#' 
#' @return Contingency tables represented in the rows of a matrix
#'
#' @examples
#'
#' paired.to.contingency(list(obfuscation$subject, obfuscation$atom),
#'                       obfuscation$control, obfuscation$treatment)
#' 
#' @export
paired.to.contingency <- function(group, t1, t2)
  stats::aggregate(t(mapply(.count.contingency.pair, t1, t2)), by=group, FUN=sum)

#' Convert between nested results and the canonical contingency tables
#'
#' Sum all concordant and discordant pairs from each nested group into a
#' contingency table.
#'
#' @param t1 lists of pre-treatment measures
#' @param t2 lists of post-treatment measures
#' 
#' @return Contingency tables represented in the rows of a matrix
#'
#' @examples
#'
#' nested.to.contingency(thyroids$x.pet, thyroids$x.spect)
#' 
#' @export
nested.to.contingency <- function(t1, t2)
  t(apply(cbind(t1, t2), 1, function(x) .count.contingency(x[1], x[2])))

# fill a vector [ak, bk, ck, dk] with the counts from matched pair data [t1, t2]
.count.contingency <- function(t1, t2)
  colSums(t(mapply(function(r1, r2) {.count.contingency.pair(r1, r2)}, unlist(t1), unlist(t2))))

# return the contingency table for a single matched pair of data
.count.contingency.pair <- function(r1, r2) {
  contingency <- c(ak = 0, bk = 0, ck = 0, dk = 0)
  idx <- 1 + 2*as.integer(!as.logical(r1)) + as.integer(!as.logical(r2))
  replace(contingency, idx, contingency[idx] + 1)
}

# test that a number is numerically integral, without regard to it's storage type
.is.whole <- function(x) all(is.numeric(x) & floor(x)==x)