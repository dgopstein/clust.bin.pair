#' @export
paired.to.contingency <- function(group, t1, t2)
  stats::aggregate(t(mapply(count.contingency.row, t1, t2)), by=group, FUN=sum)

#' @export
nested.to.contingency <- function(t1, t2)
  t(apply(cbind(t1, t2), 1, function(x) count.contingency(x[1], x[2])))

# return the index into [a, b, c, d] to increment
count.contingency.row <- function(r1, r2) {
  contingency <- c(ak = 0, bk = 0, ck = 0, dk = 0)
  idx <- 1 + 2*as.integer(!as.logical(r1)) + as.integer(!as.logical(r2))
  replace(contingency, idx, contingency[idx] + 1)
}

# fill a vector [ak, bk, ck, dk] with the counts from matched pair data [t1, t2]
count.contingency <- function(t1, t2)
  colSums(t(mapply(function(r1, r2) {count.contingency.row(r1, r2)}, unlist(t1), unlist(t2))))

has.col.name <- function (x, name) name %in% colnames(x) || name %in% names(x)

.mcnemar.test <- function(ak, bk, ck, dk) .mcnemar.impl(bk,ck)

.mcnemar.impl <- function(bk, ck) {
  b <- sum(bk)
  c <- sum(ck)
  (b - c)^2/(b + c)
}