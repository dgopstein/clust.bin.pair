#' @export
paired.to.contingency <- function(group, t1, t2) {
  contingencies <- t(mapply(count.contingency.row, t1, t2))

  aggregate(contingencies, by=group, FUN=sum)
}

# return the index into [a, b, c, d] to increment
count.contingency.row <- function(r1, r2) {
  contingency <- c(ak = 0, bk = 0, ck = 0, dk = 0)
  idx <- 1 + 2*as.integer(!as.logical(r1)) + as.integer(!as.logical(r2))
  replace(contingency, idx, contingency[idx] + 1)
}

# fill a vector [ak, bk, ck, dk] with the counts from matched pair data [t1, t2]
count.contingency <- function(t1, t2)
  colSums(t(mapply(function(r1, r2) {count.contingency.row(r1, r2)}, unlist(t1), unlist(t2))))

has.col.name <- function (x, name) {
  name %in% colnames(x) || name %in% names(x)
}

#' @export
nested.to.contingency <- function(response1, response2) {
  mapply(count.contingency, response1, response2)
  
  t(apply(cbind(response1, response2), 1, function(x) count.contingency(x[1], x[2])))
} 

.mcnemar.test <- function(ak, bk, ck, dk) {
   ak+dk
   .mcnemar.impl(bk,ck)
}

.mcnemar.impl <- function(bk, ck) {
  b <- sum(bk)
  c <- sum(ck)
  (b - c)^2/(b + c)
}