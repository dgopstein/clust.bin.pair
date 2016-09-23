#' @export
paired.to.contingency <- function(x, group.names, pre.measure.name, post.measure.name) {
  `%>%` <- dplyr::`%>%`
  
  x %>%
    dplyr::group_by_(.dots = group.names) %>%
    dplyr::summarize_(
      nk = ~n(),
      ak = lazyeval::interp(~sum( pre &  post), pre = as.name(pre.measure.name), post = as.name(post.measure.name)),
      bk = lazyeval::interp(~sum( pre & !post), pre = as.name(pre.measure.name), post = as.name(post.measure.name)),
      ck = lazyeval::interp(~sum(!pre &  post), pre = as.name(pre.measure.name), post = as.name(post.measure.name)),
      dk = lazyeval::interp(~sum(!pre & !post), pre = as.name(pre.measure.name), post = as.name(post.measure.name)))
}

# return the index into [a, b, c, d] to increment
count.contingency.row <- function(r1, r2) 1 + 2*as.integer(!as.logical(r1)) + as.integer(!as.logical(r2))

# fill a vector [ak, bk, ck, dk] with the counts from matched pair data [t1, t2]
count.contingency <- function(t1, t2) {
  contingency <- c(ak = 0, bk = 0, ck = 0, dk = 0)
  
  #Reduce(function(cntgy, row) {cntgy[count.contingency.row(row[1], row[2])])}, cbind(t1, t2), contingency)
  
  update.contingency <- function(cntgy, r1, r2) {
    idx <- count.contingency.row(r1, r2)
    replace(cntgy, idx, cntgy[idx] + 1)
  }

  colSums(t(mapply(function(r1, r2) {update.contingency(contingency, r1, r2)}, unlist(t1), unlist(t2))))
}

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