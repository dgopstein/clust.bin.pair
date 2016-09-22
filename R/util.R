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
count.contingency.row <- function(r1, r2) 1 + 2*as.integer(as.logical(r1)) + as.integer(as.logical(r2))

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

#' @export
nested.to.contingency <- function(x, id.name, response1.name, response2.name) {
  mapply(count.contingency, x[response1.name], x[response2.name])
} 

nested.to.contingency1 <- function(x, id.name, response1.name, response2.name) {
  if (!id.name %in% names(x)) {
    stop(paste0("id column '", id.name, "' not in x"))
  } else if (!response1.name %in% names(x)) {
    stop(paste0("response1.name column '", response1.name, "' not in x"))
  } else if (!response2.name %in% names(x)) {
    stop(paste0("response2.name column '", response2.name, "' not in x"))
  }
  
  df <- data.frame(x)
  df[[response1.name]] <- sapply(x[[response1.name]], function(x) data.frame(x), simplify=FALSE)
  df[[response2.name]] <- sapply(x[[response2.name]], function(x) data.frame(x), simplify=FALSE)
  
  r1 <- reshape::melt(df[[response1.name]], measure.vars=1)
  r2 <- reshape::melt(df[[response2.name]], measure.vars=1)
  
  assertthat::assert_that(all(r1$L1 == r2$L1))
  
  grouped <- data.frame(cbind(group = r1$L1, t1 = r1$value, t2 = r2$value))
  grouped.indexed <- plyr::ddply(grouped, 'group', transform, idx = seq_along(group))
  merged <- merge(x = df, y = grouped, by.x=0, by.y='group')[, -1]
  
  paired.to.contingency(merged, id.name, "t1", "t2")
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