library(dplyr)
library(plyr)
library(lazyeval)

paired.to.contingency <- function(x, group.names, pre.measure.name, post.measure.name) {
  x %>%
    group_by_(.dots = group.names) %>%
    summarize_(
      nk = ~n(),
      ak = interp(~sum( pre &  post), pre = as.name(pre.measure.name), post = as.name(post.measure.name)),
      bk = interp(~sum( pre & !post), pre = as.name(pre.measure.name), post = as.name(post.measure.name)),
      ck = interp(~sum(!pre &  post), pre = as.name(pre.measure.name), post = as.name(post.measure.name)),
      dk = interp(~sum(!pre & !post), pre = as.name(pre.measure.name), post = as.name(post.measure.name)))
}

nested.to.contingency <- function(x, id.name, response1.name, response2.name) {
  df <- data.frame(x)
  df[[response1.name]] <- sapply(x[[response1.name]], function(x) data.frame(x), simplify=FALSE)
  df[[response2.name]] <- sapply(x[[response2.name]], function(x) data.frame(x), simplify=FALSE)
  
  r1 <- melt(df[[response1.name]], measure.vars=1)
  r2 <- melt(df[[response2.name]], measure.vars=1)
  
  assertthat::assert_that(all(r1$L1 == r2$L1))
  
  grouped <- data.frame(cbind(group = r1$L1, t1 = r1$value, t2 = r2$value))
  grouped.indexed <- ddply(grouped, 'group', transform, idx = seq_along(group))
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