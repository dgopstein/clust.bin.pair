library(CorrBin)

data(dehp)
dehp
data(egde)
egde

confusion <- read.csv("confusion.csv")
save(confusion, file="data/confusion.RData")

# devtools::create('clust.bin.matched.pair')
