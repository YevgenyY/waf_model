### Prepare data for ML
setwd('~/work/waf_model')
load("data/ip_url_tdiff.Rda")

# remove records with tdiff > 5 min
df <- df[df$tdiff > 300]


a <- c(rep(1:10, 3))
b <- c(rep("aa", 10), rep("bb", 10), rep("cc", 10))
set.seed(123)
c <- sample(seq(from = 20, to = 50, by = 5), size = 30, replace = TRUE)
d <- data.frame(a,b, c)

dr <- spread(dd, key = url, value = tdiff)

