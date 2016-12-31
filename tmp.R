### Prepare data for ML
setwd('~/work/waf_model')
load("data/ip_url_tdiff.Rda")

# remove records with tdiff > 5 min
df <- df[!df$tdiff > 300,]
df <- df[df$ip != '127.0.0.1',]
df <- df[,c(1,3,4)]
df <- droplevels(df)

### TDIFF only
library(plyr)
df <- df[,c(1,3)]
dd <- split(df, df$ip)

###
y <- lapply(dd, function(x) as.character(x$ip[1]))
ip_addrs <- unlist(y, use.names = FALSE)

length_lt_threshold <- function(x) {
  if (length(x) >= 100)
    x[1:100]
  else 
    NA
}
y <- lapply(dd, function(x) x$tdiff)
selected <- lapply(y, length_lt_threshold)

tdiff <- data.frame()

for (i in 1:length(dd)) {
  tdiff <- rbind.fill(tdiff, as.data.frame(t(y[[i]])))
  print(i)
}