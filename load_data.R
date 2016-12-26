setwd('~/work/waf_model')

df <- read.csv('data/log.csv',header = FALSE, stringsAsFactors = F)
names(df) <- c('ip', 'time', 'url')
#df <- df[,c(1,2,4)]

Sys.setlocale("LC_TIME", "en_US.UTF-8")  # Modern Linux etc.
df$time <- strptime(df$time, "%d/%b/%Y:%H:%M:%S")
#df$ip <- factor(df$ip)
#df$url <- factor(df$url)

df$ip <- factor(df$ip)

### split dataframe by ip adresses
X <- split(df, df$ip)
rm(df) # save RAM

### count delays between clicks
Y <- X
TD <- lapply(Y, function(x) abs(c(0,diff(x$time, lag=1))))
X <- mapply(cbind, Y, tdiff = TD, SIMPLIFY = FALSE)

### merge data in one dataframe
#library(plyr)
#dd <- ldply(X, rbind)

df <- X[[1]]
for (i in 2:length(X)) {
  df <- rbind(df, X[[i]])
  print(i)
}
save(df, file="data/ip_url_tdiff.Rda")

