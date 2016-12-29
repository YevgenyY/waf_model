### Prepare data for ML
setwd('~/work/waf_model')
load("data/ip_url_tdiff.Rda")

# remove records with tdiff > 5 min
df <- df[!df$tdiff > 300,]
df <- df[df$ip != '127.0.0.1',]
df <- df[,c(1,3,4)]
df <- droplevels(df)

### example of spread function
#a <- c(rep(1:10, 3))
#b <- c(rep("aa", 10), rep("bb", 10), rep("cc", 10))
#set.seed(123)
#c <- sample(seq(from = 20, to = 50, by = 5), size = 30, replace = TRUE)
#d <- data.frame(a,b, c)
# spread(d, key = b, value = c)

#dd <- df[1:1000,]
#dd$id <- 1:nrow(dd)

### Drop rarely used URLs
library(plyr)
urls <- count(df, 'url')
urls <- urls[order(-urls$freq),]
save(urls, file="data/urls.Rda")
urls <- urls[urls$freq >= 500,]

# select only frequently used urls
df <- df[df$url %in% urls$url,]

### Add urls as columns
library(tidyr)
df$id <- 1:nrow(df)
dml <- spread(df, key = url, value = tdiff)
dd <- dml[dml$ip == '10.100.104.141',]
dd <- dd[,!all(is.na(dd[1,c(3:dim(dd)[2])]))]
dd <- droplevels(dd)

save(dml, file="data/data_ready.Rda")

### TDIFF only
library(plyr)
df <- df[,c(1,3)]
dd <- split(df, df$ip)

ips <- data.frame()
tdiff <- data.frame()

for (i in 1:length(dd)) {
  ip <- as.data.frame( as.character( dd[[i]]$ip[1] ) )
  names(ip) <- "ip"
  x <- data.frame(dd[[i]]$tdiff)
  
  ips <- rbind(ips, ip)
  tdiff <- rbind.fill(tdiff, as.data.frame(t(x)))
  
  print(i)
  
}

save(ips, tdiff, file="ip_tdiff.Rda")

y <- lapply(dd, function(x) as.character(x$ip[1]))


























