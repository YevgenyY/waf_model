### Prepare data for ML
setwd('~/work/waf_model')
load("data/ip_url_tdiff.Rda")

bulk <- df
df <- bulk[1:200000,]

# remove records with tdiff > 5 min
df <- df[!df$tdiff > 300,]
df <- df[df$ip != '127.0.0.1',]
df <- df[,c(1,3,4)]
df <- droplevels(df)

### TDIFF only
library(plyr)
df <- df[,c(1,3)]
l_IP <- split(df, df$ip)

###
y <- lapply(l_IP, function(x) as.character(x$ip[1]))
ip_addrs <- unlist(y, use.names = FALSE)

length_lt_threshold <- function(x) {
  if (length(x) >= 100)
    x[1:100]
}
### Select ip with more than 100 clicks ###
y <- lapply(l_IP, function(x) x$tdiff)
selected <- lapply(y, length_lt_threshold)

tdf <- data.frame() # data frame with tdiff for first 100 clicks
ips <- c() # ip address

for (i in 1:length(selected)) {
  if ( length(selected[[i]]) > 0 ) {
    tmp <- as.data.frame(t(selected[[i]]))
    tdf <- rbind(tdf, tmp)
    
    ips <- c(ips, names(selected[i]))
    
    # print(tmp)
  }
  print(i)
}
ips <- as.data.frame(ips)

### Calculate mean and sd for each ip
ip_mean <- as.data.frame(apply(tdf, 1, mean))
ip_sd <- as.data.frame(apply(tdf, 1, sd))
dml <- cbind(ips, ip_mean, ip_sd)
names(dml) <- c("ip", "ip_mean", "ip_sd")

### set labels according to access to robots.txt

### Utils ###
show_ua <- function(x) {
  print(ip_ua[ip_ua$ip==x,])
}

plot_ip_tdiff <- function(x) {
  ip_tdiff <- bulk[bulk$ip==x,]
  ggplot(ip_tdiff, aes(x=tdiff)) + geom_histogram()
}

