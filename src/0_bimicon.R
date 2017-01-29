library(RMySQL)
setwd('~eugen/work/waf_model')
## Load data from remote website
#mydb = dbConnect(MySQL(), user='stats', password='', dbname='nginx_log', host='bimicon.de')
#rs = dbSendQuery(mydb, "select * from ngx_log")
#bimicon_data = fetch(rs, n=-1)
#on.exit(dbDisconnect(mydb))
#save(bimicon_data, file="data/bimicon.Rdb")

load("data/bimicon.Rdb")

df <- bimicon_data[,c(4,15,6,9)]
names(df) <- c('ip', 'time', 'url', 'ua')

url <- df$url[2]

strsplit(url, '.', fixed = FALSE, perl = true, useBytes = FALSE)

for (i in 1:length(df$url)) {
  str_url <- strsplit(df$url[i], " ")[[1]][1]
  print(str_url)
  print(i)
  
  if (is.na(str_url)) {
    df$type[i] <- 'dir'
    next
  }
  
  buf <- strsplit(str_url, '\\.')
  url_last <- buf[[1]]
  exten <- url_last[length(url_last)]
  buf <- strsplit(exten, '\\?')
  tmp <- tolower(buf[[1]][1])
  tmp <- gsub("[^[:alnum:]=\\.]", "", tmp)
  
  if (tmp == '' || tmp == '/' || length(url_last) == 1 || nchar(tmp) > 5) {
    tmp <- 'dir'
  }
  
  df$type[i] <- tmp
}

bimicon_data_parsed <- df
save(bimicon_data_parsed, file='data/bimicon_parsed.Rda')


### Extract bot's IPs
load('data/bimicon_parsed.Rda')
df <- bimicon_data_parsed

bot_idx <- grep('bot', ignore.case = TRUE, df$ua)
bot <- df[bot_idx,]$ip
bot_idx2 <- grep('robot', df$url)
bot2 <- df[bot_idx2,]$ip
bot <- unique(c(bot, bot2)); rm(bot2, bot_idx, bot_idx2, bimicon_data_parsed)


################# Try second model based on tdiff ########################
df <- df[,c(1,2,3)]
Sys.setlocale("LC_TIME", "en_US.UTF-8")  # Modern Linux etc.
df$time <- strptime(df$time, "%Y-%m-%d %H:%M:%S")

################# Make TDIFF
### split dataframe by ip adresses
X <- split(df, df$ip)
#rm(df) # save RAM

### count time delays between clicks
Y <- X
TD <- lapply(Y, function(x) abs(c(0,diff(x$time, lag=1))))

X <- mapply(cbind, Y, tdiff = TD, SIMPLIFY = FALSE)

### merge data in one dataframe
#library(plyr)
#dd <- ldply(X, rbind)

df <- X[[1]]
for (i in 2:length(X)) {
  df <- rbind(df, X[[i]])
  print(paste(i, length(X), sep = " "))
}
save(df, file='data/bimicon_tdiff.Rda')

######################### Analyse data ####################
### TDIFF only
load('data/bimicon_tdiff.Rda')
library(plyr)
df <- df[,c(1,3)]
l_IP <- split(df, df$ip)

### Calculate features
IP_ADDRS <- unlist(lapply(l_IP, function(x) as.character(x$ip[1])), use.names = FALSE)
COUNT <- unlist(lapply(l_IP, function(x) dim(x)[1]), use.names = FALSE)
MEDIAN <- unlist(lapply(l_IP, function(x) mean(x$tdiff)), use.names = FALSE)
SD <- unlist(lapply(l_IP, function(x) sd(x$tdiff)), use.names = FALSE)

ips <- as.data.frame(ips)
ip_click_counts <- as.data.frame(click_counts)

### Put it into a single dataframe
dml <- data.frame(ip=IP_ADDRS, ip_mean=MEDIAN, ip_sd=SD, count=COUNT)
dml <- dml[complete.cases(dml),]

# set label, 1 means - ip is a bot
dml$bot <- "man"
dml$bot[dml$ip %in% bot] <- "bot"

# remove outliers
dml <- dml[dml$ip_mean < 300,]
dml <- dml[dml$ip_sd < 300,]

library(ggplot2)
ggplot(dml, aes(x=ip_mean, y=ip_sd, colour=bot)) + geom_point(size=2)

### TODO: check labels again

