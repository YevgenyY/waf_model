setwd('~/work/waf_model')

load_data <- function(fileName) {
  #df <- read.csv(fileName, header = FALSE, stringsAsFactors = F, nrows=10000)
  df <- read.csv(fileName, header = FALSE, stringsAsFactors = F)
  names(df) <- c('ip', 'time', 'url', 'ua', 'type')
  
  # save UserAgent
  ip_ua <- unique(df[,c(1,4)])
  save(ip_ua, file="data/ip_ua.Rda")
  
  df <- df[,c(1,2,3)]
  Sys.setlocale("LC_TIME", "en_US.UTF-8")  # Modern Linux etc.
  df$time <- strptime(df$time, "%d/%b/%Y:%H:%M:%S")
  #df$ip <- factor(df$ip)
  #df$url <- factor(df$url)
  #df$ip <- factor(df$ip)
  
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

  return(df)
}

make_ip_tdiff <- function(df) {
  # remove records with tdiff > 5 min
  df <- df[!df$tdiff > 300,]
  df <- df[df$ip != '127.0.0.1',]
  df <- df[,c(1,3,4)]
  df <- droplevels(df)
  
  ### TDIFF only
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

  ### set labels according to access to robots.txt ###
  tmp <- read.csv('data/robots_ip.txt',header = FALSE, 
                  stringsAsFactors=FALSE, sep = " ")
  bots <- as.data.frame(unique(tmp$V1))
  names(bots) <- "ip"
  bots$ip <- as.character(bots$ip)
  rm(tmp)
  
  # set label, 1 means - ip is a bot
  dml$bot <- "man"
  dml$bot[dml$ip %in% bots$ip] <- "bot"
  
  return(dml)
}

#df <- load_data("data/log.csv")
#droplevels(df)
#save(df, ip_ua, file="data/ip_url_tdiff.Rda")

