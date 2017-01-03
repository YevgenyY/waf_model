setwd('~/work/waf_model')

load_data <- function(fileName) {
  df <- read.csv(fileName, header = FALSE, stringsAsFactors = F)
  names(df) <- c('ip', 'time', 'url', 'ua')
  
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
    print(i, length(X))
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
  ip_mean <- as.data.frame(apply(tdf, 1, median))
  ip_sd <- as.data.frame(apply(tdf, 1, sd))
  dml <- cbind(ips, ip_mean, ip_sd)
  names(dml) <- c("ip", "ip_mean", "ip_sd")
  
  ### set labels according to access to robots.txt ###
  tmp <- read.csv('data/robots_ip.txt',header = FALSE, 
                  stringsAsFactors=FALSE, sep = " ")
  bots <- as.data.frame(unique(tmp$V8))
  names(bots) <- "ip"
  bots$ip <- as.character(bots$ip)
  rm(tmp)
  
  # set label, 1 means - ip is a bot
  dml$bot <- "man"
  dml$bot[dml$ip %in% bots$ip] <- "bot"
  
  return(dml)
}

df <- load_data("data/log.csv")
droplevels(df)
save(df, ip_ua, file="data/ip_url_tdiff.Rda")

