df <- read.csv('data/log.csv',header = FALSE, stringsAsFactors = F)
names(df) <- c('ip', 'time', 'method', 'url')

Sys.setlocale("LC_TIME", "en_US.UTF-8")  # Modern Linux etc.
df$time <- strptime(df$time, "%d/%b/%Y:%H:%M:%S")

# unique ip and url
uip <- unique(df$ip)
uurl <- unique((tolower(df$url)))


