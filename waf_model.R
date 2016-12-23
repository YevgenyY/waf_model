setwd('~/work/grcc/waf_model')

df <- read.csv('data/log.csv',header = FALSE, stringsAsFactors = F)
names(df) <- c('ip', 'time', 'method', 'url', 'ua')

Sys.setlocale("LC_TIME", "en_US.UTF-8")  # Modern Linux etc.
df$time <- strptime(df$time, "%d/%b/%Y:%H:%M:%S")
df$ip <- factor(df$ip)

# split dataframe by ip adresses
X <- split(df, df$ip)
Y <- X

rm(df)

# Calculate time diff for each click of each IP
TD <- lapply(Y, function(x) abs(c(0,diff(x$time, lag=1))))
X <- mapply(cbind, Y, tdiff = TD, SIMPLIFY = FALSE)

# Find search engines
n <- names(X)
TEST <- 1
GOOG <- which(n=='66.249.66.92')
BING <- which(n=='207.46.13.40')
BING1 <- which(n=='157.55.39.15')
BING2 <- which(n=='157.55.39.156')
BING3 <- which(n=='157.55.39.18')
BING4 <- which(n=='157.55.39.70')
BING5 <- which(n=='207.46.13.104')
BING6 <- which(n=='207.46.13.130')
YNDX <- which(n=='93.158.152.34')
IFAX <- which(n=='109.68.190.135')
UNKNOWN_BOT <- which(n=='109.95.210.3')
WGET <- which(n=='127.0.0.1')
BLEXBOT <- which(n=='136.243.36.99')
MJ12BOT <- which(n=='141.95.0.52')
MJ12BOT1 <- which(n=='144.76.61.21')
MJ12BOT1 <- which(n=='144.76.7.107')
MJ12BOT3 <- which(n=='151.80.44.115')
MJ12BOT4 <- which(n=='162.210.196.98')
MJ12BOT5 <- which(n=='163.172.49.61')
MJ12BOT6 <- which(n=='173.234.159.250')
MJ12BOT7 <- which(n=='178.63.86.11')
MJ12BOT8 <- which(n=='192.187.104.235')
MJ12BOT9 <- which(n=='193.111.140.106')
MJ12BOT10 <- which(n=='195.154.187.115')
CR007BOT <- which(n=='188.165.233.228')
WATCHMANBOT <- which(n=='195.93.246.48')

IND <- GOOG

# have a look at the time difference of clicks distribution
library(ggplot2)
ggplot(X[[IND]], aes(x=tdiff)) + geom_histogram()
ggplot(X[[IND]], aes(x=tdiff)) + geom_density()
median(X[[IND]]$tdiff)

# Calculate median
N <- data.frame(cbind(names(X),lapply(X, function(x)mean(x$tdiff))))
N[is.na(N)] <- 0
N$X2 <- unlist(N$X2)
ggplot(N, aes(x=log10(X2))) + geom_density()

# Remove outliers
THRESHOLD <- 300
Z <- lapply(X, function(x) x[!is.na(x$tdiff),] )
Z <- lapply(Z, function(x) x[!x$tdiff == 0,] )
X <- lapply(Z, function(x) x[!x$tdiff >= THRESHOLD,] )

# Calculate frequency
Z <- lapply(X, function(x) table(x$tdiff))

# Plot time difference frequency
IND <- TEST
G <- data.frame(cbind(as.numeric(names(Z[[IND]])), Z[[IND]]))
names(G) <- c("tdiff", "freq")
ggplot(G, aes(x=tdiff, y=freq)) + geom_point()

# Calculate number of clicks performed by users
sum <- lapply(Z, function(x) sum(x))

vec <- sum > 50
h50 <- names(sum[vec])
Y <- X[h50] # hosts which made more than 50 clicks
Z <- lapply(Y, function(x) table(x$tdiff))

### Find bot's manually by analyzing the curve and UserAgent
idx <- 75
G <- data.frame(cbind(as.numeric(names(Z[[idx]])), Z[[idx]]))
names(G) <- c("tdiff", "freq")
ggplot(G, aes(x=tdiff, y=freq)) + geom_point() + ggtitle(names(Y[idx]))
Y[[idx]][5] # Print UserAgent

###
click_count <- lapply(X, function(x) dim( x[1])[1] )
h50 <- click_count > 50
Y <- X[h50]








