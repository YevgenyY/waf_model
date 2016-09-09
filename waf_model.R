setwd('~/work/grcc/waf_model')

df <- read.csv('data/log.csv',header = FALSE, stringsAsFactors = F)
names(df) <- c('ip', 'time', 'method', 'url')

Sys.setlocale("LC_TIME", "en_US.UTF-8")  # Modern Linux etc.
df$time <- strptime(df$time, "%d/%b/%Y:%H:%M:%S")
df$ip <- factor(df$ip)

# split dataframe by ip adresses
X <- split(df, df$ip)
Y <- X

# Calculate time diff for each click of each IP
TD <- lapply(Y, function(x) abs(c(0,diff(x$time, lag=1))))
X <- mapply(cbind, Y, tdiff = TD, SIMPLIFY = FALSE)

# Find search engines
TEST <- 1
GOOG <- 4895 # 66.249.66.92
BING <- 2696 # 207.46.13.40
YNDX <- 6685 # 93.158.152.34
IND <- YNDX

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
G <- data.frame(cbind(as.numeric(names(Z[[IND]])), Z[[IND]]))
names(G) <- c("tdiff", "freq")
ggplot(G, aes(x=tdiff, y=freq)) + geom_point()

# Calculate number of clicks performed by users
sum <- lapply(Z, function(x) sum(x))

vec <- x>50
names(vec) <- c(1:length(vec))
ind <- names(vec)[vec]
ind










