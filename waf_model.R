setwd('~/work/grcc/waf_model')

df <- read.csv('data/log.csv',header = FALSE, stringsAsFactors = F)
names(df) <- c('ip', 'time', 'method', 'url')

Sys.setlocale("LC_TIME", "en_US.UTF-8")  # Modern Linux etc.
df$time <- strptime(df$time, "%d/%b/%Y:%H:%M:%S")
df$ip <- factor(df$ip)

# split dataframe by ip adresses
X <- split(df, df$ip)
Y <- X

# Count time diff for each click of each IP
z <- lapply(Y, function(x) abs(c(0,diff(x$time, lag=1))))

R <- mapply(c, Y, Z, SIMPLIFY = FALSE)

# Find time delays between clicks
Z <- X[[150]]
Z$diff <- abs(c(0,diff(Z$time, lag=1)))
ggplot(Z, aes(x=diff)) + geom_histogram()

ggplot(Z, aes(x=diff)) + geom_density()





