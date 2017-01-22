setwd('~/work/waf_model')

############# Load data #####################
#df <- read.csv(fileName, header = FALSE, stringsAsFactors = F, nrows=10000)
fileName <- 'data/log.csv'
df <- read.csv(fileName, header = FALSE, stringsAsFactors = F)
names(df) <- c('ip', 'time', 'url', 'ua', 'type')

types <- unique(df$type)

df <- df[,c(1,5)]
X <- split(df, df$ip)

Y <- lapply(X, function(x) aggregate(x, by=list(x$type), length))

dml <- data.frame(matrix(data=NA, nrow=0, ncol=length(types)))
names(dml) <- types

# Make dataframe
library(plyr)
ips <- names(Y)
for (i in 1:length(Y)) {
  x <- Y[[i]]
  x_types  <- x$Group.1 # pdf, html, jpg ...
  x_counts <- x$type    # number of clicks
  
  # Construct ip object with number of clicks by each resource type
  ip_obj <- data.frame(matrix(x_counts, nrow=1, ncol=length(x_types)))
  names(ip_obj) <- x_types

  # Make resulting data frame
  dml <- rbind.fill(dml, ip_obj)  
}

# Normalize numbers
dml[is.na(dml)] <- 0
dml_sum <- apply(dml, 1, sum)
dml <- round(dml / dml_sum, 2)
dml$all <- dml_sum

# Add ip addresses
dml <- cbind(ips, dml)

# Add labels
### set labels according to robots.txt ###
bots <- read.csv('data/robots_ip.txt',header = FALSE, 
                stringsAsFactors=FALSE, sep = " ")
names(bots) <- "ip"
bots$ip <- as.character(bots$ip)

# set labels
dml$bot <- "man"
dml$bot[dml$ip %in% bots$ip] <- "bot"

############# Analyze data #####################

# divide characters and numbers
bot <- dml[dml$bot=='bot',]
bot_ip <- bot[,c(1, dim(bot)[2]-1, dim(bot)[2])]
bot_tp <- bot[,-c(1, dim(bot)[2]-1, dim(bot)[2])]

man <- dml[dml$bot!='bot',]
man_ip <- man[, c(1, dim(man)[2]-1, dim(man)[2])]
man_tp <- man[,-c(1, dim(man)[2]-1, dim(man)[2])]

sapply(bot_tp, function(x) round(mean(x),2))
sapply(man_tp, function(x) round(mean(x),2))

sapply(bot_tp, function(x) round(sd(x),2))
sapply(man_tp, function(x) round(sd(x),2))

# plot bot vs man
library(reshape2)
library(ggplot2)
ggplot(data = melt(dml), mapping = aes(x = value)) + aes(fill=bot) +
  geom_histogram(bins = 10) + facet_wrap(~variable, scales = 'free_x')

mean_bot <- as.data.frame(sapply(bot_tp, function(x) round(mean(x),2)))
mean_bot$bot <- 'bot'
names(mean_bot) <- c('value', 'bot')

mean_man <- as.data.frame(sapply(man_tp, function(x) round(mean(x),2)))
mean_man$bot <- 'bot'
names(mean_man) <- c('value', 'bot')

#ggplot(data = rbind(mean_man, mean_bot))

# plot bot vs man
library(reshape2)
library(ggplot2)
ggplot(data = melt(dml), mapping = aes(x = value)) + aes(fill=bot) +
  geom_histogram(bins = 10) + facet_wrap(~variable, scales = 'free_x')

############# Train model #####################
library(caret)
inTrain <- createDataPartition(y=dml$bot, times=1, 
                               p=0.75, list=FALSE)
train <- dml[inTrain,]
test <- dml[-inTrain, ]

### Normalize data
bot <- train[train$bot=='bot',]
man <- train[train$bot!='bot',]

#plot(man$ip_mean, man$ip_sd)
#plot(bot$ip_mean, bot$ip_sd)
#ggplot(dml, aes(x=ip_mean, y=ip_sd, colour=bot)) + geom_point(size=2)

co <- dim(bot)[1]/dim(man)[1]
inTrain <- createDataPartition(y=man$bot, times=1, 
                               p=co, list=FALSE)

train_norm <- rbind(bot, man[inTrain,])

### train model #########################################
mod <- train(bot ~ ., method="glm", data=train)
summary(mod)

pred <- predict(mod, test)
cfm <- confusionMatrix(test$bot, pred)
cfm


# show FP, FN
show_fp(test, pred, 2)
show_fn(test, pred, 3) 

pred <- predict(mod, train)
cfm <- confusionMatrix(train$bot, pred)
cfm




























