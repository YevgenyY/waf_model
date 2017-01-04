setwd('~/work/waf_model')

#bulk <- load_data('data/log.csv')
#save(bulk, file='data/bulk.Rda')

load('data/bulk.Rda')
#load("data/ip_ua.Rda") # load ip address - UserAgent

dml <- make_ip_tdiff(bulk)
save(dml, ip_ua, file="data/dml_ip_ua.Rda")
# Create training and testing datasets
library(caret)
inTrain <- createDataPartition(y=dml$bot, times=1, 
                               p=0.75, list=FALSE)
train <- dml[inTrain,]
test <- dml[-inTrain, ]

### Normalize data
bots <- train[train$bot=='bot',]
man <- train[train$bot!='bot',]
co <- dim(bots)[1]/dim(man)[1]
inTrain <- createDataPartition(y=man$bot, times=1, 
                               p=co, list=FALSE)

train_norm <- rbind(bots, man[inTrain,])

### train model #########################################
mod <- train(bot ~ ip_mean + ip_sd + count, method="glm", data=train_norm)
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

# show FP + FN
test[test$bot!=pred,]
show_ua(test[test$bot!=pred,]$ip[1])

# show FP
x <- test$bot!=pred
y <- test[x,]
y[y$bot == 'man',]
ip <- y[y$bot == 'man',]$ip[9]
show_ua(ip)
plot_ip_tdiff(ip)
cmp_ip(ip)

# show FN
x <- test$bot!=pred
y <- test[x,]
y[y$bot == 'bot',]
ip <- y[y$bot == 'bot',]$ip[3]
show_ua(ip)
plot_ip_tdiff(ip)
cmp_ip(ip)

#### Utils

# show FP
show_fp <- function(data, pred, i) {
  x <- data$bot!=pred
  y <- data[x,]
  y[y$bot == 'man',]
  ip <- y[y$bot == 'man',]$ip[i]
  show_ua(ip)
  plot_ip_tdiff(ip)
  cmp_ip(data, ip)
}

show_fn <- function(data, pred, i) {
  x <- data$bot!=pred
  y <- data[x,]
  y[y$bot == 'bot',]
  ip <- y[y$bot == 'bot',]$ip[i]
  show_ua(ip)
  plot_ip_tdiff(ip)
  cmp_ip(data, ip)
}

# compare test and pred
cmp_ip <- function(data, x) {
  n <- which(data$ip==x)
  print(test[n,])
  print(paste("Test values:", data[n,5], sep=" "))
  print(paste("Predicted value:", pred[n], sep=" "))
}

