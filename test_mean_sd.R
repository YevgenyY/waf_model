setwd('~/work/waf_model')

bulk <- load_data('data/log.csv')
bulk <- bulk[!bulk$tdiff > 300,]
save(bulk, file='data/bulk.Rda')
load("data/ip_ua.Rda") # load ip address - UserAgent

dml <- make_ip_tdiff(bulk)
# Create training and testing datasets
library(caret)
inTrain <- createDataPartition(y=dml$bot, times=1, 
                               p=0.75, list=FALSE)
train <- dml[inTrain,]
test <- dml[-inTrain, ]

# train model
mod <- train(bot ~ ip_mean + ip_sd, family = binomial(link='logit'), data=train)
summary(mod)


pred <- predict(mod, test)
cfm_median <- confusionMatrix(test$bot, pred)
cfm_median

# show FP + FN
test[test$bot!=pred,]
show_ua(test[test$bot!=pred,]$ip[1])

# show FP
x <- test$bot!=pred
y <- test[x,]
y[y$bot == 'man',]
ip <- y[y$bot == 'man',]$ip[4]
show_ua(ip)
plot_ip_tdiff(ip)

# show FN
x <- dml$bot!=pred
y <- dml[x,]
y[y$bot == 'bot',]
ip <- y[y$bot == 'bot',]$ip[12]
show_ua(ip)
plot_ip_tdiff(ip)

# manual checking
n <- which(test$ip=='178.6.246.231')
test[n,]
pred[n]

