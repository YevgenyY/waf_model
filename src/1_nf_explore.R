setwd('~/work/waf_model/')
source('src/99_nf_utils.R')
library(plyr)
load('data/nf_bulk.Rda')

######## Deal with netflow data #################
# extract & clean data from errors
df <- bulk[,c(1,2,3,4,5,6,7)]
df <- df[grepl("([0-9]{1,3}[\\.]){3}[0-9]{1,3}", df$src_ip),]
df <- df[grepl("([0-9]{1,3}[\\.]){3}[0-9]{1,3}", df$dst_ip),]

# find our dst
trafIn <- df[grepl('^10\\.', df$dst_ip),]
trafOut <- df[!grepl('^10\\.', df$dst_ip),]

# extract my IP addresses
myIP <- sort(unique(trafIn$dst_ip))
myIP

######## Calculate top100 src ip ################
top100InOcts <- top100_ip(trafIn, 5, threshold = 100, sum)
top100InPkts <- top100_ip(trafIn, 6, threshold = 100, sum)

# extract & clean data from errors
df <- bulk[,c(1,2,3,4, 8,9,10,11,14,15,16,17,18,19)]
df <- df[grepl("([0-9]{1,3}[\\.]){3}[0-9]{1,3}", df$src_ip),]
df <- df[grepl("([0-9]{1,3}[\\.]){3}[0-9]{1,3}", df$dst_ip),]

# find our dst
trafIn <- df[grepl('^10\\.', df$dst_ip),]
trafOut <- df[!grepl('^10\\.', df$dst_ip),]

trafIn[1,]
dst <- aggregate(trafIn[,5], by=list(x$dst_ip), function(x) round(mean(x),2)); names(dst) <- c('ip', 'pkt_mean_in')
dst$pkt_mean_out <- aggregate_ip(trafIn, 6, fun=median)
dst$pkt_median_in <- aggregate_ip(trafIn, 5, fun=median)
dst$pkt_median_out <- aggregate_ip(trafIn, 6, fun=median)
dst$pkt_in_sd <- aggregate_ip(trafIn, 5, fun=function(x) round(sd(x),2))
dst$pkt_out_sd <- aggregate_ip(trafIn, 6, fun=function(x) round(sd(x),2))

dst$pkt_tcp_in_mean <- aggregate_ip(trafIn, 7, fun=function(x) round(mean(x),2))
dst$pkt_tcp_out_mean <- aggregate_ip(trafIn, 8, fun=function(x) round(mean(x),2))
dst$pkt_tcp_in_median <- aggregate_ip(trafIn, 7, fun=function(x) round(median(x),2))
dst$pkt_tcp_out_median <- aggregate_ip(trafIn, 8, fun=function(x) round(median(x),2))
dst$pkt_tcp_in_sd <- aggregate_ip(trafIn, 7, fun=function(x) round(sd(x),2))
dst$pkt_tcp_out_sd <- aggregate_ip(trafIn, 8, fun=function(x) round(sd(x),2))

# order by pps mean
pps_sorted <- dst[order(dst$pkt_mean_in,decreasing = TRUE),] 
pps_sorted <- pps_sorted[complete.cases(pps_sorted),]

