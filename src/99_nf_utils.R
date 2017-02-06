
top100_pkts_num <- function(traf, threshold) {
  # Find champions by pkts
  threshold <- 100
  x <- aggregate(trafIn$flow_pkts, by=list(trafIn$dst_ip), FUN=sum)
  names(x) <- c('ip', 'value'); x <- x[order(x$value, decreasing = TRUE),]
  x <- x[x$value > threshold,]
  
  return(x)
}