
top100_ip <- function(x, y, threshold = 100, fun=sum) {
  # Find top100 ip addresses by packets number or octets
  #
  # Args:
  #   x: Dataframe
  #   names(df)
  #     [1] "src_ip"      "src_port"    "dst_ip"      "dst_port"    
  #     "flow_octets" "flow_pkts"   "ip_proto" 
  #   y: data frame column number for aggregation
  #   threshold: number to remove IP addresses with low-rate of packets/octets 
  #   fun: aggregation function
  
  z <- aggregate(x[,y], by=list(x$dst_ip), FUN=fun)
  
  names(z) <- c('ip', 'value'); z <- z[order(z$value, decreasing = TRUE),]
  z <- z[z$value > threshold,]
  
  return(z)
}

aggregate_ip <- function(x, y, fun=sum) {
  # Aggregate ip addresses featuress
  #
  # Args:
  #   x: Dataframe
  #   names(df)
  #     [1] "src_ip"      "src_port"    "dst_ip"      "dst_port"    
  #     "flow_octets" "flow_pkts"   "ip_proto" 
  #   y: data frame column number for aggregation
  #   threshold: number to remove IP addresses with low-rate of packets/octets 
  #   fun: aggregation function
  
  z <- aggregate(x[,y], by=list(x$dst_ip), FUN=fun)
  names(z) <- c('ip', 'value')
  
  return(z$value)
}