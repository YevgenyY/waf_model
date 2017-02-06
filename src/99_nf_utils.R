
top100_ip <- function(x, y, threshold = 100, fun=sum) {
  # Find top100 ip addresses by packets number or octets
  #
  # Args:
  #   x: Dataframe
  #   names(df)
  #     [1] "src_ip"      "src_port"    "dst_ip"      "dst_port"    
  #     "flow_octets" "flow_pkts"   "ip_proto" 
  #   y: 0 - packets number
  #      1 - octets
  #   threshold: number to remove IP addresses with low-rate of packets/octets 
  
  if (y == 0) {
    z <- aggregate(x$flow_pkts, by=list(x$dst_ip), FUN=fun)
  } else {
    z <- aggregate(x$flow_pkts, by=list(x$dst_ip), FUN=fun)
  }
  names(z) <- c('ip', 'value'); z <- z[order(z$value, decreasing = TRUE),]
  z <- z[z$value > threshold,]
  
  return(z)
}