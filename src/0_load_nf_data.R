setwd('~/work/waf_model')

bulk <- read.csv('data/netflow.log', sep=',')
names(bulk) <- c('src_ip','src_port','dst_ip','dst_port', 'flow_octets', 'flow_pkts', 'ip_proto',
                 'in_pkts','out_pkts', 'tcp_in_pkts', 'tcp_out_pkts',
                 'tcp_syn_in_pkts', 'tcp_syn_out_pkts', 'tcp_in_bytes', 'tcp_out_bytes',
                 'udp_in_pkts', 'udp_out_pkts', 'udp_in_bytes', 'udp_out_bytes',
                 'icmp_in_pkts', 'icmp_out_pkts', 'icmp_in_bytes', 'icmp_out_bytes',
                 'in_flows', 'out_flows'
                 )
save(bulk, file='data/nf_bulk.Rda')