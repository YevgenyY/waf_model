#library(RMySQL)
## Load data from remote website
#mydb = dbConnect(MySQL(), user='stats', password='dflfgd94w', dbname='nginx_log', host='bimicon.de')
#rs = dbSendQuery(mydb, "select * from ngx_log")
#data = fetch(rs, n=-1)
#on.exit(dbDisconnect(mydb))
#save(data, file="bimicon.Rdb")

load("bimicon.Rdb")

