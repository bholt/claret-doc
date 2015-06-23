#!/usr/bin/env Rscript
source('common.r')
a <- parse.args()

d <- data.rubis(where="
  duration = 60 and name like 'v0.21%' and nclients = 4 and nthreads = 32 and rate = 100
")
d1 <- d[1,]
l1 <- as.list(fromJSON(jsfix(d1$server_bid_time_hist)))
df1 <- data.frame(x=num(names(l1))+0.02, y=vals(l1))
df1 <- df1[order(df1$x),]
df1v <- as.vector(rep(df1$x,  df1$y))

save(
  ggplot(data.frame(x=df1v, aes(x=x))+
    # geom_histogram(binwidth=0.02, fill=c.blue)+
    stat_ecdf(color=c.blue)+
    # geom_line()+
    # geom_density(fill=NA, color=c.blue, adjust=1/2)+
    scale_x_continuous(limits=c(0,1.0))+
    xlab('Percentage auction time')+ylab('# of bids')+
    theme_mine
, w=3.4, h=3.0)
