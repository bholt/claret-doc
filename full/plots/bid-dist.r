#!/usr/bin/env Rscript
source('common.r')
a <- parse.args()

d <- data.rubis(where="duration = 60 and name like 'v0.21%' and nclients = 4 and nthreads = 32 and rate = 100")
d1 <- d[1,]
l1 <- as.list(fromJSON(jsfix(d1$server_bid_time_hist)))
df1 <- data.frame(x=num(names(l1)), y=vals(l1))
save(
  ggplot(df1, aes(x=x,weight=y))+
    geom_histogram(binwidth=0.02, fill=c.blue)+
    # stat_ecdf()+
    xlab('Percentage auction time')+ylab('# of bids')+
    theme_mine
, w=3.4, h=3.0)
