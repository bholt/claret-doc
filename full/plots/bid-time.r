#!/usr/bin/env Rscript
source('common.r')

d <- data.rubis(where="nclients = 4 order by id desc limit 1")

bidtime.hist <- to.hist(jsfix(d$server_bid_time_hist))

save(
  ggplot(bidtime.hist, aes(x=x*100, weight=y))+
  xlab('% of auction window time')+
    geom_line(aes(x=(x+0.02)*100, y=y), color=c.blue)+
    ylab('# of bids')+
    scale_x_continuous(breaks=c(0,20,40,60,80,100), limits=c(0,100))+
    scale_y_continuous(labels=si.labels())+
    theme_mine
, w=3.4, h=2.0)
