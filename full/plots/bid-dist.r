#!/usr/bin/env Rscript
source('common.r')
a <- parse.args()

d <- data.rubis(where="nclients = 4 order by id desc limit 1")

nbid.hist <- to.hist(d$stat_nbids_hist)
# scale first 10 items (to account for different bin-widths)
nbid.hist[nbid.hist$x >= 10, ]$y <- nbid.hist[nbid.hist$x >= 10, ]$y/10

save(
  # ggplot(subset(nbid.hist, x != 0), aes(x=x, weight=y))+
  ggplot(nbid.hist, aes(x=x,y=y))+
    geom_point(color=c.blue, size=1.4)+
    xlab('bids / item (log scale)')+ylab('count (log scale)')+
    scale_x_log10(labels=si.labels(), breaks=c(1,10,100,1000,10000))+
    scale_y_log10(labels=si.labels(), breaks=c(1,10,100,1000,10000))+
    theme_mine
, w=3.4, h=3.0)
