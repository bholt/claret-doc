#!/usr/bin/env Rscript
source('common.r')
a <- parse.args()

d <- data.rubis(where="
  name like 'v0.24%' and bidspread = 20000 and nthreads = 64
  and duration = 60
  and phasing = 1 and ccmode = 'simple'
  and gen = 'rubis'
")
# and loaddir like '%default%'


d1 <- d[1,]
l1 <- as.list(fromJSON(jsfix(d1$server_bid_time_hist)))
df1 <- data.frame(x=num(names(l1)), y=vals(l1))
df1 <- df1[order(df1$x),]

# df1v <- as.vector(rep(df1$x, df1$y))
# save(
#   ggplot(data.frame(x=df1v), aes(x=x))+
#     # geom_histogram(binwidth=0.02, fill=c.blue)+
#     stat_ecdf(color=c.blue)+
#     # geom_line()+
#     # geom_density(fill=NA, color=c.blue, adjust=1/2)+
#     scale_x_continuous(limits=c(0,1.0))+
#     xlab('Percentage auction time')+ylab('# of bids')+
#     theme_mine
# , w=3.4, h=3.0)

save(
  ggplot(df1, aes(x=x, weight=y))+
    geom_histogram(binwidth=0.02, fill=c.blue)+
    # scale_x_continuous(limits=c(0,1.0))+
    # xlim(0,1.0)+ylim(0,50000)+
    coord_cartesian(xlim=c(0,1))+
    xlab('Percentage auction time')+ylab('# of bids')+
    scale_y_continuous(labels=si.labels())+
    theme_mine
, w=3.4, h=2.0)
