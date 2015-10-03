#!/usr/bin/env Rscript
source('common.r')
a <- parse.args()

d <- tryCatch({
  d <- data.rubis(where="name like '%28.1%' and nclients = 4 and nthreads = 64 and mix = 'bid-heavy' and ccmode = 'simple'")
  write.csv(subset(d, select = c('id', 'name', 'nclients', 'nthreads', 'cc', 'phasing', 'cc_ph', 'rate', 'alpha', 'mix', 'timeout_scaling', 'max_timeout_ms', 'phase_limit', 'max_retries', 'max_op_retries', 'prepare_retries', 'lambda', 'throughput', 'avg_latency_ms', 'txn_count', 'txn_failed', 'stat_nbids_hist', 'server_bid_time_hist')), file = 'data/bid-dist.csv')
  d
}, error = function(e) {
  error.database_unreachable(e)
  d <- read.table(file = 'data/bid-dist.csv', sep = ",", quote = "\"\"", header = T)
  d$stat_nbids_hist <- as.character(d$stat_nbids_hist)
  d
})

nbid.hist <- adply(d, 1, function(r) {
  s <- subset(r, select=c('id','cc','phasing','nthreads','nclients','mix'))
  h <- to.hist(r$stat_nbids_hist)
  # scale first 10 items to account for different bin widths
  h[h$x < 10, ]$y <- h[h$x < 10, ]$y*6.5
  data.frame(s, h)
})

save(
  # ggplot(subset(nbid.hist, x != 0), aes(x=x, weight=y))+
  ggplot(nbid.hist, aes(x = x, y = y))+
    geom_point(color = c.blue, size = 1.3)+
    xlab('bids / item (log scale)')+ylab('count (log scale)')+
    scale_x_log10(labels=si.labels(), breaks=c(1,10,100,1000,10000))+
    scale_y_log10(labels=si.labels(), breaks=c(1,10,100,1000,10000,100000))+
    my_theme()
, 'bid-dist', w=3.4, h=3.0)



bidtime.hist <- adply(d, 1, function(r) {
  s <- subset(r, select=c('id','cc','phasing','nthreads','nclients','mix'))
  h <- to.hist(jsfix(r$server_bid_time_hist))
  data.frame(s, h)
})

save(
  ggplot(bidtime.hist, aes(x = (x+0.02)*100, y = y))+
    xlab('% of auction window time')+
    ylab('# of bids')+
    stat_summary(geom = 'line', fun.y = mean, color = c.blue)+
    scale_x_continuous(breaks=c(0,20,40,60,80,100), limits=c(0,100))+
    scale_y_continuous(labels=si.labels())+
    theme_mine
, 'bid-time', w=3.4, h=2.0)
