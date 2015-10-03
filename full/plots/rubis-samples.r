#!/usr/bin/env Rscript
source('common.r')

d <- tryCatch(
  {
    d <- data.rubis(where="name = 'v0.28.2-sampa-sampled' and samples_txn_rate is not null and samples_txn_rate != '{}' and mix = 'bid-heavy' and phasing = 1")
    
    write.csv(subset(d, select = c('id', 'name', 'nclients', 'nthreads', 'cc', 'phasing', 'cc_ph', 'rate', 'mix', 'alpha', 'lambda', 'nusers', 'ncategories', 'nregions', 'throughput', 'state', 'avg_latency_ms', 'samples_txn_rate', 'samples_bid_rate')), file = 'data/rubis-samples.csv')
    d
  }, error = function(e) {
    error.database_unreachable()
    read.csv(file = 'data/rubis-samples.csv')
  }
)


print(nrow(d))

samples <- adply(d, 1, function(r){
  s <- subset(r, select=c('id','cc','phasing','cc_ph','mix'))
  h <- to.hist(jsfix(r$samples_txn_rate))
  h$x <- h$x / 1e6 # convert to seconds
  data.frame(s, h)
})

samples$x <- ave(samples$x, cut(samples$x, 60*3))

samples <- subset(samples, x > 14 & x < 46)

save(
  ggplot(samples, aes(
    x = x,
    y = y*4,
    color = cc_ph, fill = cc_ph
  ))+
    xlab('execution time (s)')+
    ylab('throughput (txn/s)')+
    stat_summary(geom='line', fun.y=mean, size=0.4)+
    scale_x_continuous()+
    scale_y_continuous(labels = function(x){ x/1000+'k' })+
    cc_ph_scales(name = 'Mode:', guide = guide_legend(nrow = 1))+
    coord_cartesian(xlim=c(15,45))+
    my_theme()+legend.bottom()+theme(legend.margin = unit(-25,'pt'), legend.background = element_rect(fill=alpha('white', 0)))
, 'rubis-samples', w=5, h=3)
