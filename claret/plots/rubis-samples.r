#!/usr/bin/env Rscript
source('common.r')

d <- tryCatch(
  {
    d <- data.rubis(where="name = 'v0.28.2-sampa-sampled' and samples_txn_rate is not null and samples_txn_rate != '{}' and mix = 'bid-heavy' and phasing = 1")
    
    write.csv(subset(d, select = c('id', 'name', 'nclients', 'nthreads', 'cc', 'phasing', 'cc_ph', 'rate', 'mix', 'alpha', 'lambda', 'nusers', 'ncategories', 'nregions', 'throughput', 'state', 'avg_latency_ms', 'samples_txn_rate', 'samples_bid_rate')), file = 'data/rubis-samples.csv')
    d
  }, error = function(e) {
    error.database_unreachable(e)
    db.csv(file = 'data/rubis-samples.csv')
  }
)


print(nrow(d))

d <- subset(d, cc_ph %in% c(RW+PH,COMM+PH))

samples <- adply(d, 1, function(r){
  s <- subset(r, select=c('id','cc','phasing','cc_ph','mix'))
  h <- to.hist(jsfix(r$samples_txn_rate))
  h$x <- h$x / 1e6 # convert to seconds
  data.frame(s, h)
})

samples$x <- ave(samples$x, cut(samples$x, 60*3))

samples <- subset(samples, x > 14 & x < 46 )

save(
  ggplot(samples, aes(
    x = x,
    y = y * 4, # multiply by number of clients for total throughput
    color = cc_ph, fill = cc_ph
  ))+
    xlab('execution time (s)')+
    ylab('throughput (txn/s)')+
    stat_summary(geom='line', fun.y=mean, size=0.4)+
    
    # annotations for variance
    annotate(geom='text', x = 15.5, y = 13700, 
             size = 3, color = c.blue, hjust = 0, family="Helvetica",
             label = 'stddev (over 5s): 287')+

    annotate(geom='text', x = 44.5, y = 10500,
             size = 3, color = c.gray, hjust = 1, family="Helvetica",
             label = 'stddev (over 5s): 800')+
    
    scale_x_continuous()+
    scale_y_continuous(labels = function(x){ x/1000+'k' })+
    cc_ph_scales(name = '', guide = guide_legend(nrow = 1))+
    coord_cartesian(xlim=c(15,45))+
    my_theme()+legend.bottom()+theme(legend.margin = unit(-25,'pt'), legend.background = element_rect(fill=alpha('white', 0)), legend.key.width = unit(16,'pt'))
, 'rubis-samples', w=5, h=3)
