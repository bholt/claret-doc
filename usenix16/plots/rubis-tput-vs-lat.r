#!/usr/bin/env Rscript
source('common.r')
a <- parse.args()

d <- tryCatch(
  {
    d <- data.rubis(where="duration = 60 and name like 'v0.29%' and nthreads <= 96 and total_time < 65")
    
    write.csv(subset(d, select = c('name', 'nclients', 'nthreads', 'cc', 'rate', 'mix', 'alpha', 'lambda', 'nusers', 'ncategories', 'nregions', 'phasing', 'cc_ph', 'timeout_scaling', 'throughput', 'state', 'avg_latency_ms')), file = 'data/rubis-tput-vs-lat.csv')
    d
  }, error = function(e) {
    error.database_unreachable(e)
    d <- db.csv(file = 'data/rubis-tput-vs-lat.csv')
  }
)

d$x <- d$nthreads * num(d$nclients)
d$label <- d$nthreads * num(d$nclients) + "x" + d$rate

# d$throughput <- d$rubis_txn_count / d$total_time

read.heavy <- 'read-heavy (~10% bid)'
bid.heavy <- 'bid-heavy (~50% bid)'
no.browse <- 'mostly bid'

d$workload <- factor(revalue(d$mix, c(
  'mixed'     = read.heavy,
  'bid-heavy' = bid.heavy,
  'no-browse' = no.browse
)), levels = c(read.heavy, bid.heavy, no.browse))

d$facet <- with(d, workload)

d <- subset(d, lambda == 20 & grepl('read-heavy|bid-heavy', workload) & nthreads != 80)

d <- subset(d, cc_ph %in% c(RW+BASE, RW+PH, COMM+PH, COMB+PH, NOTXN))


save(
  ggplot(d, aes(
    x = throughput,
    y = avg_latency_ms,
    group = cc_ph, fill = cc_ph, color = cc_ph, linetype = cc_ph
  ))+
  xlab('Throughput (txn/s)')+ylab('Mean latency (ms)')+
  scale_x_continuous(breaks = c(0, 5000, 10000), labels = function(x){ x/1000+'k' })+
  # geom_text(aes(label = nthreads))+
  geom_mean_path(d, throughput, avg_latency_ms, .(x,facet,cc,phasing,cc_ph))+
  expand_limits(x=0, y=0)+
  facet_wrap(~facet, scales="free_x")+
  cc_ph_scales()+
  my_theme()+coord_cartesian(y=c(0,10))
, 'rubis-tput-vs-lat', w=5, h=3)

save(
  ggplot(subset(d), aes(
    x = x,
    y = throughput,
    group = cc_ph, fill = cc_ph, color = cc_ph, linetype = cc_ph
  ))+
  xlab('Clients')+ylab('Throughput (txn/s)')+
  stat_summary(geom='line', fun.y=mean)+
  stat_summary(geom='point', fun.y=mean)+
  scale_x_continuous(trans=log2_trans(), breaks=c(8,16,32,64,128,256))+
  scale_y_continuous(breaks = c(0, 5000, 10000, 15000, 20000), labels = si.labels())+
  expand_limits(x=0, y=0)+
  facet_wrap(~facet, scales="free_x")+
  cc_ph_scales(guide = guide_legend(nrow = 5))+
  my_theme()
, 'rubis-tput', w=5, h=2.5)
