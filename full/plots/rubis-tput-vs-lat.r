#!/usr/bin/env Rscript
source('common.r')
a <- parse.args()

d <- tryCatch(
  {
    d <- data.rubis(where="duration = 60 and name like 'v0.28.1%'")
    
    write.csv(subset(d, select = c('name', 'nclients', 'nthreads', 'cc', 'rate', 'mix', 'alpha', 'lambda', 'nusers', 'ncategories', 'nregions', 'phasing', 'cc_ph', 'timeout_scaling', 'throughput', 'state', 'avg_latency_ms')), file = 'data/rubis-tput-vs-lat.csv')
    d
  }, error = function(e) {
    write("\n!! Database unreachable. Reading stashed results from CSV.\n", stderr())
    d <- read.csv(file = 'data/rubis-tput-vs-lat.csv')
  }
)

d$x <- d$nthreads * num(d$nclients)
d$label <- d$nthreads * num(d$nclients) + "x" + d$rate

# d$throughput <- d$rubis_txn_count / d$total_time

d$facet <- with(d, state+" (z:"+alpha+") | "+mix )

d <- subset(d, lambda == 20)

save(
  ggplot(d, aes(
    x = throughput,
    y = avg_latency_ms,
    group = cc_ph, fill = cc_ph, color = cc_ph, linetype = cc_ph
  ))+
  xlab('Throughput (txn/s)')+ylab('Mean latency (ms)')+
  geom_point()+
  scale_x_continuous(labels=function(x){ x/1000+'k' })+
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
    group = x(cc,phasing),
    fill = cc_ph, color = cc_ph, linetype = cc_ph
  ))+
  # geom_point()+
  stat_summary(geom='line', fun.y=mean)+
  expand_limits(y=0)+
  scale_y_continuous(labels=function(x){ x/1000+'k' })+
  facet_wrap(~facet)+ #, scales="free")+
  cc_ph_scales()+
  my_theme()
, 'rubis-explore', w=8, h=6)
