#!/usr/bin/env Rscript
source('common.r')
a <- parse.args()

d <- data.retwis(where="name like '%v0.26.1%' and nclients = 2")
d <- subset(d, avg_latency_ms < 50)
d$x <- d$nthreads * num(d$nclients)
d$label <- d$nthreads * num(d$nclients) + "x" + d$rate
d$facet <- with(d, "scale " + scale + ", " + mix)

save(
  ggplot(d, aes(
    x = throughput,
    y = avg_latency_ms,
    group = x(cc,phasing),
    fill = cc,
    color = cc,
    linetype = phasing,
    shape = phasing
  ))+
  xlab('Throughput (txn/s)')+ylab('Mean latency (ms)')+
  # geom_point()+
  # geom_text(aes(label=label), size=1.7)+
  # scale_x_continuous(labels=si.labels())+
  scale_x_continuous(labels=function(x){ x/1000+'k' })+
  geom_point()+
  geom_mean_path(d, throughput, avg_latency_ms, .(x,facet,cc,phasing,cc_ph))+
  expand_limits(y=0)+
  facet_wrap(~facet)+ #, scales="free")+
  cc_scales()+
  phasing.linetype()+
  my_theme()+theme(legend.position='bottom')
, w=6, h=4.5)

save(
  ggplot(subset(d), aes(
    x = x,
    y = throughput,
    group = x(cc,phasing),
    fill = cc, color = cc,
    linetype = phasing, shape = phasing
  ))+
  # geom_point()+
  stat_summary(geom='line', fun.y=mean)+
  expand_limits(y=0)+
  scale_y_continuous(labels=function(x){ x/1000+'k' })+
  facet_wrap(~facet, scales="free")+
  cc_scales()+phasing.linetype()+
  my_theme()+theme(legend.position='bottom')
, 'retwis-explore', w=5, h=4)

