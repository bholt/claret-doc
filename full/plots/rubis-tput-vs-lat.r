#!/usr/bin/env Rscript
source('common.r')
a <- parse.args()

d <- data.rubis(where="duration = 60 and name like 'v0.27.2%'")
d$x <- d$nthreads * num(d$nclients)
d$label <- d$nthreads * num(d$nclients) + "x" + d$rate

d$facet <- with(d, state+" (z:"+alpha+") | "+mix )

# save(
#   ggplot(subset(d), aes(
#     x = x,
#     y = throughput,
#     group = cc,
#     fill = cc,
#     color = cc,
#   ))+
#   geom_point()+
#   stat_summary(geom='line', fun.y=mean)+
#   expand_limits(y=0)+
#   facet_wrap(~facet, scales="free")+
#   cc_scales()+
#   my_theme()+theme(legend.position='bottom')
# , w=5, h=4)

save(
  ggplot(d, aes(
    x = throughput,
    y = avg_latency_ms,
    group = x(cc,phasing),
    fill = cc,
    color = cc,
    linetype = phasing
  ))+
  xlab('Throughput (txn/s)')+ylab('Mean latency (ms)')+
  # geom_point()+
  # geom_text(aes(label=label), size=1.7)+
  scale_x_continuous(labels=si.labels())+
  geom_point()+
  geom_mean_path(d, throughput, avg_latency_ms, .(x,facet,cc,phasing))+
  expand_limits(y=0)+
  # facet_wrap(~facet)+ #, scales="free")+
  cc_scales()+
  phasing.linetype()+
  my_theme()+theme(legend.position='bottom')
, w=4, h=4)
