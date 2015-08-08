#!/usr/bin/env Rscript
library('sitools')
source('common.r')
a <- parse.args()

d <- data.rubis(where="duration = 30 and name like 'v0.23%'")
d$x <- d$nthreads * num(d$nclients)
d$label <- d$nthreads * num(d$nclients) + "x" + d$rate
odir <- 'plot/rubis/threads'

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
    group = cc_ph,
    fill = cc_ph,
    color = cc_ph,
  ))+
  xlab('Throughput (txn/s)')+ylab('Mean latency (ms)')+
  # geom_point()+
  # geom_text(aes(label=label), size=1.7)+
  scale_x_continuous(labels=fmt.label())+
  geom_point()+
  geom_mean_path(d, throughput, avg_latency_ms, .(x,facet,cc_ph))+
  expand_limits(y=0)+
  facet_wrap(~facet)+ #, scales="free")+
  cc_scales()+
  my_theme()+theme(legend.position='bottom')
, w=4, h=4)
