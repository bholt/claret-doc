#!/usr/bin/env Rscript
source('common.r')

# my_smooth <- function() stat_smooth(method=loess) #, span=0.3)
# cc_line <- function() list(
#   stat_summary(fun.y=mean, geom='line'),
#   cc_scales()
# )

  d <- data.rawmix(where="name like '%v0.22.1%' and nclients = 2 and duration = 30 and length = 4")

d$facet <- with(d, "zipf:"+alpha+", keys:"+nkeys+"\n"+num(commute_ratio)*100+"% / len: "+length)

d$x <- d$nthreads * num(d$nclients)
d$label <- d$nthreads * num(d$nclients) + "x" + d$rate

save(
  ggplot(subset(d), aes(
    x = x,
    y = throughput,
    group = cc,
    fill = cc,
    color = cc,
  ))+
  geom_point()+
  stat_summary(geom='line', fun.y=mean)+
  expand_limits(y=0)+
  facet_wrap(~facet)+ #, scales="free")+
  cc_scales()+
  my_theme()+theme(legend.position='bottom')
, name='plot/raw_explore', w=8, h=7)

save(
  ggplot(subset(d), aes(
    x = throughput,
    y = avg_latency_ms,
    group = cc,
    fill = cc,
    color = cc,
  ))+
  xlab('Throughput (txns/sec)')+ylab('Mean latency (ms)')+
  # geom_point()+
  # geom_text(aes(label=label), size=1.7)+
  geom_point()+
  geom_mean_path(d, throughput, avg_latency_ms, .(cc,x,facet))+
  expand_limits(y=0)+
  facet_wrap(~facet)+ #, scales="free")+
  cc_scales()+
  my_theme()+theme(legend.position='bottom')
, name='plot/raw_tput_v_lat', w=8, h=7)
