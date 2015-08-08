#!/usr/bin/env Rscript
source('common.r')

# my_smooth <- function() stat_smooth(method=loess) #, span=0.3)
# cc_line <- function() list(
#   stat_summary(fun.y=mean, geom='line'),
#   cc_scales()
# )

d <- data.rawmix(where="name like '%v0.23%' and nclients = 2 and duration = 30 and length = 4")

d$facet <- with(d, "zipf:"+alpha+", keys:"+nkeys+"\n"+num(commute_ratio)*100+"% / len: "+length)

d <- subset(d, rate == 100); d$x <- d$nthreads
# d <- subset(d, nthreads == 32); d$x <- d$rate

d$grp <- x(d$cc,d$phase)

d$label <- d$nthreads + "/" + d$rate

save(
  ggplot(d, aes(
    x = x,
    y = throughput,
    linetype = phase,
    group = grp,
    fill = cc,
    color = cc,
  ))+
  geom_point()+
  stat_summary(geom='line', fun.y=mean)+
  expand_limits(y=0)+
  facet_wrap(~facet)+ #, scales="free")+
  cc_scales()+
  phase.linetype()+
  my_theme()+theme(legend.position='bottom')
, name='plot/raw_explore', w=8, h=7)

save(
  ggplot(d, aes(
    x = throughput,
    y = avg_latency_ms,
    linetype = phase,
    group = grp,
    fill = cc,
    color = cc,
  ))+
  xlab('Throughput (txns/sec)')+ylab('Mean latency (ms)')+
  # geom_point()+
  # geom_text(aes(label=label), size=1.7)+
  geom_point()+
  geom_mean_path(d, throughput, avg_latency_ms, .(grp,x,facet,phase,cc))+
  expand_limits(y=0)+
  coord_cartesian(ylim=c(0,25))+
  facet_wrap(~facet)+ #, scales="free")+
  cc_scales()+
  phase.linetype()+
  my_theme()+theme(legend.position='bottom')
, name='plot/raw_tput_v_lat', w=8, h=7)
