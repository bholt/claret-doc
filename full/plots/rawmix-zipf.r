#!/usr/bin/env Rscript
library('sitools')
source('common.r')
a <- parse.args()

d <- data.rawmix(where="name like '%v0.23%' and nclients = 2 and duration = 30 and length = 4")
d$throughput <- d$throughput
d$facet <-  num(d$commute_ratio)*100 + "% update\nzipf: " + d$alpha

d <- subset(d, rate == 100 & nkeys == 1000 & commute_ratio == 0.8); d$x <- d$nthreads
# d <- subset(d, nthreads == 32*2); d$x <- d$rate

d$label <- d$nthreads + "@" + d$rate

# PH <- 'phasing\n(R/W locks)'
BOTH <- 'boosting+phasing'

d$cc_ph <- factor(revalue(x(d$ccmode,d$combining,d$phasing), c(
  'rw#0#0'=RW,
  'simple#0#0'=COMM,
  'rw#0#1'=PH,
  'simple#0#1'=BOTH
)), levels=c(RW,PH,COMM,BOTH))

my_palette[[BOTH]] <- c.yellow

d$zipf <- num(d$alpha)

d.mean <- ddply(d[!is.na(d$cc_ph),], .(facet,rate,nthreads,cc_ph,zipf,label), summarize, throughput=mean(throughput))

# save(
#   ggplot(d, aes(
#     x = throughput,
#     y = avg_latency_ms,
#     group = cc_ph,
#     fill = cc_ph,
#     color = cc_ph,
#   ))+
#   xlab('Throughput (txn/s)')+ylab('Mean latency (ms)')+
#   # geom_point()+
#   # geom_text(aes(label=label), size=1.7)+
#   scale_x_continuous(labels=fmt.label())+
#   geom_point()+
#   geom_mean_path(d, throughput, avg_latency_ms, .(x,facet,cc_ph))+
#   expand_limits(y=0)+
#   facet_wrap(~facet)+ #, scales="free")+
#   cc_scales()+
#   my_theme()+theme(legend.position='bottom')
# , w=5, h=5)

# save(
#   ggplot(d.mean, aes(
#     x = cc_ph,
#     y = throughput,
#     group = cc_ph,
#     fill = cc_ph,
#     color = cc_ph,
#   ))+
#   ylab('Peak throughput (txn/s)')+
#   stat_summary(geom='bar', fun.y=max)+
#   expand_limits(y=0)+
#   facet_wrap(~facet, ncol=6)+ #, scales="free")+
#   cc_scales()+
#   my_theme()+theme(legend.position='bottom')+
#   theme(axis.text.x=element_blank(), axis.ticks=element_blank(), axis.title.x=element_blank())
# , w=5, h=5)

save(
  ggplot(d.mean, aes(
    x = zipf,
    y = throughput,
    group = cc_ph,
    fill = cc_ph,
    color = cc_ph,
    label = label
  ))+
  xlab('Zipfian parameter')+ylab('Peak throughput (txn/s)')+
  stat_summary(geom='line', fun.y=max)+
  stat_summary(geom='point', fun.y=max)+
  # geom_text(size=1.2)+
  expand_limits(y=0)+
  scale_x_continuous(breaks=c(0.2,0.4,0.6,0.8,1.0,1.2))+
  scale_y_continuous(labels=fmt.label())+
  # facet_wrap(~facet, ncol=6)+ #, scales="free")+
  # cc_scales()+
  color_scales('', my_palette)+
  my_theme()+theme(legend.position='bottom')
, w=4, h=3)
