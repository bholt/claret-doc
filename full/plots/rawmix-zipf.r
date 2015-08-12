#!/usr/bin/env Rscript
library('sitools')
source('common.r')
a <- parse.args()

d <- data.rawmix(where="name like 'v0.23%' and nclients = 2 and duration = 30 and length = 4")
d$facet <-  num(d$commute_ratio)*100 + "% update\nzipf: " + d$alpha

d$zipf <- num(d$alpha)

# d <- subset(d, nthreads == 32*2); d$x <- d$rate

d$label <- d$nthreads + "@" + d$rate

# PH <- 'phasing\n(R/W locks)'
BOTH <- 'boosting+phasing'

d$cc_ph <- factor(revalue(x(d$ccmode,d$combining,d$phasing), c(
  'rw#0#no'=RW,
  'simple#0#no'=COMM,
  'rw#0#yes'=PH,
  'simple#0#yes'=BOTH
)), levels=c(RW,PH,COMM,BOTH))

d <- d[!is.na(d$cc_ph),]

my_palette[[BOTH]] <- c.yellow

d.zipf <- subset(d, nkeys == 1000 & commute_ratio == 0.8)
d.zipf.mean <- ddply(d.zipf, .(facet,rate,nthreads,cc_ph,zipf,label,phasing,cc), summarize, throughput=mean(throughput))

d.mix <- subset(d, nkeys == 1000 & zipf == 0.8 & name == 'v0.23.1-sampa' & txn_failed < 1000)
d.mix.mean <- ddply(d.mix, .(facet,rate,nthreads,cc_ph,zipf,label,commute_ratio,phasing,cc), summarize, throughput=mean(throughput))

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
#   scale_x_continuous(labels=si.labels())+
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
  ggplot(d.zipf.mean, aes(
    x = zipf,
    y = throughput,
    group = x(cc,phasing),
    fill = cc, color = cc,
    linetype = phasing,
    label = label
  ))+
  xlab('Zipfian parameter')+ylab('Peak throughput (txn/s)')+
  stat_summary(geom='line', fun.y=max)+
  stat_summary(geom='point', fun.y=max)+
  # geom_text(size=1.2)+
  expand_limits(y=0)+
  scale_x_continuous(breaks=c(0.2,0.4,0.6,0.8,1.0,1.2))+
  scale_y_continuous(labels=si.labels())+
  # facet_wrap(~facet, ncol=6)+ #, scales="free")+
  cc_scales(title='Concurrency\ncontrol:')+
  # color_scales('', my_palette)+
  phasing.linetype(title='Phasing:')+
  my_theme()+legend.bottom()
, w=4, h=3)

save(
  ggplot(d.mix.mean, aes(
    x = commute_ratio,
    y = throughput,
    group = x(cc,phasing),
    fill = cc, color = cc,
    linetype = phasing,
    label = label
  ))+
  xlab('Operation mix (% update)')+ylab('Peak throughput (txn/s)')+
  stat_summary(geom='line', fun.y=max)+
  stat_summary(geom='point', fun.y=max)+
  # geom_text(size=1.2)+
  expand_limits(y=0)+
  # scale_x_continuous(breaks=c(0.2,0.4,0.6,0.8,1.0,1.2))+
  scale_y_continuous(labels=si.labels())+
  # color_scales('', my_palette)+
  cc_scales(title='Concurrency\ncontrol:')+
  phasing.linetype()+
  my_theme()+legend.bottom()
, 'rawmix-mix', w=4, h=3)
