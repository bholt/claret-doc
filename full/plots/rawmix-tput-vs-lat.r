#!/usr/bin/env Rscript
source('common.r')
a <- parse.args()

d <- data.rawmix(where="name like 'v0.23-%' and nclients = 2 and duration = 30 and length = 4")
d$x <- d$nthreads * num(d$nclients)
d$label <- d$nthreads * num(d$nclients) + "x" + d$rate

d <- subset(d, commute_ratio == 0.5 & alpha == 0.8 & rate == 100 & combining == 0)

# d$cc_ph <- factor(revalue(x(d$ccmode,d$combining,d$phasing), c(
#   'rw#0#no'=RW,
#   'simple#0#no'=COMM,
#   'rw#0#yes'=PH,
#   'simple#1#yes'=ALL
# )), levels=c(RW,PH,COMM,ALL))

save(
  ggplot(d, aes(
    x = throughput,
    y = avg_latency_ms,
    group = x(cc,phasing),
    fill = cc, color = cc,
    linetype = phasing,
  ))+
  xlab('Throughput (txn/s)')+ylab('Mean latency (ms)')+
  # geom_point()+
  # geom_text(aes(label=label), size=1.7)+
  scale_x_continuous(labels=si.labels())+
  # geom_point()+
  geom_mean_path(d, throughput, avg_latency_ms, .(x,cc,phasing))+
  expand_limits(y=0)+
  cc_scales(title='Concurrency\ncontrol:')+
  # color_scales('', my_palette)+
  phasing.linetype(title='Phasing:')+
  my_theme()+legend.bottom()
, w=4, h=4)
