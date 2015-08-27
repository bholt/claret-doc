#!/usr/bin/env Rscript
source('common.r')
a <- parse.args()

d <- data.rawmix(where="name like 'v0.27-%' and nclients = 4 and duration = 30 and length = 4")
d$x <- d$nthreads * num(d$nclients)
d$label <- d$nthreads * num(d$nclients) + "x" + d$rate

d <- subset(d, commute_ratio == 0.5 & alpha == 0.6 & rate == 100)

# d$cc_ph <- factor(revalue(x(d$ccmode,d$combining,d$phasing), c(
#   'rw#0#no'=RW,
#   'simple#0#no'=COMM,
#   'rw#0#yes'=PH,
#   'simple#1#yes'=ALL
# )), levels=c(RW,PH,COMM,ALL))

g.cc <- guide_legend(nrow = 3)
g.phasing <- guide_legend(title = "Phasing:", nrow = 2)

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
  coord_cartesian(ylim=c(0,10))+
  cc_scales(title='Mode:', guide = g.cc)+
  # color_scales('', my_palette)+
  phasing.linetype(title='Phasing:', guide = g.phasing)+
  my_theme() #+legend.bottom()
, w=4, h=3)


# save(
#   ggplot(d, aes(
#     x = x,
#     y = txns_behind_schedule,
#     group = x(cc,phasing),
#     fill = cc, color = cc,
#     linetype = phasing,
#   ))+
#   stat_summary(geom='line', fun.y=mean)+
#   expand_limits(y=0)+
#   cc_scales(title='Concurrency\ncontrol:')+
#   phasing.linetype(title='Phasing:')+
#   my_theme()+legend.bottom()
# , 'rawmix-explore', w=4, h=3.5)
