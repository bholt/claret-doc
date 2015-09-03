#!/usr/bin/env Rscript
source('common.r')
a <- parse.args()

d <- tryCatch(
  {
    d <- data.rawmix(where="name like 'v0.27-%' and nclients = 4 and duration = 30 and length = 4")
        
    d <- subset(d, commute_ratio == 0.5 & alpha == 0.6 & rate == 100)
    
    write.csv(subset(d, select = c('name', 'nclients', 'nthreads', 'cc', 'phasing', 'cc_ph', 'rate', 'alpha', 'commute_ratio', 'timeout_scaling', 'throughput', 'op_timeouts', 'avg_latency_ms')), file = 'rawmix-tput-vs-lat.csv')
    
    d
  }, error = function(e) {
    write("!! Database unreachable. Reading stashed results from CSV.\n")
    d <- read.csv(file = 'rawmix-tput-vs-lat.csv')
  }
)

print(d$cc_ph)

d$label <- d$nthreads * num(d$nclients) + "x" + d$rate

d$x <- d$nthreads * num(d$nclients)

g.cc_ph <- guide_legend(nrow = 6)
# g.phasing <- guide_legend(title = "Phasing:", nrow = 2)


save(
  ggplot(d, aes(
    x = throughput,
    y = avg_latency_ms,
    group = cc_ph, fill = cc_ph, color = cc_ph, linetype = cc_ph
  ))+
  xlab('Throughput (txn/s)')+ylab('Mean latency (ms)')+
  # geom_point()+
  # geom_text(aes(label=label), size=1.7)+
  scale_x_continuous(labels=si.labels())+
  # geom_point()+
  geom_mean_path(d, throughput, avg_latency_ms, .(x,cc,phasing,cc_ph))+
  expand_limits(y=0)+
  coord_cartesian(ylim=c(0,20))+
  # cc_scales(title='Mode:', guide = g.cc)+
  # color_scales('', my_palette)+
  # phasing.linetype(title='Phasing:', guide = g.cc_ph)+
  cc_ph_scales()+
  my_theme() #+legend.bottom()
, w=5, h=3)


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
