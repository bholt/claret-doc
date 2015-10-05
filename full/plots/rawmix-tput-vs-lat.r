#!/usr/bin/env Rscript
source('common.r')
a <- parse.args()

d <- tryCatch(
  {
    d <- data.rawmix(where="name like 'v0.27.5%' and nclients = 4 and duration = 30 and length = 4")
        
    d <- subset(d, commute_ratio == 0.5 & alpha == 0.6 & rate == 100)
    
    write.csv(subset(d, select = c('name', 'nclients', 'nthreads', 'cc', 'phasing', 'cc_ph', 'rate', 'alpha', 'commute_ratio', 'timeout_scaling', 'throughput', 'op_timeouts', 'avg_latency_ms', 'txn_failed')), file = 'data/rawmix-tput-vs-lat.csv')
    
    d
  }, error = function(e) {
    error.database_unreachable(e)
    d <- db.csv(file = 'data/rawmix-tput-vs-lat.csv')
  }
)

d$label <- d$nthreads * num(d$nclients) + "x" + d$rate

d$x <- d$nthreads * num(d$nclients)

g.cc_ph <- guide_legend(nrow = 6)


# save(
#   ggplot(d, aes(
#     x = throughput,
#     y = avg_latency_ms,
#     group = cc_ph, fill = cc_ph, color = cc_ph, linetype = cc_ph
#   ))+
#   xlab('Throughput (txn/s)')+ylab('Mean latency (ms)')+
#   # geom_point()+
#   # geom_text(aes(label=label), size=1.7)+
#   scale_x_continuous(labels=si.labels())+
#   # geom_point()+
#   geom_mean_path(d, throughput, avg_latency_ms, .(x,cc,phasing,cc_ph))+
#   expand_limits(y=0)+
#   coord_cartesian(ylim=c(0,20))+
#   # cc_scales(title='Mode:', guide = g.cc)+
#   # color_scales('', my_palette)+
#   # phasing.linetype(title='Phasing:', guide = g.cc_ph)+
#   cc_ph_scales()+
#   my_theme() #+legend.bottom()
# , w=5, h=3)


save(
  ggplot(d, aes(
    x = x,
    y = throughput,
    group = cc_ph, fill = cc_ph, color = cc_ph, linetype = cc_ph,
  ))+
  xlab('Clients')+ylab('Throughput (txn/s)')+
  stat_summary(geom='line', fun.y=mean)+
  stat_summary(geom='point', fun.y=mean)+  
  scale_x_continuous(trans=log2_trans(), breaks=c(16,32,64,128,256,384))+
  scale_y_continuous(labels=si.labels())+
  expand_limits(y=0)+
  cc_ph_scales()+
  my_theme()
, 'rawmix-tput', w=5, h=3.5)

save(
  ggplot(d, aes(
    x = x,
    y = avg_latency_ms,
    group = cc_ph, fill = cc_ph, color = cc_ph, linetype = cc_ph,
  ))+
  xlab('Clients')+ylab('Mean transaction latency (ms)')+
  stat_summary(geom='line', fun.y=mean)+
  stat_summary(geom='point', fun.y=mean)+  
  # stat_summary(geom='smooth', fun.data=mean_cl_normal)+
  # stat_summary(fun.data=mean_cl_normal, geom='errorbar', width=0.2, aes(color='black'))+
  scale_x_continuous(breaks=c(16,32,64,128,256,384))+
  scale_y_continuous(labels=si.labels())+
  expand_limits(y=0)+
  cc_ph_scales()+
  my_theme()
, 'rawmix-latency', w=5, h=3.5)

cl_normal_min <- function(r) { mean_cl_normal(r)$ymin }
cl_normal_max <- function(r) { mean_cl_normal(r)$ymax }

d.mean <- ddply(d, .(rate,nthreads,cc_ph,phasing,cc,timeout_scaling), summarise, y=mean(throughput), ymin=cl_normal_min(throughput), ymax=cl_normal_max(throughput))

my.max <- function(r) { r[which.max(r$y),] }

d.max <- ddply(d.mean, .(cc_ph), my.max)

save(
  ggplot(d.max, aes(
    x = cc_ph,
    y = y,
    group = cc_ph, fill = cc_ph,
  ))+
  xlab('total clients')+ylab('throughput (txn/s)')+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=ymin,ymax=ymax), width=0.2)+
  # scale_x_continuous(trans=log2_trans(), breaks=c(16,32,64,128,256,384))+
  scale_y_continuous(labels=si.labels())+
  expand_limits(y=0)+
  cc_ph_scales()+
  my_theme()+theme(legend.position='none')
, w=4.5, h=3.5)


dr <- data.or.csv(
  csv = 'data/rawmix-retries.csv',
  gen = function() {
    d <- data.rawmix(where="name like 'v0.28%' and nclients = 4 and duration = 30 and length = 4 and rate = 50")
    dc <- adply(d, 1, function(r){
        c <- fromJSON(jsfix(r$server_txn_conflict_on_tag))
        c <- c[['0']][['s']]
        data.frame(conflicts = c$conflicts, conflicts_total = c$total)
      })
    subset(dc, select = c('name', 'nthreads', 'cc', 'phasing', 'cc_ph', 'avg_latency_ms', 'throughput', 'txn_retries', 'conflicts', 'conflicts_total'))
  }
)

dr$cc_ph <- factor(dr$cc_ph, levels = rev(levels(dr$cc_ph)))
save(
  ggplot(dr, aes(x = cc_ph, y = txn_retries, group = cc_ph, color = cc_ph, fill = cc_ph))+
    xlab('mode')+
    ylab('txn retries')+
    geom_bar(stat="identity")+
    scale_y_continuous(labels=k.labels)+
    expand_limits(y=0)+
    cc_ph_scales()+
    coord_flip()+
    my_theme()+theme(legend.position='none')
    
, 'rawmix-retries', w=4.5, h=2.5)
