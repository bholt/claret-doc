#!/usr/bin/env Rscript
source('common.r')
a <- parse.args()

# d <- data.or.csv('data/rawmix-tput-vs-lat.csv', gen = function() {
    d <- data.rawmix(where="name like 'v0.28.1%' and nclients = 4 and duration = 30 and length = 4 and rate = 100 and total_time < 61")
    
    d <- subset(d, commute_ratio == 0.5 & alpha == 0.6)
    
    # d
  # }
# )

d$label <- d$nthreads * num(d$nclients) + "x" + d$rate

d$x <- d$nthreads * num(d$nclients)

g.cc_ph <- guide_legend(nrow = 6)

# force all non-transactional ones to be considered 'rw' mode (messes up some groupings otherwise)
d[d$cc_ph == NOTXN,]$cc <- RW

# subset to just my selected set of lines
d <- subset(d, cc_ph %in% c(RW+BASE, COMM, COMM+PH, COMB+PH, NOTXN))

# d.mean <- mean_path(d, throughput, avg_latency_ms, .(nthreads,cc,phasing,cc_ph))

save(
  ggplot(d, aes(
    x = throughput,
    y = avg_latency_ms,
    group = cc_ph, fill = cc_ph, color = cc_ph, linetype = cc_ph
  ))+
  xlab('Throughput (txn/s)')+ylab('Mean latency (ms)')+
  # geom_point()+
  # geom_path()+
  geom_mean_path(d, throughput, avg_latency_ms, .(x,cc_ph,rate))+
  expand_limits(y=0)+
  coord_cartesian(ylim=c(0,4))+
  # scale_y_continuous(breaks=c(2,4,6,8,10))+
  scale_x_continuous(labels=function(x){ x/1000 + 'k' })+
  cc_ph_scales(guide = guide_legend(nrow=5))+
  theme.mine()+
  theme(
    panel.grid.major.x = element_line(color="grey80", size=0.2),
    panel.grid.minor.x = element_line(color="grey90", size=0.2),
    panel.grid.minor.y = element_line(color="grey90", size=0.2),
    legend.key = element_rect(fill=NA, color=NA),
    legend.text = element_text(lineheight=0.9),
    legend.key.height = unit(32,'pt'),
    legend.key.width = unit(20,'pt'),
    legend.title.align = 0.5,
    legend.margin = unit(0,'pt'),
    legend.title = element_blank()
    
  )
, w=5, h=2.5)


save(
  ggplot(subset(d), aes(
    x = x,
    y = throughput,
    group = cc_ph, fill = cc_ph, color = cc_ph, linetype = cc_ph,
  ))+
  xlab('Clients')+ylab('Throughput (txn/s)')+
  stat_summary(geom='line', fun.y=mean)+
  stat_summary(geom='point', fun.y=mean)+  
  scale_x_continuous(trans=log2_trans(), breaks=c(16,32,64,128,256,384))+
  expand_limits(y=0)+
  cc_ph_scales()+
  my_theme()
, 'rawmix-tput', w=5, h=3.5)

# save(
#   ggplot(d, aes(
#     x = x,
#     y = avg_latency_ms,
#     group = cc_ph, fill = cc_ph, color = cc_ph, linetype = cc_ph,
#   ))+
#   xlab('Clients')+ylab('Mean transaction latency (ms)')+
#   stat_summary(geom='line', fun.y=mean)+
#   stat_summary(geom='point', fun.y=mean)+
#   # stat_summary(geom='smooth', fun.data=mean_cl_normal)+
#   # stat_summary(fun.data=mean_cl_normal, geom='errorbar', width=0.2, aes(color='black'))+
#   scale_x_continuous(breaks=c(16,32,64,128,256,384))+
#   scale_y_continuous(labels=k.labels)+
#   expand_limits(y=0)+
#   cc_ph_scales()+
#   my_theme()
# , 'rawmix-latency', w=5, h=3.5)

# cl_normal_min <- function(r) { mean_cl_normal(r)$ymin }
# cl_normal_max <- function(r) { mean_cl_normal(r)$ymax }
#
# d.mean <- ddply(d, .(rate,nthreads,cc_ph,phasing,cc,timeout_scaling), summarise, y=mean(throughput), ymin=cl_normal_min(throughput), ymax=cl_normal_max(throughput))
#
# save(
#   ggplot(d.max, aes(
#     x = cc_ph,
#     y = y,
#     group = cc_ph, fill = cc_ph,
#   ))+
#   xlab('total clients')+ylab('throughput (txn/s)')+
#   geom_bar(stat="identity")+
#   geom_errorbar(aes(ymin=ymin,ymax=ymax), width=0.2)+
#   # scale_x_continuous(trans=log2_trans(), breaks=c(16,32,64,128,256,384))+
#   scale_y_continuous(labels=k.labels)+
#   expand_limits(y=0)+
#   cc_ph_scales()+
#   my_theme()+theme(legend.position='none')
# , w=4.5, h=3.5)


# dr <- data.or.csv(
#   csv = 'data/rawmix-retries.csv',
#   gen = function() {
#     d <- data.rawmix(where="name like 'v0.28%' and nclients = 4 and duration = 30 and length = 4 and rate = 50")
#     dc <- adply(d, 1, function(r){
#         c <- fromJSON(jsfix(r$server_txn_conflict_on_tag))
#         c <- c[['0']][['s']]
#         data.frame(conflicts = c$conflicts, conflicts_total = c$total)
#       })
#     subset(dc, select = c('name', 'nthreads', 'cc', 'phasing', 'disable_txns', 'cc_ph', 'txn_retries', 'txn_count', 'total_time', 'throughput', 'avg_latency_ms', 'conflicts', 'conflicts_total', 'server_acquire_attempts', 'server_acquire_first_success'))
#   }
# )
#
# dr$cc_ph <- factor(dr$cc_ph, levels = rev(levels(dr$cc_ph)))
# # dr$retry_rate <- with(dr, txn_retries / txn_count * 100)
# dr$retry_rate <- with(dr, txn_retries / total_time)
#
# dr$acquire_rate <- with(dr,
#   (server_acquire_first_success)/server_acquire_attempts * 100
# )
#
# save(
#   ggplot(subset(dr, disable_txns == 0), aes(x = cc_ph, y = acquire_rate, group = cc_ph, color = cc_ph, fill = cc_ph))+
#     xlab('mode')+
#     ylab('lock acquire success (percentage)')+
#     stat_summary(geom='bar', fun.y=mean)+
#     scale_y_continuous(labels=function(x) x+"%")+
#     expand_limits(y=0)+
#     cc_ph_scales()+
#     coord_flip()+
#     my_theme()+theme(legend.position='none')
# , 'rawmix-retries', w=4.5, h=2.5)
