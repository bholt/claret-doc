#!/usr/bin/env Rscript
source('common.r')

where <- "name like '%v0.18%' and loaddir like '%up64%' and ntotal = 20000 and snb_queries = '1,2,3,7,8,9'"

# DATA.MODE <- 'local'
# d <- data.ldbc(where="name like '%v0.16-multinode%' and snb_threads = 24")
d <- data.ldbc(where=where, melt=F)

tags <- gsub('snb_(.*)_count', '\\1', names(d[,!names(d) %in% "snb_getperson_count" & grepl('snb_(.*)_count',names(d))]))
print(tags)

d$facet <- d$snb_queries

save(
  ggplot(subset(d, snb_time_ratio > 3e-6), aes(
    x = 1/snb_time_ratio,
    y = throughput,
    group = cc,
    fill = cc,
    color = cc,
    label = snb_time_ratio,
  ))+
  geom_point()+
  # stat_smooth()+
  stat_summary(geom='line', fun.y=mean)+
  # geom_text(size=1.7, position='dodge')+
  expand_limits(y=0)+
  facet_wrap(~facet, scales="free")+
  # scale_x_log10(breaks=trans_breaks("log10", function(x) 10^x),
  #               labels=trans_format("log10", math_format(10^.x)))+
  cc_scales()+
  my_theme()
, name='plot/explore_ldbc_cm', w=6, h=5)

d$latency_ms <- num(d$txn_time) / num(d$txn_count) * 1e6
save(
  ggplot(d, aes(
    x = throughput,
    y = latency_ms,
    group = cc,
    fill = cc,
    color = cc,
    label = snb_time_ratio,
  ))+
  geom_point()+
  geom_mean_path(d, throughput, latency_ms, .(cc,snb_time_ratio,facet))+
  expand_limits(y=0)+
  facet_wrap(~facet, scales="free")+
  cc_scales()+
  my_theme()
, name='plot/ldbc_lat_vs_tput_avg', w=6, h=5)

melt_metric <- function(d, metric) {
  dm <- melt(d, measure='snb_'+tags+'_'+metric)
  dm$txn <- unlist(lapply(dm$variable, function(s) gsub('snb_(.*)_'+metric,'\\1', s)))
  dm[,metric] <- dm$value
  dm
}

for(t in tags) d['snb_'+t+'_lat_ms'] <- num(d[,'snb_'+t+'_latency']) * 1e3 / num(d[,'snb_'+t+'_count'])
d <- do.call(data.frame,lapply(d, function(x) replace(x, is.infinite(x),NA)))
dm <- melt(subset(d, grepl('^2$|1,2,3,7,8,9', snb_queries)), measure=p('snb_',tags,'_lat_ms'))
dm$txn <- unlist(lapply(dm$variable, function(s) gsub('snb_(.*)_lat_ms','\\1', s)))
dm$facet <- dm$snb_queries + " - " + dm$txn
dm$latency_ms <- dm$value

save(
  ggplot(subset(dm, grepl('^2$|1,2,3,7,8,', snb_queries)), aes(
    x = throughput,
    y = latency_ms,
    group = cc,
    fill = cc,
    color = cc,
    # label = snb_time_ratio,
  ))+
  xlab('Throughput (txns/sec)')+ylab('Mean latency (ms)')+
  geom_point()+
  geom_mean_path(dm, throughput, latency_ms, .(cc,snb_time_ratio,facet))+
  expand_limits(y=0)+
  facet_wrap(~facet, scales="free", ncol=5)+
  cc_scales()+
  theme(legend.position='bottom')+
  my_theme()
, name='plot/ldbc_lat_vs_tput_cm', w=10, h=8)

dm.fail <- melt_metric(d, 'failed')
save(
  ggplot(dm.fail, aes(
    x = 1/snb_time_ratio,
    y = num(failed),
    group = cc,
    fill = cc,
    color = cc,
    # label = snb_time_ratio,
  ))+
  xlab('Throughput (txns/sec)')+ylab('Failed txns')+
  geom_point()+
  geom_mean_path(dm.fail, throughput, failed, .(cc,snb_time_ratio,facet))+
  expand_limits(y=0)+
  facet_wrap(~txn, scales="free", ncol=5)+
  cc_scales()+
  theme(legend.position='bottom')+
  my_theme()
, name='plot/ldbc_failed_cm', w=10, h=8)


##########################################################################

d <- data.ldbc(where=where)

d$facet <- d$name
d$latency_ms <- d$time_mean / 1000
save(
  ggplot(subset(d
    #, name == 'AddPost'
  ), aes(
    x = throughput,
    y = latency_ms,
    group = cc,
    fill = cc,
    color = cc,
  ))+
  xlab('Throughput (txns/sec)')+ylab('Mean latency (ms)')+
  geom_point()+
  # geom_text(size=1.7, position="jitter")+
  # stat_smooth()+
  geom_mean_path(d, throughput, latency_ms, .(cc,snb_time_ratio,facet))+
  # geom_path(data=mean_path(d, throughput, avg_latency_ms, .(cc,snb_time_ratio,facet)), aes(x=x, y=y))+
  expand_limits(y=0)+
  facet_wrap(~facet, scales="free")+
  # scale_x_log10(breaks=trans_breaks("log10", function(x) 10^x), labels=trans_format("log10", math_format(10^.x)))+
  cc_scales()+
  my_theme()
, name='plot/ldbc_lat_vs_tput', w=10, h=8)
#
#
# save(
#   ggplot(subset(d,
#     # ccmode == 'simple'
#     ntotal == 50000
#     # & is.na(machines)
#     #& grepl('zork',machines)
#     # & name == 'AddPost'
#   ), aes(
#     x = 1/snb_time_ratio,
#     y = time_mean / 1000,
#     group = cc,
#     fill = cc,
#     color = cc,
#     label = snb_time_ratio,
#   ))+
#   geom_point()+ylab('avg latency (ms)')+
#   geom_text(size=1.7, position='dodge')+
#   # stat_summary(aes(label=ntotal), fun.y=mean, geom="text")+
#   stat_summary(geom='line', fun.y=mean)+
#   # stat_smooth()+
#   expand_limits(y=0)+
#   facet_wrap(~name)+
#   scale_x_log10(breaks=trans_breaks("log10", function(x) 10^x),
#                 labels=trans_format("log10", math_format(10^.x)))+
#   cc_scales()+
#   my_theme()
# , name='plot/explore_latency', w=10, h=8)
#
# # save(
# #   ggplot(subset(d,
# #     # ccmode == 'simple'
# #     ntotal == 50000
# #     & machines == 'carrey'
# #     # & name == 'Query2'
# #   ), aes(
# #     x = 1/snb_time_ratio,
# #     y = server_cc_check_failed,
# #     group = cc,
# #     fill = cc,
# #     color = cc,
# #     label = ntotal,
# #   ))+
# #   geom_point()+
# #   # geom_text(size=1.7)+
# #   # stat_summary(aes(label=ntotal), fun.y=mean, geom="text")+
# #   stat_smooth()+
# #   # expand_limits(y=0)+
# #   facet_wrap(~facet)+
# #   scale_x_log10(breaks=trans_breaks("log10", function(x) 10^x),
# #                 labels=trans_format("log10", math_format(10^.x)))+
# #   cc_scales()+
# #   my_theme()
# #   # scale_fill_manual(values=my_palette, name='Variant')+
# #   # scale_color_manual(values=my_palette, name='Variant')
# # , name='plot/explore_retries', w=10, h=8)
#
# save(
#   ggplot(subset(d, ntotal==50000), aes(
#     x = name,
#     y = time_count,
#     fill = name,
#     color = name,
#     group = name,
#   ))+
#   geom_meanbar()+
#   theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0))+
#   expand_limits(y=0)+
#   my_theme()
# , name='plot/explore_counts', w=8, h=6)
