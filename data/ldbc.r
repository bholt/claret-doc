#!/usr/bin/env Rscript
source('common.r')

# DATA.MODE <- 'local'
# d <- data.ldbc(where="name like '%v0.16-multinode%' and snb_threads = 24")
d <- data.ldbc(where="name like '%v0.17%up64' and ntotal = 200000")

save(
  ggplot(d, aes(
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
  facet_wrap(~machines, scales="free")+
  # scale_x_log10(breaks=trans_breaks("log10", function(x) 10^x),
  #               labels=trans_format("log10", math_format(10^.x)))+
  cc_scales()+
  my_theme()
, name='plot/explore_ldbc', w=6, h=5)

d$facet <- with(d, sprintf("%s*%d\n%s", machines, snb_threads, name))
d$latency_ms <- d$time_mean / 1000

save(
  ggplot(subset(d
    #, name == 'AddPost'
  ), aes(
    x = per_tput,
    y = latency_ms,
    group = cc,
    fill = cc,
    color = cc,
    label = snb_time_ratio,
  ))+
  xlab('Throughput (txns/sec)')+ylab('Mean latency (ms)')+
  geom_point()+
  # geom_text(size=1.7, position="jitter")+
  # stat_smooth()+
  geom_mean_path(d, per_tput, latency_ms, .(cc,snb_time_ratio,facet))+
  # geom_path(data=mean_path(d, throughput, avg_latency_ms, .(cc,snb_time_ratio,facet)), aes(x=x, y=y))+
  expand_limits(y=0)+
  facet_wrap(~facet, scales="free")+
  # scale_x_log10(breaks=trans_breaks("log10", function(x) 10^x), labels=trans_format("log10", math_format(10^.x)))+
  cc_scales()+
  my_theme()
, name='plot/ldbc_lat_vs_tput', w=10, h=8)

save(
  ggplot(subset(d
    #, name == 'AddPost'
  ), aes(
    x = throughput,
    y = wavg_mean_latency,
    group = cc,
    fill = cc,
    color = cc,
    label = snb_time_ratio,
  ))+
  xlab('Throughput (txns/sec)')+ylab('Mean latency (ms)')+
  geom_point()+
  # geom_text(size=1.7, position="jitter")+
  # stat_smooth()+
  geom_mean_path(d, throughput, wavg_mean_latency, .(cc,snb_time_ratio))+
  # geom_path(data=mean_path(d, throughput, avg_latency_ms, .(cc,snb_time_ratio,facet)), aes(x=x, y=y))+
  expand_limits(y=0)+
  # facet_wrap(~facet, scales="free")+
  # scale_x_log10(breaks=trans_breaks("log10", function(x) 10^x), labels=trans_format("log10", math_format(10^.x)))+
  cc_scales()+
  my_theme()
, name='plot/ldbc_lat_vs_tput_wavg', w=10, h=8)


save(
  ggplot(subset(d,
    # ccmode == 'simple'
    ntotal == 50000
    # & is.na(machines)
    #& grepl('zork',machines)
    # & name == 'AddPost'
  ), aes(
    x = 1/snb_time_ratio,
    y = time_mean / 1000,
    group = cc,
    fill = cc,
    color = cc,
    label = snb_time_ratio,
  ))+
  geom_point()+ylab('avg latency (ms)')+
  geom_text(size=1.7, position='dodge')+
  # stat_summary(aes(label=ntotal), fun.y=mean, geom="text")+
  stat_summary(geom='line', fun.y=mean)+
  # stat_smooth()+
  expand_limits(y=0)+
  facet_wrap(~name)+
  scale_x_log10(breaks=trans_breaks("log10", function(x) 10^x),
                labels=trans_format("log10", math_format(10^.x)))+
  cc_scales()+
  my_theme()
, name='plot/explore_latency', w=10, h=8)

# save(
#   ggplot(subset(d,
#     # ccmode == 'simple'
#     ntotal == 50000
#     & machines == 'carrey'
#     # & name == 'Query2'
#   ), aes(
#     x = 1/snb_time_ratio,
#     y = server_cc_check_failed,
#     group = cc,
#     fill = cc,
#     color = cc,
#     label = ntotal,
#   ))+
#   geom_point()+
#   # geom_text(size=1.7)+
#   # stat_summary(aes(label=ntotal), fun.y=mean, geom="text")+
#   stat_smooth()+
#   # expand_limits(y=0)+
#   facet_wrap(~facet)+
#   scale_x_log10(breaks=trans_breaks("log10", function(x) 10^x),
#                 labels=trans_format("log10", math_format(10^.x)))+
#   cc_scales()+
#   my_theme()
#   # scale_fill_manual(values=my_palette, name='Variant')+
#   # scale_color_manual(values=my_palette, name='Variant')
# , name='plot/explore_retries', w=10, h=8)

save(
  ggplot(subset(d, ntotal==50000), aes(
    x = name,
    y = time_count,
    fill = name,
    color = name,
    group = name,
  ))+
  geom_meanbar()+
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0))+
  expand_limits(y=0)+
  my_theme()
, name='plot/explore_counts', w=8, h=6)
