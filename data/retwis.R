#!/usr/bin/env Rscript
source('common.R')

d <- data.retwis(where="nshards = 4 and rate != 0 and loaddir like '%12%'")

sub <- function(d=d) subset(d, mix == 'geom_repost')

# save(
#   ggplot(melt(
#     sub(d),
#     measure=c('throughput','avg_latency_ms')
#   ), aes(
#     x = rate,
#     y = value,
#     group = cc,
#     fill = cc,
#     color = cc,
#     label = rate,
#   ))+
#   geom_point()+
#   # geom_text(size=1.7)+
#   # stat_summary(aes(label=ntotal), fun.y=mean, geom="text")+
#   # stat_smooth()+
#   expand_limits(y=0)+
#   facet_wrap(~variable, scales="free_y")+
#   # scale_x_log10(breaks=trans_breaks("log10", function(x) 10^x),
#   #               labels=trans_format("log10", math_format(10^.x)))+
#   cc_scales()+
#   my_theme()
# , name='plot/retwis', w=10, h=8)

d.mean <- ddply(d, .(workload,cc,rate), summarize, x=mean(throughput), y=mean(avg_latency_ms))

save(
  ggplot(d, aes(
    x = throughput,
    y = avg_latency_ms,
    group = cc,
    fill = cc,
    color = cc,
  ))+
  xlab('Throughput')+ylab('Average latency (ms)')+
  geom_point()+
  geom_path(data=d.mean, aes(x=x,y=y))+
  # geom_path(stat=summary, stat_params=list(fun.x=mean, fun.y=mean), aes(x=x,y=y))+
  expand_limits(y=0)+
  cc_scales()+
  facet_wrap(~workload)+
  my_theme()
, name='plot/retwis_tput_v_lat', w=8, h=5)


d$retwis_newuser_latency  <- d$retwis_newuser_time  / d$retwis_newuser_count
d$retwis_post_latency     <- d$retwis_post_time     / d$retwis_post_count
d$retwis_repost_latency   <- d$retwis_repost_time   / d$retwis_repost_count
d$retwis_timeline_latency <- d$retwis_timeline_time / d$retwis_timeline_count
d$retwis_follow_latency   <- d$retwis_follow_time   / d$retwis_follow_count

d.m <- melt(sub(d),
  measure=c(
    'retwis_newuser_latency',
    'retwis_post_latency',
    'retwis_repost_latency',
    'retwis_timeline_latency',
    'retwis_follow_latency'
  )
)
d.m$txn_type <- capply(d.m$variable, function(s) gsub('retwis_(\\w+)_latency','\\1', s))
d.m$latency_ms <- d.m$value * 1000

save(
  ggplot(sub(d.m), aes(
    x = throughput,
    y = latency_ms,
    group = cc,
    fill = cc,
    color = cc,
    label = rate,
  ))+
  # geom_point(size=1)+
  geom_text(size=1.2)+ #, position='dodge')+
  # stat_summary(aes(label=ntotal), fun.y=mean, geom="text")+
  # stat_smooth()+
  expand_limits(y=0)+
  facet_wrap(~txn_type, scales="free_y")+
  # scale_x_log10(breaks=trans_breaks("log10", function(x) 10^x),
  #               labels=trans_format("log10", math_format(10^.x)))+
  cc_scales()+
  my_theme()
, name='plot/retwis_breakdown', w=10, h=8)
