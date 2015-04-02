#!/usr/bin/env Rscript
source('common.R')

d <- data.retwis(where="nshards = 4 and rate != 0 and loaddir like '%12%' and rate <= 1000")

sub <- function(d=d) subset(d, mix == 'geom_repost' & is.na(machines))

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

d$throughput <- d$retwis_txn_count * num(d$nclients) / d$total_time;

d$client_machines <- factor(d$machines)

plot.path <- function(d) {
  
  d$facet <- sprintf('%s\n%s', d$machines, d$workload)
  
  d.mean <- ddply(d, .(facet,cc,rate), summarize, x=mean(throughput), y=mean(avg_latency_ms))

  return(
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
    expand_limits(y=0)+
    cc_scales()+
    facet_wrap(~facet, scales="free_x", ncol=3)+
    my_theme()
  )
}

# save(plot.path(subset(d, !grepl('zork',machines))), name='plot/retwis_tput_v_lat', w=8, h=5)
save(plot.path(
  subset(d, is.na(machines) | grepl('^candy$|,',machines) )
), name='plot/retwis_tput_v_lat_zork', w=8, h=10)


plot.path.breakdown <- function(d) {
  d$retwis_newuser_latency  <- d$retwis_newuser_time  / d$retwis_newuser_count
  d$retwis_post_latency     <- d$retwis_post_time     / d$retwis_post_count
  d$retwis_repost_latency   <- d$retwis_repost_time   / d$retwis_repost_count
  d$retwis_timeline_latency <- d$retwis_timeline_time / d$retwis_timeline_count
  d$retwis_follow_latency   <- d$retwis_follow_time   / d$retwis_follow_count
    
  d.m <- melt(d,
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
  d.m$facet <- with(d.m, sprintf('%s\n%s', mix, txn_type))

  d.m.mean <- ddply(d.m, .(facet,cc,rate), summarize, x=mean(throughput), y=mean(latency_ms))

  return(
    ggplot(d.m, aes(
      x = throughput,
      y = latency_ms,
      group = cc,
      fill = cc,
      color = cc,
      label = rate,
    ))+
    geom_text(size=1.2)+
    geom_path(data=d.m.mean, aes(x=x,y=y))+
    expand_limits(y=0)+
    facet_wrap(~facet, scales="free")+
    cc_scales()+
    my_theme()
  )
}

save(plot.path.breakdown(subset(sub(d), !grepl('zork',machines))), name='plot/retwis_breakdown', w=10, h=8)
save(plot.path.breakdown(subset(sub(d), grepl('zork',machines))), name='plot/retwis_breakdown_zork', w=10, h=8)
