#!/usr/bin/env Rscript
# DATA.MODE <- 'local'
source('common.r')
d <- data.retwis(where="nshards = 4 and nclients = 4 and rate != 0 and rate <= 1000 and nthreads = 32 and duration = 60 and name like '%v0.17%'")

tags <- c('newuser','post','repost','timeline','follow')

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

# d$throughput <- d$retwis_txn_count * num(d$nclients) / d$total_time;

d$client_machines <- factor(d$machines)

plot.path <- function(d) {
  
  d$facet <- sprintf('%s*%d\nscale %s - %s', d$machines, d$nthreads*num(d$nclients), d$scale, d$workload)
  
  d.mean <- ddply(d, .(facet,variant,rate), summarize, x=mean(throughput), y=mean(avg_latency_ms))

  return(
    ggplot(d, aes(
      x = throughput,
      y = avg_latency_ms,
      group = variant,
      fill = variant,
      color = variant,
    ))+
    xlab('Throughput')+ylab('Average latency (ms)')+
    geom_point()+
    geom_path(data=d.mean, aes(x=x,y=y))+
    expand_limits(y=0)+
    cc_scales()+
    theme(legend.position="top")+
    facet_wrap(~facet, ncol=2)+ # scales="free"
    my_theme()
  )
}

# save(plot.path(subset(d, !grepl('zork',machines))), name='plot/retwis_tput_v_lat', w=8, h=5)
save(plot.path(
  subset(d, (is.na(machines) | grepl('^candy$|.sampa$',machines)) & grepl('-heavy',workload))
), name='plot/retwis_tput_v_lat', w=8, h=10)


d$facet <- sprintf('%s*%d\nscale %s - %s', d$machines, d$nthreads*num(d$nclients), d$scale, d$workload)
save(
  ggplot(d, aes(
    x = rate * num(nthreads) * num(nclients) / 1000,
    y = throughput / 1000,
    group = variant,
    fill = variant,
    color = variant,
  ))+
  xlab('Offered load (ktxns/sec)')+ylab('Throughput (ktxns/sec)')+
  stat_summary(geom='line', fun.y=mean)+
  expand_limits(y=0)+
  cc_scales()+
  theme(legend.position="top")+
  facet_wrap(~facet, ncol=2)+ #, scales="free")+
  my_theme()
, name='plot/retwis_tput', w=8, h=10)

plot.path.final <- function(d) {
  d$facet <- d$workload
  d.mean <- ddply(d, .(facet,variant,rate), summarize, x=mean(throughput/1000), y=mean(avg_latency_ms))
  return(
    ggplot(d, aes(
      x = throughput / 1000,
      y = avg_latency_ms,
      group = variant,
      fill = variant,
      color = variant,
    ))+
    xlab('Throughput (ktxns/s)')+ylab('Average latency (ms)')+
    # geom_point(size=1.0)+
    geom_path(data=d.mean, aes(x=x,y=y))+
    expand_limits(y=0)+
    # cc_scales()+
    scale_fill_manual(values=my_palette, name='')+
    scale_color_manual(values=my_palette, name='')+
    # scale_linetype_manual(name='Mode', values=c('commutative'=1,'reader/writer'=2))+
    facet_wrap(~facet)+
    # theme(legend.direction='horizontal', legend.position='bottom', legend.title.align=1)+
    my_theme()
  )
}

d.f <- subset(d, grepl('sampa',machines) & grepl('heavy',workload) & threads == 32 & scale == 12 & rate <= 100 & !(ccmode == 'rw' & approx == 1))

bottom_legend <- theme(legend.direction='horizontal', legend.position='bottom', legend.title.align=1)

# save(plot.path(subset(d, !grepl('zork',machines))), name='plot/retwis_tput_v_lat', w=8, h=5)
# save(plot.path.final(subset(d.f, approx == 0)+bottom_legend), name='plot/retwis_tput_v_lat_final', w=4, h=4)
# save(plot.path.final(d.f)+bottom_legend, name='plot/retwis_tput_v_lat_final_approx', w=4, h=4)

limits <- coord_cartesian(xlim=c(0,22))

save(plot.path.final(subset(d.f, approx == 0 & ccmode == 'rw'))+limits, name='plot/retwis_tput_v_lat_final_rw', w=5.5, h=4)
save(plot.path.final(subset(d.f, approx == 0))+limits, name='plot/retwis_tput_v_lat_final', w=5.5, h=4)
save(plot.path.final(d.f)+limits, name='plot/retwis_tput_v_lat_final_approx', w=5.5, h=4)


# plot.path.approx <- function(d) {
#   d$facet <- d$workload
#   d.mean <- ddply(d, .(facet,variant,rate), summarize, x=mean(throughput), y=mean(avg_latency_ms))
#   return(
#     ggplot(d, aes(
#       x = throughput,
#       y = avg_latency_ms,
#       group = variant,
#       fill = variant,
#       color = variant,
#     ))+
#     xlab('Throughput')+ylab('Average latency (ms)')+
#     geom_point()+
#     geom_path(data=d.mean, aes(x=x,y=y))+
#     expand_limits(y=0)+
#     # cc_scales()+
#     scale_fill_manual(values=my_palette, name='Variant')+
#     scale_color_manual(values=my_palette, name='Variant')+
#     # scale_linetype_manual(name='Mode', values=c('commutative'=1,'reader/writer'=2))+
#     facet_wrap(~facet)+
#     my_theme()
#   )
# }
#
# save(plot.path.approx(
#   subset(d, grepl('^candy$',machines) & grepl('heavy',workload) & threads == 8)
# ), name='plot/retwis_tput_v_lat_approx', w=6, h=4)

plot.path.breakdown <- function(d) {
  d[p('retwis_',tags,'_latency')] <- d[p('retwis_',tags,'_time')] / d[p('retwis_',tags,'_count')]
  
  d.m <- melt(d, measure=p('retwis_',tags,'_latency'))
  d.m$txn_type <- capply(d.m$variable, function(s) gsub('retwis_(\\w+)_latency','\\1', s))
  d.m$latency_ms <- d.m$value * 1000
  d.m$facet <- with(d.m, sprintf('%s\n%s', workload, txn_type))

  # d.m.mean <- ddply(d.m, .(facet,variant,rate), summarize, x=mean(throughput), y=mean(latency_ms))
  
  return(
    ggplot(d.m, aes(
      x = rate,
      y = latency_ms,
      group = variant,
      fill = variant,
      color = variant,
      label = rate,
    ))+
    geom_point()+
    stat_smooth(method=loess, enp.target=10)+
    # geom_path(data=d.m.mean, aes(x=x,y=y))+
    expand_limits(y=0)+
    facet_wrap(~facet, scales="free", ncol=1)+
    cc_scales()+
    my_theme()
  )
}

save(plot.path.breakdown(subset(d,
  nthreads == 32
  # & grepl('^candy$',machines)
  & grepl('.sampa',machines)
  & grepl('repost',workload)
)), name='plot/retwis_breakdown', w=10, h=8)

# d.c <- data.retwis(where="server_conflict LIKE ")
# d.reposts <- adply(subset(d, grepl('_any', server_conflict)), 1, function(r){
#   c <- fromJSON(r$server_conflict)
#   c.reposts <- c[unlist(grepl('reposts_',names(c)))]
#   data.frame(
#     c.reposts
#   )
# })

# stat <- function(d, tag) adply(data.ldbc(where="server_conflict is not null"), 1, function(r) {
#   c <- fromJSON(r$server_conflict)
#   names(c)
#   data.frame(c)
# })

d.s <- subset(d, 
  # nthreads == 8 & grepl('^candy$',machines) & grepl('repost',workload)
  nthreads == 32 & grepl('sampa',machines) & grepl('read-heavy',workload)
)

txns <- gsub('retwis_(.*)_success', '\\1', grep('retwis_.*_success', colnames(d.s), value=T))
d.s[p(txns,'_retry_rate')] <- d.s[p('retwis_',txns,'_retries')] / d.s[p('retwis_',txns,'_count')]

# d.m <- melt(d.s,
#   measure=c(
#     'retwis_newuser_success',
#     'retwis_post_success',
#     'retwis_repost_success',
#     'retwis_timeline_success',
#     'retwis_follow_success'
#   )
# )
# d.m$txn_type <- unlist(lapply(
#   d.m$variable,
#   function(s) gsub('retwis_(\\w+)_success','\\1', s))
# )

d.m <- melt(d.s,
  measure=c(
    'newuser_retry_rate',
    'post_retry_rate',
    'repost_retry_rate',
    'timeline_retry_rate',
    'follow_retry_rate'
  )
)
d.m$txn_type <- unlist(lapply(
  d.m$variable,
  function(s) gsub('(\\w+)_retry_rate','\\1', s))
)

subset(d.m, select=c('rate'))

# d.m$facet <- d.m$txn_type
save(
  ggplot(d.m, aes(
      x = rate,
      y = value,
      fill = variant,
      color = variant,
      group = variant,
  ))+
  ylab('retry rate')+
  # geom_meanbar()+
  stat_summary(fun.y='mean', geom='line')+
  # geom_line()+
  geom_point()+
  # common_layers+
  facet_wrap(~txn_type, scales="free", ncol=5)+
  theme_mine  
  # facet_grid(Graph~ccmode, labeller=label_pretty)
, name='plot/retwis_aborts', w=10, h=5)

save(
  ggplot(subset(d.m, rate == 100), aes(
      x = txn_type,
      y = value,
      fill = variant,
      color = variant,
      group = variant,
  ))+
  ylab('avg retries / txn')+
  # geom_meanbar()+
  stat_summary(fun.y='mean', geom='bar', position='dodge')+
  # geom_line()+
  # geom_point()+
  # common_layers+
  # facet_wrap(~txn_type, scales="free", ncol=5)+
  theme_mine
  # facet_grid(Graph~ccmode, labeller=label_pretty)
, name='plot/retwis_aborts_bar', w=5, h=5)

save(
  ggplot(subset(d.m, rate == 1000), aes(
      x = txn_type,
      y = value,
      fill = variant,
      color = variant,
      group = variant,
      label = value
  ))+
  ylab('avg retries / txn')+xlab('Transaction type')+
  stat_summary(fun.y='mean', geom='text', position=position_dodge(width=0.9), vjust=-0.25, size=0.4)+
  coord_cartesian(ylim=c(0,0.3))+
  stat_summary(fun.y='mean', geom='bar', position='dodge')+
  theme(legend.position=c(0.22,0.85))+
  theme_mine+
  scale_fill_manual(values=my_palette, name='')+
  scale_color_manual(values=my_palette, name='')
  # annotate("text", x = 4, y = , label = "2.1")
, name='plot/retwis_aborts_bar_zoom', w=4, h=4)

