#!/usr/bin/env Rscript
source('common.r')

d <- data.or.csv(
  csv = 'data/tickets_google.csv',
  gen = function() subset(
    data.ipa.tickets(where = "ipa_duration=60 and ipa_version = 'v6.1google' and honeycomb_mode = 'fast'"),
    select = c('load', 'honeycomb_mode', 'ipa_bound', 'bound', 'ipa_consistency', 'lease', 'condition', 
      'purchase_lat_mean', 'purchase_lat_median', 'purchase_lat_p99',
      'view_lat_mean', 'view_lat_median', 'view_lat_p99', 'overall_lat_mean', 'overall_rate'
    )
  )
)


log(nrow(d) + " rows")

d$grp <- d$bound

save(
  ggplot(subset(d, load == 2048
  ), aes(
      y = overall_lat_mean,
      # y = purchase_lat_mean,
      x = grp,
      color=grp, fill=grp, group=grp
  ))+
  #stat_summary(geom='bar', fun.y=mean, size=0.5, width=0.7)+
  geom_meanbar(position=position_dodge(width = 0.7))+
  #ggtitle('Mean latency')+
  ylab('Mean latency (ms)')+
  #facet_wrap(~condition, ncol=6, scale="free")+
  theme_mine()+
  scale_y_continuous(breaks=c(0, 100, 500, 1000, 2000))+
  theme(axis.title.x = element_blank())+
  theme.bar()+
  # coord_cartesian(ylim=c(0,500))+
  ipa.scales()
, w=3, h=4.0)


s <- subset(d) #, ipa_bound != 'tolerance:0.01')

# save(
#   ggplot(s, aes(
#       y = overall_lat_mean,
#       x = load,
#       color=grp, fill=grp, group=grp
#   ))+
#   stat_summary(geom='line', fun.y=mean)+
#   ylab('Mean latency (ms)')+
#   theme_mine()+
#   scale_y_continuous(breaks=c(0, 100, 500, 1000, 2000))+
#   theme(axis.title.x = element_blank())+
#   # theme.bar()+
#   # coord_cartesian(ylim=c(0,1000))+
#   ipa.scales()
# , 'tickets_google_lat', w=4.5, h=4.0)

save(
  ggplot(subset(s, grepl('tolerance:0.05|cons|lat', ipa_bound)), aes(
      y = overall_rate,
      x = load,
      color=grp, fill=grp, group=grp
  ))+
  stat_summary(geom='line', fun.y=mean)+
  xlab('# concurrent clients')+
  ylab('Throughput (actions/s)')+
  theme_mine()+
  scale_y_continuous(labels=k.labels)+
  # scale_y_continuous(breaks=c(0, 100, 500, 1000, 2000))+
  # theme(axis.title.x = element_blank())+
  # theme.bar()+
  # coord_cartesian(ylim=c(0,1000))+
  ipa.scales(guide = guide_legend(nrow=4))
, 'tickets_google_rate', w=4.5, h=2.5)


# save(
#   ggplot(s, aes(
#       y = overall_lat_mean,
#       x = overall_rate,
#       color=grp, fill=grp, group=grp
#   ))+
#   geom_point()+
#   geom_mean_path(s, overall_rate, overall_lat_mean, .(load, grp))+
#   ylab('Mean latency (ms)')+
#   #facet_wrap(~condition, ncol=6, scale="free")+
#   theme_mine()+
#   scale_y_continuous(breaks=c(0, 100, 500, 1000, 2000))+
#   theme(axis.title.x = element_blank())+
#   theme.bar()+
#   coord_cartesian(ylim=c(0,1000))+
#   ipa.scales()
# , 'tickets_google_tput_v_lat', w=4.5, h=4.0)
