#!/usr/bin/env Rscript
source('common.r')

d <- data.or.csv(
  csv = 'data/tickets.csv',
  gen = function() subset(
    data.ipa.tickets(where = "ipa_duration=60 and ipa_version = 'v5.0'"),
    select = c('load', 'honeycomb_mode', 'bound', 'ipa_consistency', 'lease', 'condition', 
      'purchase_lat_mean', 'purchase_lat_median', 'purchase_lat_p99',
      'view_lat_mean', 'view_lat_median', 'view_lat_p99'
    )
  )
)

d$grp <- d$bound

save(
  ggplot(subset(d, !is.na(condition)
         & grepl('((error: 5%|weak|lat).*#200ms)|(strong.*#0ms)', x(bound,lease))
         & grepl('strong#strong|weak', x(bound,ipa_consistency))
        ), aes(
      #y = view_lat_mean,
      y = purchase_lat_mean,
      x=condition, color=grp, fill=grp, group=grp
  ))+
  #stat_summary(geom='bar', fun.y=mean, size=0.5, width=0.7)+
  geom_meanbar(position=position_dodge(width = 0.7))+
  #ggtitle('Mean latency')+
  ylab('Mean latency (ms)')+
  #facet_wrap(~condition, ncol=6, scale="free")+
  theme_mine()+
  theme(axis.title.x = element_blank())+
  #theme.bar()+
  coord_cartesian(ylim=c(0,500))+
  ipa.scales()
, w=5.5, h=4.0)
