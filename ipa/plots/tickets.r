#!/usr/bin/env Rscript
source('common.r')

sim <- data.or.csv(
  csv = 'data/tickets.csv',
  gen = function() subset(
    data.ipa.tickets(where = "ipa_duration=60 and ipa_version = 'v5.0'"),
    select = c('load', 'honeycomb_mode', 'ipa_bound', 'bound', 'ipa_consistency', 'lease', 'condition',
      'purchase_lat_mean', 'purchase_lat_median', 'purchase_lat_p99',
      'view_lat_mean', 'view_lat_median', 'view_lat_p99', 'overall_lat_mean', 'overall_rate', 'read_weak', 'read_strong'
    )
  )
)

sim <- subset(sim,
  !is.na(condition)
   & grepl('((error: 5%|weak|lat).*#200ms)|(strong.*#0ms)', x(bound,lease))
   & grepl('strong#strong|weak', x(bound,ipa_consistency))
)

google <- data.or.csv(
  csv = 'data/tickets_google.csv',
  gen = function() subset(
    data.ipa.tickets(where = "ipa_duration=60 and ipa_version = 'v6.1google' and honeycomb_mode = 'fast'"),
    select = c('load', 'honeycomb_mode', 'ipa_bound', 'bound', 'ipa_consistency', 'lease', 'condition', 
      'purchase_lat_mean', 'purchase_lat_median', 'purchase_lat_p99',
      'view_lat_mean', 'view_lat_median', 'view_lat_p99', 'overall_lat_mean', 'overall_rate', 'read_weak', 'read_strong'
    )
  )
)

google$condition <- 'GCE\n(actual)'



d <- rbind(sim, google)


d$grp <- d$bound

save(
  ggplot(subset(d,
  grepl('cons|lat|tolerance:0.05',ipa_bound)
        ), aes(
      #y = view_lat_mean,
      # y = purchase_lat_mean,
      y = overall_lat_mean,
      x=grp, color=grp, fill=grp, group=grp
  ))+
  #stat_summary(geom='bar', fun.y=mean, size=0.5, width=0.7)+
  geom_meanbar(position=position_dodge(width = 0.7))+
  #ggtitle('Mean latency')+
  scale_y_log10(breaks=c(10, 20, 50, 100, 500, 1000))+
  ylab('Mean latency (ms)')+
  facet_wrap(~condition, ncol=6, scale="free")+
  theme_mine()+
  # theme(axis.title.x = element_blank())+
  theme.bar()+
  # coord_cartesian(ylim=c(0,500))+
  ipa.scales()
, w=6.5, h=4.0)


ddply(d, .(bound, condition), summarize, 
  percent_strong=mean(read_strong/(read_strong+read_weak)*100), strong=mean(read_strong), weak=mean(read_weak))