#!/usr/bin/env Rscript
source('common.r')

d <- data.or.csv(
  csv = 'data/rawmix-counter.csv',
  gen = function() subset(
    data.ipa.rawmix(where = "datatype='counter' and ipa_duration=60 and ipa_version = 'v3.1'"),
    select = c('load', 'honeycomb_mode', 'ipa_bound', 'bound', 'ipa_consistency', 'lease', 'condition', 
      'read_lat_mean', 'read_lat_median', 'read_lat_p99', 'read_rate',
      'incr_lat_mean', 'incr_lat_median', 'incr_lat_p99', 'incr_rate',
      'overall_latency_mean', 'read_strong_fraction'
    )
  )
)

d$grp <- d$bound

s <- subset(d, ipa_bound != 'consistency:weak' & honeycomb_mode != 'normal')


save(
  ggplot(subset(s, !is.na(condition)
    #& grepl('(weakwrite)', x(ipa_bound,lease))
         # & grepl('((error: 5%|weak|lat).*#200ms)|(strong.*#0ms)', x(bound,lease))
         # & grepl('strong#strong|weak', x(bound,ipa_consistency))
        ), aes(
      y = overall_latency_mean,
      x=grp, color='black', fill=grp, group=grp
  ))+
  geom_meanbar(position=position_dodge(width = 0.7))+
  ylab('Mean latency (ms)')+
  theme_mine()+
  # theme(axis.title.x = element_blank())+
  theme.bar()+
  facet_wrap(~condition, scales="free", ncol=6)+
  # coord_cartesian(ylim=c(0,500))+
  ipa.scales()
, w=7.5, h=3.5)

####################
# Latency Bound
####################
save(
  ggplot(subset(s, !is.na(condition) 
    & grepl('cons|lat', ipa_bound)
    & grepl('flat5|slowpoke|google', honeycomb_mode)
    #& grepl('(weakwrite)', x(ipa_bound,lease))
         # & grepl('((error: 5%|weak|lat).*#200ms)|(strong.*#0ms)', x(bound,lease))
         # & grepl('strong#strong|weak', x(bound,ipa_consistency))
        ), aes(
      y = overall_latency_mean,
      x=grp, color='black', fill=grp, group=grp
  ))+
  geom_meanbar(position=position_dodge(width = 0.7))+
  ylab('Mean latency (ms)')+
  theme_mine()+
  # theme(axis.title.x = element_blank())+
  theme.bar()+
  facet_wrap(~condition, scales="free", ncol=6)+
  # coord_cartesian(ylim=c(0,500))+
  ipa.scales()
, 'counter-lbound', w=5, h=3.5)

save(
  ggplot(subset(s, !is.na(condition) 
    & grepl('cons|lat', ipa_bound)
    & grepl('flat5|slowpoke|google', honeycomb_mode)
    #& grepl('(weakwrite)', x(ipa_bound,lease))
         # & grepl('((error: 5%|weak|lat).*#200ms)|(strong.*#0ms)', x(bound,lease))
         # & grepl('strong#strong|weak', x(bound,ipa_consistency))
        ), aes(
      y = (1 - read_strong_fraction) * 100,
      x=grp, color='black', fill=grp, group=grp
  ))+
  stat_summary(geom='bar', fun.y='mean')+
  # geom_meanbar(position=position_dodge(width = 0.7))+
  ylab('% weak consistency')+
  theme_mine()+
  # theme(axis.title.x = element_blank())+
  theme.bar()+
  facet_wrap(~condition, scales="free", ncol=6)+
  # coord_cartesian(ylim=c(0,500))+
  ipa.scales()
, 'counter-lbound-consistency', w=5, h=3.5)
