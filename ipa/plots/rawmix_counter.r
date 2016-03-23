#!/usr/bin/env Rscript
source('common.r')

d <- data.or.csv(
  csv = 'data/rawmix-counter.csv',
  gen = function() subset(
    data.ipa.rawmix(where = "datatype='counter' and ipa_duration=60 and ipa_version = 'v3.1'"),
    select = c('load', 'honeycomb_mode', 'ipa_bound', 'bound', 
      'ipa_consistency', 'lease', 'condition', 'mix',
      'read_lat_mean', 'read_lat_median', 'read_lat_p95', 'read_lat_p99', 'read_rate',
      'incr_lat_mean', 'incr_lat_median', 'incr_rate',
      'overall_latency_mean', 'read_strong_fraction'
    )
  )
)

d$grp <- d$bound

s <- subset(d, 
  ipa_bound != 'consistency:weak'
  & mix == 'default'
  & honeycomb_mode != 'normal'
  & grepl('Local|Uniform|Slow|Geo|High', condition)
  & grepl('tol.*#0ms|cons|lat', x(ipa_bound,lease))
)


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
s$percent_weak <- (1 - s$read_strong_fraction) * 100

s.l <- subset(s,
  !is.na(condition) 
  # & ipa_bound != 'consistency:weak'
  & grepl('cons|lat', ipa_bound)
  & grepl('Local|Uniform|Slow|Geo|High', condition)
)

save(
  ggplot(s.l, aes(
      # y = overall_latency_mean,
      y = read_lat_mean,
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
, 'counter_lbound', w=5, h=3.5)

save(
  ggplot(s.l, aes(
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
, 'counter_lbound_consistency', w=5, h=3)

print(subset(
  ddply(s.l, .(condition, bound), summarize, percent_weak=mean((1-read_strong_fraction)*100)),
))

save(
  ggplot(s.l, aes(
      y = read_lat_p95,
      x=grp, color='black', fill=grp, group=grp
  ))+
  # stat_summary(geom='bar', fun.y='mean')+
  geom_meanbar(position=position_dodge(width = 0.7))+
  ylab('95th percentile latency')+
  theme_mine()+
  # theme(axis.title.x = element_blank())+
  theme.bar()+
  facet_wrap(~condition, scales="free", ncol=6)+
  # coord_cartesian(ylim=c(0,500))+
  ipa.scales()
, 'counter_lbound_tail', w=5, h=3)



