#!/usr/bin/env Rscript
source('common.r')

d <- data.or.csv(
  csv = 'data/rawmix-counter.csv',
  gen = function() {
    # dv3 <- subset(data.ipa.rawmix(where = "datatype='counter' and ipa_duration=60 and ipa_version = 'v3.1'"), honeycomb_mode != 'amazon')
  
    d <- data.ipa.rawmix(where = "datatype='counter' and ipa_duration=60 and (ipa_version LIKE 'v7.%' or ipa_version = 'v3.1')")
    
    # d <- rbind(dv3, dv7)
  
    subset(d,
      select = c('load', 'honeycomb_mode', 'ipa_bound', 'bound', 
        'ipa_consistency', 'lease', 'condition', 'mix',
        'read_lat_mean', 'read_lat_median', 'read_lat_p95', 'read_lat_p99', 'read_rate',
        'incr_lat_mean', 'incr_lat_median', 'incr_rate',
        'overall_latency_mean', 'read_strong_fraction', 'ipa_version'
      )
    )
  }
)

d$grp <- d$bound

s <- subset(d, 
  ipa_bound != 'consistency:weak'
  & mix == 'default'
  & honeycomb_mode != 'normal'
  & grepl('Local|Uniform|Slow|Geo|High', condition)
  & grepl('tol.*#0ms|cons|lat', x(ipa_bound,lease))
  & ipa_bound != 'tolerance:0'
)


save(
  ggplot(subset(s, !is.na(condition) & ipa_version == 'v7.2'
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
s$percent_strong <- s$read_strong_fraction * 100
s$percent_weak <- (1 - s$read_strong_fraction) * 100

s.l <- subset(s,
  !is.na(condition) 
  # & ipa_bound != 'consistency:weak'
  & grepl('cons|lat', ipa_bound)
  & grepl('Uniform|Slow|Geo|High', condition)
  & ipa_bound != 'latency:20ms'
)

s.tail <- subset(s.l, (ipa_version == 'v7.1' & honeycomb_mode == 'amazon') | (ipa_version == 'v3.1' & honeycomb_mode != 'amazon'))
s.l <- subset(s.l, ipa_version == 'v7.2')

v.strong <- round(mean(subset(s.l, honeycomb_mode == 'amazon' & bound == 'strong')$read_lat_mean), 0)
ann.strong <- data.frame(
    condition = factor('Geo-\ndistributed', levels=vals(conds)),
    grp='strong',
    read_lat_mean = 82,
    label = '(' + v.strong + ' ms)'
)

s.l.l <- ddply(subset(s.l), .(bound, condition, grp), summarize, y=round(mean(percent_strong),0), read_lat_mean=mean(read_lat_mean))

s.l$label <- s.l$percent_weak

save(
  ggplot(s.l, aes(
      # y = overall_latency_mean,
      y = read_lat_mean,
      x=grp, color='black', fill=grp, group=grp
  ))+
  geom_meanbar(position=position_dodge(width = 0.7))+
  geom_text(aes(label=y+'%', y = -5), data = s.l.l, color='black', size=2.3, family='Helvetica')+
  geom_text(aes(label=label), data = ann.strong, color='black', size=2.5, family='Helvetica', hjust=-0.2)+
  
  scale_y_continuous(breaks=c(0, 10, 25, 50, 75, 100))+
  
  ylab('Mean latency (ms)')+
  theme_mine()+
  # theme(axis.title.x = element_blank())+
  theme.bar()+
  facet_wrap(~condition, scales="free_x", ncol=6)+
  coord_cartesian(ylim=c(-5,80))+
  # coord_cartesian(ylim=c(0,100))+
  ipa.scales()
, 'counter_lbound', w=5, h=3.5)
#
# save(
#   ggplot(s.l, aes(
#       y = (1 - read_strong_fraction) * 100,
#       x=grp, color='black', fill=grp, group=grp
#   ))+
#   stat_summary(geom='bar', fun.y='mean')+
#   # geom_meanbar(position=position_dodge(width = 0.7))+
#   ylab('% weak consistency')+
#   theme_mine()+
#   # theme(axis.title.x = element_blank())+
#   theme.bar()+
#   facet_wrap(~condition, scales="fixed", ncol=6)+
#   # coord_cartesian(ylim=c(0,500))+
#   ipa.scales()
# , 'counter_lbound_consistency', w=5, h=3)

print(subset(
  ddply(s.l, .(condition, bound), summarize, percent_weak=mean((1-read_strong_fraction)*100)),
))


s.l <- s.tail

v.strong <- round(mean(subset(s.l, honeycomb_mode == 'amazon' & bound == 'strong')$read_lat_p95), 0)
ann.strong <- data.frame(
    condition = factor('Geo-\ndistributed', levels=vals(conds)),
    grp='strong',
    read_lat_p95 = 162.5,
    label = '(' + v.strong + ' ms)'
)

save(
  ggplot(s.l, aes(
      y = read_lat_p95,
      x=grp, color='black', fill=grp, group=grp
  ))+
  # stat_summary(geom='bar', fun.y='mean')+
  geom_meanbar(position=position_dodge(width = 0.7))+
  # geom_text(aes(label=ipa_version), color='black', size=1)+
  geom_text(aes(label=label), data = ann.strong, color='black', size=2.5, family='Helvetica', hjust=-0.2)+
  
  ylab('95th percentile latency')+
  theme_mine()+
  scale_y_continuous(breaks=c(0, 10, 50, 100, 150, 200), minor_breaks=c(25, 75, 125))+
  # theme(axis.title.x = element_blank())+
  theme.bar()+
  facet_wrap(~condition, scales="free_x", ncol=6)+
  coord_cartesian(ylim=c(0,160))+
  ipa.scales()
, 'counter_lbound_tail', w=5, h=3)



