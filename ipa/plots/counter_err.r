#!/usr/bin/env Rscript
source('common.r')

#########################
# Measure performance

d <- data.or.csv(
  csv = 'data/counter_err_zipf.csv',
  gen = function() {
    dv3 <- subset(data.ipa.rawmix(where = "datatype='counter' and ipa_duration=60 and ipa_version = 'v3.1'"), honeycomb_mode != 'amazon')

    dv7 <- subset(data.ipa.rawmix(where = "datatype='counter' and ipa_duration=60 and ipa_version = 'v7.1'"), honeycomb_mode == 'amazon')
  
    d <- rbind(dv3, dv7)
    
    subset(d, select = c('load', 'honeycomb_mode', 'ipa_bound', 'bound', 
      'ipa_consistency', 'lease', 'condition', 'mix',
      'read_lat_mean', 'read_lat_median', 'read_lat_p95', 'read_lat_p99', 'read_rate',
      'incr_lat_mean', 'incr_lat_median', 'incr_rate',
      'overall_latency_mean', 'read_strong_fraction'
    ))
  }
)

d$grp <- d$bound

s <- subset(d, 
  ipa_bound != 'consistency:weak'
  & mix == 'default'
  & honeycomb_mode != 'normal'
  & grepl('Local|Uniform|Slow|Geo|High', condition)
  & grepl('tol.*#0ms|cons', x(ipa_bound,lease))
  & ipa_bound != 'tolerance:0'
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
  ylab('Overall mean latency (ms)')+
  theme_mine()+
  # theme(axis.title.x = element_blank())+
  theme.bar()+
  facet_wrap(~condition, scales="free", ncol=6)+
  # coord_cartesian(ylim=c(0,500))+
  ipa.scales()
, 'counter_err_perf', w=6, h=3.5)


#########################
# Measure error

d <- data.or.csv(
  csv = 'data/counter_err.csv',
  gen = function()
  subset(
    data.ipa.rawmix(where = "datatype='counter' and ipa_duration=60 and ipa_version = 'v6.1'"),
    select = c('load', 'honeycomb_mode', 'ipa_bound', 'bound', 
      'mix', 'mix_incr', 'mix_read',
      'ipa_consistency', 'lease', 'condition', 
      'read_lat_mean', 'read_lat_median', 'read_lat_p95', 
      'read_lat_p99', 'read_rate',
      'incr_lat_mean', 'incr_lat_median', 'incr_rate',
      'overall_latency_mean', 'read_strong_fraction', 'interval_mean',
      'interval_percent_mean', 'interval_percent_p99', 'interval_percent_max'
    )
  )
)

d$grp <- d$bound

####################
# Error Bound
####################
print(unique(d$ipa_bound))

d$tol <- factor.remap(d$ipa_bound, list(
  'consistency:strong'='st',
  'tolerance:0'='0%',
  'tolerance:0.01'='1%',
  'tolerance:0.05'='5%',
  'tolerance:0.1'='10%',
  'consistency:weakwrite'='wk'
))

print("after tol")

# d$mix_disp <- factor.remap(d$mix, list(
#   'default' = 'mixed',
#   'read_heavy' = 'read-heavy'
# ))

s <- subset(d,
  !is.na(condition)
  & honeycomb_mode != 'normal'
  & ipa_bound != 'consistency:weak'
  & grepl('cons|tol', ipa_bound)
  & grepl('Uniform|Slow|Google', condition)
)

s.c <- subset(s, mix=='default')
s.c$grp <- with(s.c, x(bound,lease))

# Effect of caching
# save(
#   ggplot(s.c, aes(
#       y = read_lat_mean,
#       x=tol, color='black', fill=grp, group=grp
#   ))+
#   geom_meanbar(position=position_dodge(width = 0.7))+
#   ylab('Mean latency (ms)')+
#   theme_mine()+
#   theme.bar(angle=0, hjust=0.5)+
#   facet_wrap(~condition, scales="free", ncol=6)+
#   # coord_cartesian(ylim=c(0,500))+
#   ipa.scales()
# , w=5, h=3.5)

##########################
# Interval & error width
##########################

d$mode <- factor.remap(d$honeycomb_mode, list(fast='Local', flat5='High load', slowpoke_flat='Slow replica', amazon='Geo-\ndistributed'))

s <- subset(d, ipa_bound != 'tolerance:0.1' & !is.na(mode) & interval_percent_max < 101)
m <- melt.by(s, 'measure', '^interval_percent_(mean|max)')
m$measure <- factor.remap(m$measure, list(mean='mean % error', max='max % error'))

save(ggplot(
  m, aes(
  x=num(mix_incr), color=grp, fill=grp, group=grp,
  y = value
))+
  xlab('fraction increment ops')+
  stat_summary(geom='line', fun.y=mean)+
  facet_grid(measure~mode, scales="free", switch="y")+
  scale_x_continuous(breaks=c(0,0.5,1), labels=c('0','0.5','1'))+
  theme_mine()+
  expand_limits(y=1)+
  theme(axis.title.y=element_blank())+
  ipa.scales(guide=guide_legend())+
  legend.bottom()
, 'counter_err', w=6, h=3.5)
