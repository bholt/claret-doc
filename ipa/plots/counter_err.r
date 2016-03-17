#!/usr/bin/env Rscript
source('common.r')

d <-
# d <- data.or.csv(
  # csv = 'data/counter_err.csv',
  # gen = function() 
  subset(
    data.ipa.rawmix(where = "datatype='counter' and ipa_duration=60 and ipa_version = 'v3.1'"),
    select = c('load', 'honeycomb_mode', 'ipa_bound', 'bound', 'mix',
      'ipa_consistency', 'lease', 'condition', 
      'read_lat_mean', 'read_lat_median', 'read_lat_p95', 
      'read_lat_p99', 'read_rate',
      'incr_lat_mean', 'incr_lat_median', 'incr_rate',
      'overall_latency_mean', 'read_strong_fraction', 'interval_mean'
    )
  )
# )

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
save(
  ggplot(s.c, aes(
      y = read_lat_mean,
      x=tol, color='black', fill=grp, group=grp 
  ))+
  geom_meanbar(position=position_dodge(width = 0.7))+
  ylab('Mean latency (ms)')+
  theme_mine()+
  theme.bar(angle=0, hjust=0.5)+
  facet_wrap(~condition, scales="free", ncol=6)+
  # coord_cartesian(ylim=c(0,500))+
  ipa.scales()
, w=5, h=3.5)

# Interval width
save(
  ggplot(subset(s, grepl('tol', ipa_bound) ), aes(
      y = interval_mean,
      x=tol, color='black', fill=grp, group=x(grp,lease) 
  ))+
  geom_meanbar(position=position_dodge(width = 0.7))+
  ylab('Mean latency (ms)')+
  theme_mine()+
  theme.bar(angle=0, hjust=0.5)+
  facet_grid(condition~mix, scales="free")+
  # coord_cartesian(ylim=c(0,500))+
  ipa.scales()
, 'counter_err_width', w=5, h=3.5)
