#!/usr/bin/env Rscript
source('common.r')

d <- data.or.csv(
  csv = 'data/twitter.csv',
  gen = function() {
    s <- data.ipa.twitter(where = "ipa_duration=60 and ipa_version = 'v7.0'")
    g <- data.ipa.twitter(where = "ipa_duration=60 and ipa_version = 'v7.0google'")
    g$condition <- 'GCE\n(actual)'
    d <- rbind(s, g)
    subset(d, select = c(
      'load', 'honeycomb_mode', 'ipa_bound', 'bound',
      'ipa_consistency', 'lease', 'condition',
      'retweet_lat_mean', 'retweet_lat_median', 'retweet_lat_p99',
      'tweet_load_lat_mean', 'tweet_load_lat_median', 'tweet_load_lat_p99',
      'timeline_lat_mean', 'timeline_lat_median', 'timeline_lat_p99',
      'overall_lat_mean', 'overall_rate', 
      'containers'
    ))
  }
)

unique(d$containers)

d$grp <- d$bound

# adjustments to filter out bad experiments where I hadn't fixed the throttled cluster problem
d <- subset(d,
    (condition == 'Local' & overall_lat_mean < 300)
  | (condition == 'Uniform' & overall_lat_mean < 95)
  | (grepl('High', condition) & overall_lat_mean < 500)
  | (grepl('Slow', condition) & overall_lat_mean < 600)
  | (grepl('Slow', condition) & overall_lat_mean < 600)  
  | (grepl('Geo', condition) & overall_lat_mean < 4500)
  | (grepl('GCE', condition) & load == 2048)
)

save(
  ggplot(subset(d,
    !is.na(condition)
    
    & grepl('cons|lat|tolerance:0.05',ipa_bound)
        ), aes(
      #y = view_lat_mean,
      # y = purchase_lat_mean,
      y = overall_lat_mean,
      x=grp, color=grp, fill=grp, group=grp
  ))+
  #stat_summary(geom='bar', fun.y=mean, size=0.5, width=0.7)+
  geom_meanbar(position=position_dodge(width = 0.7))+
  # geom_point(position=position_dodge(width = 0.7), color='black')+
  #ggtitle('Mean latency')+
  # scale_y_log10(breaks=c(10, 20, 50, 100, 500, 1000))+
  ylab('Mean latency (ms)')+
  facet_wrap(~condition, ncol=6, scale="free")+
  theme_mine()+
  # theme(axis.title.x = element_blank())+
  theme.bar()+
  # coord_cartesian(ylim=c(0,500))+
  ipa.scales()
, w=6.5, h=4.0)


# ddply(d, .(bound, condition), summarize,
#   percent_strong=mean(read_strong/(read_strong+read_weak)*100), strong=mean(read_strong), weak=mean(read_weak))