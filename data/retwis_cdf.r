#!/usr/bin/env Rscript
source('common.r')

df <- subset(db("
    select * from tapir where stat_following_counts is not null
    and name like '%v0.14%'
"),
  nclients == 32
  & initusers == 4096
)
df$grp <- with(df, sprintf("%s\n%s\nmix:%s/%s,\n%s", name, ccmode, mix, alpha, gen))

histogram.facets <- function(df, measure, grp) {
  d <- data.frame(x=c(),y=c(),version=c())
  for (i in 1:nrow(df)) {
    d <- rbind(d, df.histogram(df[i,measure], df[i,grp]))
  }
  return(d)
}

d.follow <- histogram.facets(subset(df,
    initusers == 4096 & mix == 'geom_repost'
), 'stat_follower_counts', 'grp')
save(ggplot(d.follow, aes(x=x, weight=y))+
    stat_ecdf(color=c.blue)+
    xlab('# followers / user (log scale)')+ylab('CDF (log scale)')+
    scale_x_log10(breaks=c(1,10,100,1000))+scale_y_log10(breaks=c(0.1,0.5,1.0))+
    theme_mine
, name='plot/retwis_cdf_followers', w=4, h=4)

d.repost <- histogram.facets(
    subset(df, initusers == 4096 & mix == 'geom_repost')
, 'stat_repost_counts', 'grp')
save(ggplot(d.repost, aes(x=x, weight=y))+
    stat_ecdf(color=c.blue)+
    xlab('# reposts')+ylab('count')+
    scale_x_log10(breaks=c(1,10,100,1000))+
    scale_y_log10(breaks=c(0.1,0.2,0.4,0.6,0.8,1.0))+
    xlab('# reposts (log scale)')+ylab('CDF (log scale)')+
    theme_mine
, name='plot/retwis_cdf_reposts', w=4, h=4)
