#!/usr/bin/env Rscript
source('common.r')

data.prob <- function(d) {
  d <- adply(d, 1, function(r) {
    o <- fromJSON(r$server_conflict)
    
    o$total_potential_ro <- sum(vals(o[grepl('potential_ro',names(o))])*1.0)
    o$total_potential_add <- sum(vals(o[grepl('potential_add',names(o))])*1.0)
    o$p_comm <- (o$total_potential_ro) / (o$count^2)
    o$p_rw <- (o$total_potential_add/1e6 + o$total_potential_ro/1e6) / (o$count^2/1e6)
    
    data.frame(o)
  })

  d$ccv <- revalue(d$cc, c('commutative'='comm', 'reader/writer'='r/w'))
  d$wkld <- revalue(d$workload, c('mixed'='mixed','read-heavy'='read','repost-heavy'='repost'))
  
  return(d)
}

stat.tag <- function(d, tag){
  l <- d[grepl(sprintf('%s_|^cca|^rate$',tag), colnames(d))]
  names(l) <- gsub('potential_','pot_', gsub(sprintf('%s_',tag), '', colnames(l)))
  l
}


# do.all <- function() {

  d.retwis <- data.prob(data.retwis(where="name like '%v0.16.2%' and server_conflict like '%_any%' and nthreads = 32 and machines like '%sampa' and approx = 0"))
  g.retwis <- ddply(d.retwis, .(scale,workload), summarize, p_rw_avg=mean(p_rw), p_rw_sd=sd(p_rw), p_comm_avg=mean(p_comm), p_comm_sd=sd(p_comm))
  g.retwis$p_facet <- with(g.retwis, sprintf("retwis\nscale %s\n%s", scale, workload))
  
  m.retwis <- melt(subset(d.retwis), measure=c('p_rw', 'p_comm'))
  m.retwis$type <- factor(capply(m.retwis$variable, function(s) gsub('p_(\\w+)','\\1', s)), levels=c('rw','comm'))
  m.retwis$prob <- m.retwis$value
  m.retwis$facet <- with(m.retwis, sprintf("scale %s\n%s", scale, workload))
  m.retwis$app <- 'Retwis'
  
  save(ggplot(m.retwis, aes(
    x=type, y=prob, color=type, fill=type
  ))+
    stat_summary(geom='bar', fun.y=mean)+
    xlab('Concurrency control')+ylab('Probability of conflict')+
    expand_limits(y=0)+
    facet_wrap(~facet, ncol=4)+ #, scales="free")+
    cc_scales()+
    my_theme()+
    theme(legend.position="none")
  , name='plot/prob_retwis', w=4, h=4)
  
  
  #   scale         mix     p_rw_avg      p_rw_sd   p_comm_avg    p_comm_sd
  # 1    12 geom_repost 0.0016305066 0.0013213412 0.0007094797 0.0004642239
  # 2    12  read_heavy 0.0004721751 0.0004372620 0.0003187115 0.0002913828
  # 3    16 geom_repost 0.0010597930 0.0006685362 0.0005467570 0.0004794253
  # 4    16  read_heavy 0.0004187280 0.0002750631 0.0002317333 0.0002491577

  d.stress <- data.prob(data.stress(where="name like '%v0.17%' and server_conflict is not null and alpha != '-1'"))
  g.stress <- ddply(d.stress, .(nkeys,workload,alpha,length), summarize, 
    p_rw_avg=mean(p_rw), p_rw_sd=sd(p_rw), p_comm_avg=mean(p_comm), p_comm_sd=sd(p_comm))
  g.stress$p_facet <- with(g.stress, sprintf(
    "raw mix\nzipf %s\n%s\nlen: %s", alpha, workload, length))
  
  m.stress <- melt(subset(d.stress, workload != 'mixed'), measure=c('p_rw', 'p_comm'))
  m.stress$type <- factor(capply(m.stress$variable, function(s) gsub('p_(\\w+)','\\1', s)), levels=c('rw','comm'))
  m.stress$prob <- m.stress$value
  m.stress$facet <- with(m.stress, sprintf("zipf %s\n%s", alpha, workload))
  m.stress$app <- 'Raw Mix'
  
  save(ggplot(m.stress, aes(
    x=type, y=prob, color=type, fill=type
  ))+
    stat_summary(geom='bar', fun.y=mean)+
    xlab('Concurrency control')+ylab('Probability of conflict')+
    expand_limits(y=0)+
    facet_wrap(~facet, ncol=4)+ #, scales="free")+
    cc_scales()+
    my_theme()+
    theme(legend.position="none")
  , name='plot/prob_rawmix', w=4, h=4)
  
  
  #    nkeys           mix alpha length     p_rw_avg      p_rw_sd   p_comm_avg    p_comm_sd
  # 1  10000 mostly_update   0.6      1 0.0008640672 2.486185e-04 0.0004336642 1.012167e-04
  # 2  10000 mostly_update   0.6      2 0.0010811935 6.051591e-04 0.0004372832 7.523450e-05
  # 3  10000 mostly_update   0.6      3 0.0013449193 5.521098e-04 0.0003140705 1.852255e-04
  # 4  10000 mostly_update   0.6      4 0.0014673635 4.882826e-04 0.0002312610 1.748258e-04
  # 5  10000 mostly_update   0.8      1 0.0058294103           NA 0.0028296737           NA
  # 6  10000 mostly_update   0.8      2 0.0091509270           NA 0.0036718952           NA
  # 7  10000 mostly_update   0.8      3 0.0093652072 3.487200e-03 0.0009561442 9.102506e-04
  # 8  10000 mostly_update   0.8      4 0.0072585527 1.117992e-03 0.0003029579 2.719993e-04
  # 9  10000    read_heavy   0.6      1 0.0002538760 4.222583e-05 0.0002294132 3.451768e-05
  # 10 10000    read_heavy   0.6      2 0.0003578371 1.773635e-04 0.0003012113 1.172873e-04
  # 11 10000    read_heavy   0.6      3 0.0005910427 3.061659e-04 0.0003924497 1.450800e-04
  # 12 10000    read_heavy   0.6      4 0.0008019152 2.628339e-04 0.0004276830 1.645452e-04
  # 13 10000    read_heavy   0.8      3 0.0064276186 9.142812e-04 0.0029973472 1.049680e-03
  # 14 10000    read_heavy   0.8      4 0.0065324560 4.942602e-04 0.0020131804 1.194738e-03
  # 15 10000  update_heavy   0.6      1 0.0006925959 1.577167e-04 0.0004479866 8.481389e-05
  # 16 10000  update_heavy   0.6      2 0.0010433954 6.005745e-04 0.0005227230 1.187375e-04
  # 17 10000  update_heavy   0.6      3 0.0012398898 3.446407e-04 0.0004122235 1.957397e-04
  # 18 10000  update_heavy   0.6      4 0.0013163859 3.058664e-04 0.0003104982 2.175509e-04
  # 19 10000  update_heavy   0.8      1 0.0047185042           NA 0.0029856098           NA
  # 20 10000  update_heavy   0.8      3 0.0081568921 2.192215e-03 0.0013916682 1.038846e-03
  # 21 10000  update_heavy   0.8      4 0.0071160205 1.463767e-03 0.0005518392 4.221980e-04

  d.ldbc <- data.prob(data.ldbc(where="name like '%v0.17%up64' and ntotal = 200000 and machines like '%sampa' and snb_time_ratio < 4e-5"))
  g.ldbc <- ddply(d.ldbc, .(nclients), summarize, p_rw_avg=mean(p_rw), p_rw_sd=sd(p_rw), p_comm_avg=mean(p_comm), p_comm_sd=sd(p_comm))
  g.ldbc$p_facet <- "LDBC"
  
  m.ldbc <- melt(subset(d.ldbc), measure=c('p_rw', 'p_comm'))
  m.ldbc$type <- factor(capply(m.ldbc$variable, function(s) gsub('p_(\\w+)','\\1', s)), levels=c('rw','comm'))
  m.ldbc$prob <- m.ldbc$value
  m.ldbc$app <- 'LDBC'
  m.ldbc$facet <- ''
  
  save(ggplot(m.ldbc, aes(
    x=type, y=prob, color=type, fill=type
  ))+
    stat_summary(geom='bar', fun.y=mean)+
    xlab('Concurrency control')+ylab('Probability of conflict')+
    expand_limits(y=0)+
    facet_wrap(~facet, ncol=4)+ #, scales="free")+
    cc_scales()+
    my_theme()+
    theme(legend.position="none")
  , name='plot/prob_ldbc', w=1.5, h=4)
  
  cols <- c('facet','prob','type','app','cc')
  m.all <- rbind(m.stress[cols], m.retwis[cols], m.ldbc[cols])
  m.all$app_facet <- with(m.all, p(app,'\n',facet))
  save(ggplot(subset(m.all), aes(
    x=type, y=prob, fill=type
  ))+
    # stat_summary(geom='bar', fun.y=mean)+
    geom_meanbar()+
    xlab('Concurrency control')+ylab('Probability of conflict')+
    expand_limits(y=0)+
    facet_wrap(~app_facet, ncol=9)+ #, scales="free")+
    cc_scales()+
    my_theme()+
    theme(legend.position="none")
  , name='plot/prob_all', w=10, h=5)
  
  #   snb_time_ratio     p_rw_avg      p_rw_sd   p_comm_avg    p_comm_sd
  # 1          5e-07 0.0005669203 3.974317e-04 6.461379e-06 1.496239e-06
  # 2          1e-06 0.0005468197 3.773548e-04 5.849386e-06 3.707909e-07
  # 3          2e-06 0.0006136770 4.577699e-04 6.893944e-06 1.840338e-06
  # 4          4e-06 0.0005207006 3.518079e-04 6.501708e-06 5.288821e-07
  # 5          8e-06 0.0005309988 3.657752e-04 7.473543e-06 7.829918e-07
  # 6          1e-05 0.0005350550 3.714525e-04 8.532493e-06 2.451067e-06
  # 7          4e-05 0.0002248457 6.173388e-05 4.883953e-06 1.332703e-07
  # 8          8e-05 0.0001767419 1.234908e-05 4.937337e-06 2.314584e-07
  
  
  g.all <- rbind(
             g.stress[grepl('p_',names(g.stress))],
             g.retwis[grepl('p_',names(g.retwis))],
             g.ldbc[grepl('p_',names(g.ldbc))]
           )
  # plot
  
  avg <- melt(g.all[grep('_avg|facet',names(g.all))],id='p_facet')
  sd <- melt(g.all[grep('_sd|facet',names(g.all))],id='p_facet')
  avg$avg <- avg$value
  sd$sd <- sd$value
  avg$cc <- factor(capply(avg$variable, function(s) gsub('p_(\\w+)_avg','\\1', s)), levels=c('rw','comm'))
  # avg$cc <- capply(avg$variable, function(s) gsub('p_(\\w+)_avg','\\1', s))
  avg$facet <- avg$p_facet
  sd$facet <- sd$p_facet
  sd$cc <- factor(capply(sd$variable, function(s) gsub('p_(\\w+)_sd','\\1', s)), levels=c('rw','comm'))
  # sd$cc <- capply(sd$variable, function(s) gsub('p_(\\w+)_sd','\\1', s))
  
  g.m <- merge(avg[,c('facet','cc','avg')],sd[,c('facet','cc','sd')])
  
  save(ggplot(subset(g.m, grepl('raw',facet) & !grepl('mixed',facet)),
    aes(x=cc, y=avg, color=cc, fill=cc))+
    geom_bar()+
    xlab('Concurrency control')+ylab('Probability of conflict')+
    expand_limits(y=0)+
    facet_wrap(~facet, ncol=4)+ #, scales="free")+
    cc_scales()+
    my_theme()+
    theme(legend.position="none")
  , name='plot/prob_bars_rawmix', w=4, h=10)
  
  save(ggplot(subset(g.m, !grepl('raw',facet)),
    aes(x=cc, y=avg, color=cc, fill=cc))+
    geom_bar()+
    xlab('Concurrency control')+ylab('Probability of conflict')+
    expand_limits(y=0)+
    facet_wrap(~facet, ncol=5)+ #, scales="free")+
    cc_scales()+
    my_theme()+
    theme(legend.position="none")
  , name='plot/prob_bars', w=9, h=6)

# }

# do.all()
