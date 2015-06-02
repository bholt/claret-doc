#!/usr/bin/env Rscript
# DATA.MODE <- 'local'
source('common.r')
d <- data.rubis(where="nclients = 4 and nthreads = 32 and duration = 60 and name like 'v0.20.1%'")

# txns <- gsub('rubis_(.*)_count', '\\1', names(d[,grepl('rubis_(.*)_count',names(d))]))

# codes <- list(
#   c('AddUser',      '1'),
#   c('UserSummary',  '2'),
#   c('OpenAuction',  '3'),
#   c('CloseAuction', '4'),
#   c('NewBid',       '5'),
#   c('BrowseItems',  '6'),
#   c('ViewAuction',  '7'),
#   c('NewComment',   '8')
# )

codes <- list(
  AddUser =      1,
  UserSummary =  2,
  OpenAuction =  3,
  CloseAuction = 4,
  NewBid =       5,
  BrowseItems =  6,
  ViewAuction =  7,
  NewComment =   8
)

# names(c) <- replace_txn_codes(names(c), cl)

txns <- names(codes)

d$facet <- with(d, state+" (z:"+alpha+") | "+mix )

save(
  ggplot(subset(d), aes(
    x = rate,
    y = throughput,
    group = cc,
    fill = cc,
    color = cc,
  ))+
  geom_point()+
  stat_summary(geom='line', fun.y=mean)+
  expand_limits(y=0)+
  facet_wrap(~facet, scales="free")+
  cc_scales()+
  my_theme()+theme(legend.position='bottom')
, name='plot/rubis_explore', w=8, h=7)

save(
  ggplot(subset(d), aes(
    x = throughput,
    y = avg_latency_ms,
    group = cc,
    fill = cc,
    color = cc,
  ))+
  xlab('Throughput (txns/sec)')+ylab('Mean latency (ms)')+
  geom_point()+
  geom_mean_path(d, throughput, avg_latency_ms, .(cc,rate,facet))+
  expand_limits(y=0)+
  facet_wrap(~facet, scales="free")+
  cc_scales()+
  my_theme()+theme(legend.position='bottom')
, name='plot/rubis_tput_v_lat', w=8, h=7)

for(t in txns) d['rubis_'+t+'_lat_ms'] <- num(d[,'rubis_'+t+'_latency']) * 1e3 / num(d[,'rubis_'+t+'_count'])
dm <- melt(d, measure=p('rubis_',txns,'_lat_ms'))
dm$latency_ms <- dm$value
dm$txn <- unlist(lapply(dm$variable, function(s) gsub('rubis_(.*)_lat_ms','\\1', s)))
dm$facet <- dm$txn
save(
  ggplot(dm, aes(
    x = throughput,
    y = latency_ms,
    group = cc,
    fill = cc,
    color = cc,
    shape = cc,
  ))+
  xlab('Throughput (txns/sec)')+ylab('Mean latency (ms)')+
  geom_point()+
  geom_mean_path(dm, throughput, latency_ms, .(cc,rate,facet))+
  expand_limits(y=0)+
  facet_wrap(~facet, scales="free", ncol=5)+
  cc_scales()+
  theme(legend.position='bottom')+
  my_theme()
, name='plot/rubis_tput_v_lat_breakdown', w=10, h=8)

dm <- melt(d, measure=p('rubis_',txns,'_retries'))
dm$txn <- unlist(lapply(dm$variable, function(s) gsub('rubis_(.*)_retries','\\1', s)))
dm$retries <- dm$value
save(
  ggplot(dm, aes(
    x = rate,
    y = retries,
    group = txn,
    fill = txn,
    color = txn,
  ))+
  geom_point()+
  stat_summary(geom='line', fun.y=mean)+
  expand_limits(y=0)+
  facet_wrap(~cc, scales="free")+
  # cc_scales()+
  my_theme()+theme(legend.position='bottom')
, name='plot/rubis_retries_breakdown', w=8, h=7)

