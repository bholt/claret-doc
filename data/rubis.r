#!/usr/bin/env Rscript
# DATA.MODE <- 'local'
source('common.r')

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
txns <- names(codes)

pp.info <- function(r) cat("## "+r$machines+" | "+r$cca+" | nclients: "+r$nclients+" | nthreads: "+r$nthreads+"\n")

pp.txn_conflicts <- function(r) {
  o <- fromJSON(jsfix(replace_txn_codes(r$server_txn_conflicts,codes)))
  pp.info(r)
  pp(o[order(names(o))])
}

pp.txn_conflict_on_tag <- function(r) {
  pp.info(r)
  prettify(jsfix(replace_txn_codes(r$server_txn_conflict_on_tag, codes)), indent=2)
}

json_to_hist <- function(str,...) {
  l <- as.list(fromJSON(jsfix(str)))
  data.frame(x=num(names(l)), y=vals(l))
}

# h <- hist(as.vector(rep(df$x, df$y)),plot=F,...)
# data.frame(top=h$breaks[-1], count=h$counts)

# pp.bid_time_hist <- function(r) {
#   pp.info(r)
#   cat(bid_time_hist(r))
# }

d <- data.rubis(where="duration = 30 and name like 'v0.22.1%' and nclients = 4 and nthreads = 64 and rate = 50")
d1 <- d[1,]
l1 <- as.list(fromJSON(jsfix(d1$server_bid_time_hist)))
df1 <- data.frame(x=num(names(l1)), y=vals(l1))
df1 <- df1[order(df1$x),]
df1v <- as.vector(rep(df1$x,  df1$y))

save(
  ggplot(df1, aes(x=x,weight=y))+
    geom_histogram(binwidth=0.02, fill=c.blue)+
    # stat_ecdf()+
    xlab('Percentage auction time')+ylab('# of bids')+
    theme_mine
, 'plot/rubis_bid_hist', w=4, h=4)

save(
  ggplot(json_to_hist(d1$stat_nbids_hist), aes(x=x,weight=y))+
    geom_histogram(binwidth=0.02, fill=c.blue)+
    # stat_ecdf()+
    xlab('Percentage auction time')+ylab('# of bids')+
    theme_mine
, 'plot/rubis_bids_per_item_hist', w=4, h=4)


# d <- data.rubis(where="nclients = 4 and nthreads = 32 and duration = 60 and name like 'v0.21%'")
# d$x <- d$rate * d$nthreads * num(d$nclients)
# odir <- 'plot/rubis/rates'

d <- data.rubis(where="duration = 30 and name like 'v0.22%' and nclients = 4")
d$x <- d$nthreads * num(d$nclients)
d$label <- d$nthreads * num(d$nclients) + "x" + d$rate
odir <- 'plot/rubis/threads'

d$facet <- with(d, state+" (z:"+alpha+") | "+mix )


save(
  ggplot(subset(d), aes(
    x = x,
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
, name=odir+'/rubis_explore', w=5, h=4)

save(
  ggplot(subset(d), aes(
    x = x,
    y = txn_retries / txn_count,
    group = cc,
    fill = cc,
    color = cc,
  ))+
  geom_point()+
  stat_summary(geom='line', fun.y=mean)+
  expand_limits(y=0, x=0)+
  facet_wrap(~facet, scales="free")+
  cc_scales()+
  my_theme()+theme(legend.position='bottom')
, name=odir+'/rubis_retries', w=5, h=4)

save(
  ggplot(subset(d), aes(
    x = x,
    y = combined_adds,
    group = cc,
    fill = cc,
    color = cc,
  ))+
  geom_point()+
  stat_summary(geom='line', fun.y=mean)+
  expand_limits(y=0, x=0)+
  facet_wrap(~facet, scales="free")+
  cc_scales()+
  my_theme()+theme(legend.position='bottom')
, name=odir+'/rubis_combining', w=5, h=4)

save(
  ggplot(subset(d), aes(
    x = throughput,
    y = avg_latency_ms,
    group = cc,
    fill = cc,
    color = cc,
  ))+
  xlab('Throughput (txns/sec)')+ylab('Mean latency (ms)')+
  # geom_point()+
  geom_text(aes(label=label), size=1.7)+
  geom_mean_path(d, throughput, avg_latency_ms, .(cc,x,facet))+
  expand_limits(y=0, x=0)+
  facet_wrap(~facet, scales="free")+
  cc_scales()+
  my_theme()+theme(legend.position='bottom')
, name=odir+'/rubis_tput_v_lat', w=8, h=7)

dm <- melt(d, measure=p('rubis_',txns,'_avg_latency_ms'))
dm$latency_ms <- dm$value
dm$txn <- unlist(lapply(dm$variable, function(s) gsub('rubis_(.*)_avg_latency_ms','\\1', s)))
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
  geom_mean_path(dm, throughput, latency_ms, .(cc,x,facet))+
  expand_limits(y=0)+
  facet_wrap(~facet, ncol=4)+
  cc_scales()+
  theme(legend.position='bottom')+
  my_theme()
, name=odir+'/rubis_tput_v_lat_breakdown', w=10, h=8)

dm <- melt(d, measure=p('rubis_',txns,'_retries'))
dm$txn <- unlist(lapply(dm$variable, function(s) gsub('rubis_(.*)_retries','\\1', s)))
dm$retries <- dm$value
save(
  ggplot(dm, aes(
    x = x,
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
, name=odir+'/rubis_retries_breakdown', w=8, h=7)

