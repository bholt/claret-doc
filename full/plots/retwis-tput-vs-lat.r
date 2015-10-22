#!/usr/bin/env Rscript
source('common.r')
a <- parse.args()

txns <- c('follow','newuser','post','repost','timeline')

d <- tryCatch(
  {
    d <- data.retwis(where="(name like '%v0.30%' and nclients = 4)")
    # d <- data.retwis(where="name like '%v0.29%' and nclients = 4")
    # d <- subset(d, grepl("\\.(3|4)", name) & grepl('_heavy',mix) & max_retries == 100)
    # d <- subset(d, avg_latency_ms < 50)
    
    # d$throughput <- d$retwis_txn_count * num(d$nclients) / d$total_time
    
    # d$throughput <- with(d, (
    #   (  retwis_follow_count +
    #     retwis_newuser_count +
    #     retwis_post_count +
    #     retwis_repost_count +
    #     retwis_timeline_count ) * num(nclients)
    #   / total_time
    # ))
    
    write.csv(subset(d,
      select = c('name', 'nclients', 'nthreads', 'cc', 'rate', 'scale', 'mix', 'workload', 'alpha', 'phasing', 'async', 'cc_ph', 'timeout_scaling', 'throughput', 'op_timeouts', 'avg_latency_ms', 'txn_failed', 'total_time')), 
      file = 'data/retwis-tput-vs-lat.csv')
    d
  }, error = function(e) {
    error.database_unreachable(e)
    print("!!")
    d <- db.csv(file = 'data/retwis-tput-vs-lat.csv')
  }
)

d <- subset(d, cc_ph %in% c(RW+PH, COMM, COMM+PH, COMB+PH, NOTXN))
d$cc_ph <- factor(d$cc_ph, levels = rev(c(RW+PH, COMM, COMM+PH, COMB+PH, NOTXN)))

d$x <- d$nthreads * num(d$nclients)
d$label <- d$nthreads * num(d$nclients) + "x" + d$rate
d$facet <- with(d, workload) # + "\n" + timeout_scaling)

# d$retwis_txn_count <- d$retwis_follow_count + d$retwis_newuser_count + d$retwis_post_count + d$retwis_repost_count + d$retwis_timeline_count / 10
# d$throughput <- d$retwis_txn_count * num(d$nclients) / d$total_time

d <- subset(d, async == 0 & txn_failed < 20 & total_time > 60 & total_time < 65)

save(
  ggplot(d, aes(
    x = throughput,
    y = avg_latency_ms,
    group = cc_ph, fill = cc_ph, color = cc_ph, linetype = cc_ph
  ))+
  xlab('Throughput (txn/s)')+ylab('Mean latency (ms)')+
  # geom_point()+
  # geom_text(aes(label=label), size=1.7)+
  # scale_x_continuous(labels=si.labels())+
  scale_x_continuous(labels = function(x){ x/1000+'k' })+
  # geom_point()+
  geom_mean_path(d, throughput, avg_latency_ms, .(x,facet,cc,phasing,cc_ph))+
  expand_limits(y=0)+
  facet_wrap(~facet, scales="free_x")+
  # cc_scales()+phasing.linetype()+
  cc_ph_scales()+
  coord_cartesian(ylim=c(0,100))+
  my_theme() #+theme(legend.position='bottom')
, 'retwis-tput-vs-lat', w=5, h=3)

save(
  ggplot(subset(d, x <= 384 & x >= 16), aes(
    x = x,
    y = throughput,
    group = cc_ph, fill = cc_ph, color = cc_ph, linetype = cc_ph
  ))+
  xlab('Clients')+ylab('Throughput (txn/s)')+
  stat_summary(geom='line', fun.y=mean)+
  stat_summary(geom='point', fun.y=mean)+  
  scale_x_continuous(trans=log2_trans(), breaks=c(8,16,32,64,128,256,384))+
  scale_y_continuous(labels = function(x){ x/1000+'k' })+
  expand_limits(x=0, y=0)+
  facet_wrap(~facet, scales="free")+
  cc_ph_scales(guide = guide_legend(nrow = 5))+
  my_theme()
, 'retwis-tput', w=5.5, h=2.5)
