#!/usr/bin/env Rscript
source('common.r')
a <- parse.args()

txns <- c('follow','newuser','post','repost','timeline')

d <- tryCatch(
  {
    d <- data.retwis(where="name like '%v0.27.5%' and nclients = 2")
    # d <- subset(d, grepl("\\.(3|4)", name) & grepl('_heavy',mix) & max_retries == 100)
    # d <- subset(d, avg_latency_ms < 50)
    
    d$throughtput <- d$retwis_txn_count * num(d$nclients) / d$total_time
    
    # d$throughput <- with(d, (
    #   (  retwis_follow_count +
    #     retwis_newuser_count +
    #     retwis_post_count +
    #     retwis_repost_count +
    #     retwis_timeline_count ) * num(nclients)
    #   / total_time
    # ))
    
    write.csv(subset(d, select = c('name', 'nclients', 'nthreads', 'cc', 'rate', 'scale', 'mix', 'workload', 'alpha', 'phasing', 'async', 'cc_ph', 'timeout_scaling', 'throughput', 'op_timeouts', 'avg_latency_ms', 'txn_failed', 'total_time')), file = 'data/retwis-tput-vs-lat.csv')
    d
  }, error = function(e) {
    write("\n!! Database unreachable. Reading stashed results from CSV.\n", stderr())
    print("!!")
    d <- read.csv(file = 'data/retwis-tput-vs-lat.csv')
  }
)

# d <- subset(d, grepl('v0.27.1', name))

d$x <- d$nthreads * num(d$nclients)
d$label <- d$nthreads * num(d$nclients) + "x" + d$rate
d$facet <- with(d, workload)

plot <- function(suffix, d, layers) {

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
    my_theme()+layers #+theme(legend.position='bottom')
  , 'retwis-tput-vs-lat'+suffix, w=5, h=3)

  save(
    ggplot(subset(d), aes(
      x = x,
      y = throughput,
      group = x(cc,phasing),
      fill = cc_ph, color = cc_ph, linetype = cc_ph
    ))+
    # geom_point()+
    stat_summary(geom='line', fun.y=mean)+
    expand_limits(y=0)+
    scale_y_continuous(labels=function(x){ x/1000+'k' })+
    facet_wrap(~facet)+ #, scales="free")+
    # cc_scales()+phasing.linetype()+
    cc_ph_scales()+
    my_theme() #+theme(legend.position='bottom')
  , 'retwis-explore'+suffix, w=8, h=6)

}

# plot('', subset(d, async == 1 & txn_failed < 100), list(coord_cartesian(ylim=c(0,15))))
plot('-no-async', subset(d, async == 0 & txn_failed < 20 & total_time > 60 & total_time < 65), list(coord_cartesian(ylim=c(0,30))))
