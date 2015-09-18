#!/usr/bin/env Rscript
source('common.r')

d <- tryCatch({
  d <- data.retwis(where="name like '%28.1%' and nclients = 2 and nthreads = 64 and server_followers_hist is not null and server_reposts_hist is not null order by id desc limit 1")
  d$server_followers_hist <- jsfix(d$server_followers_hist)
  d$server_reposts_hist <- jsfix(d$server_reposts_hist)
  
  write.csv(subset(d, select = c('id', 'name', 'nclients', 'nthreads', 'cc', 'phasing', 'cc_ph', 'rate', 'mix', 'timeout_scaling', 'max_timeout_ms', 'phase_limit', 'max_retries', 'max_op_retries', 'prepare_retries', 'throughput', 'avg_latency_ms', 'txn_count', 'txn_failed', 'server_followers_hist', 'server_reposts_hist')), file = 'data/retwis-dist.csv')
  d
}, error = function(e) {
  write("!! Database unreachable. Reading stashed results from CSV.\n", stderr())
  d <- read.table(file = 'data/retwis-dist.csv', sep = ",", quote = "\"\"", header = T)
  d$server_followers_hist <- as.character(d$server_followers_hist)
  d$server_reposts_hist <- as.character(d$server_reposts_hist)
  d
})

r <- d[1,]

save(
  # ggplot(subset(nbid.hist, x != 0), aes(x=x, weight=y))+
  ggplot(
    subset(to.hist(r$server_followers_hist), x > 0),
    aes(x = x, y = y)
  )+
    geom_point(color = c.blue, size = 1.3)+
    xlab('followers / user (log scale)')+ylab('count (log scale)')+
    scale_x_log10(labels=si.labels())+ #, breaks=c(1,10,100,1000,10000))+
    scale_y_log10(labels=si.labels())+ #, breaks=c(1,10,100,1000,10000,100000))+
    my_theme()
, 'retwis-followers', w=3.4, h=3.0)

save(
  # ggplot(subset(nbid.hist, x != 0), aes(x=x, weight=y))+
  ggplot(
    subset(to.hist(r$server_followers_hist), x > 0),
    aes(x = x, weight = y)
  )+
    stat_ecdf(color=c.blue)+
    xlab('followers / user (log scale)')+ylab('count (log scale)')+
    scale_x_log10(labels=si.labels(), breaks=c(10,50,100,1000))+
    scale_y_log10(breaks=c(0.10, 0.5, 1))+
    my_theme()
, 'retwis-followers-cdf', w=3.4, h=3.0)

save(
  # ggplot(subset(nbid.hist, x != 0), aes(x=x, weight=y))+
  ggplot(
    subset(to.hist(r$server_reposts_hist), x > 0),
    aes(x = x, y = y)
  )+
    geom_point(color = c.blue, size = 1.3)+
    xlab('# reposts (log scale)')+ylab('count (log scale)')+
    scale_x_log10(labels=si.labels(), breaks=c(1,10,100,1000,10000))+
    scale_y_log10(labels=si.labels(), breaks=c(1,10,100,1000,10000))+
    my_theme()
, 'retwis-reposts', w=3.4, h=3.0)
