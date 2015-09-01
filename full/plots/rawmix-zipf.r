#!/usr/bin/env Rscript
library('sitools')
source('common.r')
a <- parse.args()

d.zipf <- tryCatch({
  d <- data.rawmix(where="name like 'v0.27%' and nclients = 4 and duration = 30 and length = 4")

  d$facet <-  num(d$commute_ratio)*100 + "% update\nzipf: " + d$alpha
  d$zipf <- num(d$alpha)
  d$label <- d$nthreads + "@" + d$rate + " <" + d$op_timeouts + ">"  
  
  d.zipf <- subset(d, nkeys == 1000 & commute_ratio == 0.5)
  
  write.csv(subset(d.zipf, select = c('name', 'nclients', 'nthreads', 'cc', 'phasing', 'rate', 'facet',  'zipf', 'commute_ratio', 'timeout_scaling', 'throughput', 'op_timeouts')), file = 'rawmix-zipf.csv')
  d.zipf
}, error = function(e) {
  d.zipf <- read.csv(file = 'rawmix-zipf.csv')
})

d.mix <- tryCatch(
{
  d <- data.rawmix(where="name like 'v0.27%' and nclients = 4 and duration = 30 and length = 4")

  d$facet <-  num(d$commute_ratio)*100 + "% update\nzipf: " + d$alpha
  d$zipf <- num(d$alpha)
  d$label <- d$nthreads + "@" + d$rate + " <" + d$op_timeouts + ">"  
  
  d.mix <- subset(d, nkeys == 1000 & zipf == 0.6) # & txn_failed < 1000

  write.csv(subset(d.mix, select = c('facet', 'rate', 'nthreads', 'cc_ph', 'cc', 'zipf', 'label', 'commute_ratio', 'phasing', 'timeout_scaling', 'throughput', 'op_timeouts')), file = 'rawmix-mix.csv')
  d.mix
}, error = function(e){
  d.mix <- read.csv(file = 'rawmix-mix.csv')
})


d.zipf.mean <- ddply(d.zipf, .(facet,rate,nthreads,cc_ph,zipf,label,phasing,cc,timeout_scaling), summarize, throughput=mean(throughput), op_timeouts=mean(op_timeouts))

d.mix.mean <- ddply(d.mix, .(facet,rate,nthreads,cc_ph,zipf,label,commute_ratio,phasing,cc,timeout_scaling), summarize, throughput=mean(throughput), op_timeouts=mean(op_timeouts))

save(
  ggplot(d.zipf.mean, aes(
    x = zipf,
    y = throughput,
    group = x(cc,phasing),
    fill = cc, color = cc,
    linetype = phasing,
    label = label
  ))+
  xlab('Zipfian parameter')+ylab('Peak throughput (txn/s)')+
  stat_summary(geom='line', fun.y=max)+
  stat_summary(geom='point', fun.y=max)+
  # geom_text(size=1.2)+
  expand_limits(y=0)+
  scale_x_continuous(breaks=c(0.2,0.4,0.6,0.8,1.0,1.2))+
  scale_y_continuous(labels=si.labels())+
  # facet_wrap(~facet, ncol=6)+ #, scales="free")+
  cc_scales(title='Mode:', guide = guide_legend(nrow = 3))+
  # color_scales('', my_palette)+
  phasing.linetype(title='Phasing:', guide = guide_legend(nrow = 2))+
  my_theme() #+legend.bottom()
, w=5, h=3)

save(
  ggplot(subset(d.mix.mean, commute_ratio != 0.5), aes(
    x = num(commute_ratio),
    y = throughput,
    group = x(cc,phasing),
    fill = cc, color = cc,
    linetype = phasing,
    label = label
  ))+
  xlab('Operation mix (% update)')+ylab('Peak throughput (txn/s)')+
  stat_summary(geom='line', fun.y=max)+
  stat_summary(geom='point', fun.y=max)+
  # stat_summary(geom='text', fun.y=max, size=1.2)+
  expand_limits(y=0)+
  scale_x_continuous(breaks=c(0.0,0.2,0.4,0.6,0.8,1.0))+
  scale_y_continuous(labels=si.labels())+
  # color_scales('', my_palette)+
  cc_scales(title='Mode:', guide = guide_legend(nrow = 3))+
  phasing.linetype(guide = guide_legend(nrow = 2))+
  my_theme() #+legend.bottom()
, 'rawmix-mix', w=5, h=3)
