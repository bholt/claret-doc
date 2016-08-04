#!/usr/bin/env Rscript
library('sitools')
source('common.r')
a <- parse.args()

d.zipf <- tryCatch({
  d <- data.rawmix(where="name like 'v0.28.1%' and nclients = 4 and duration = 30 and length = 4 and rate = 100 and timeout_scaling = 1000 and phase_limit = 100")

  d$facet <-  num(d$commute_ratio)*100 + "% update\nzipf: " + d$alpha
  d$zipf <- num(d$alpha)
  
  d.zipf <- subset(d, nkeys == 1000 & commute_ratio == 0.5)
  
  write.csv(subset(d.zipf, select = c('name', 'nclients', 'nthreads', 'cc', 'phasing', 'cc_ph', 'rate', 'facet',  'zipf', 'commute_ratio', 'timeout_scaling', 'throughput', 'op_timeouts', 'txn_failed')), file = 'data/rawmix-zipf.csv')
  d.zipf
}, error = function(e) {
  error.database_unreachable(e)
  d.zipf <- db.csv(file = 'data/rawmix-zipf.csv')
})

d.mix <- tryCatch(
{
  d <- data.rawmix(where="name like 'v0.28.1%' and nclients = 4 and duration = 30 and length = 4 and rate = 100")

  d$facet <-  num(d$commute_ratio)*100 + "% update\nzipf: " + d$alpha
  d$zipf <- num(d$alpha)
  
  d.mix <- subset(d, nkeys == 1000 & zipf == 0.6) # & txn_failed < 1000

  write.csv(subset(d.mix, select = c('facet', 'rate', 'nthreads', 'cc_ph', 'cc', 'zipf', 'commute_ratio', 'phasing', 'timeout_scaling', 'throughput', 'op_timeouts')), file = 'data/rawmix-mix.csv')
  d.mix
}, error = function(e){
  error.database_unreachable(e)
  d.mix <- db.csv(file = 'data/rawmix-mix.csv')
})


d.zipf.mean <- ddply(d.zipf, .(facet,rate,nthreads,cc_ph,zipf,phasing,cc,timeout_scaling), summarize, throughput=mean(throughput), op_timeouts=mean(op_timeouts))

d.mix.mean <- ddply(d.mix, .(facet,rate,nthreads,cc_ph,zipf,commute_ratio,phasing,cc,timeout_scaling), summarize, throughput=mean(throughput), op_timeouts=mean(op_timeouts))

subset.modes <- function(d) {
  d <- subset(d, cc_ph %in% c(RW+BASE, RW+PH, COMM, COMB, BETT+COMM+PH, BETT+COMB+PH, NOTXN))
  d[d$cc_ph == BETT+COMM+PH,]$cc_ph <- COMM+PH
  d[d$cc_ph == BETT+COMB+PH,]$cc_ph <- COMB+PH
  d
}

save(
  ggplot(subset.modes(d.zipf.mean), aes(
    x = zipf,
    y = throughput,
    group = cc_ph, fill = cc_ph, color = cc_ph, linetype = cc_ph
  ))+
  xlab('Zipf parameter')+ylab('Peak throughput (txn/s)')+
  stat_summary(geom='line', fun.y=max, size=0.7)+
  #stat_summary(geom='point', fun.y=max)+
  # geom_text(size=1.2)+
  expand_limits(y=0)+
  scale_x_continuous(breaks=c(0.2,0.4,0.6,0.8,1.0,1.2,1.4))+
  # scale_y_continuous(labels=si.labels())+
  scale_y_continuous(labels=function(x){ x/1000+'k' })+
  # facet_wrap(~facet, ncol=6)+ #, scales="free")+
  # cc_scales(title='Mode:', guide = guide_legend(nrow = 3))+
  # color_scales('', my_palette)+
  # phasing.linetype(title='Phasing:', guide = guide_legend(nrow = 2))+
  cc_ph_scales()+
  theme.mine()+theme(
    legend.key.height = unit(20,'pt'),
    legend.key.width = unit(20,'pt')
  )
, w=6.5, h=3)

save(
  ggplot(subset.modes(subset(d.mix.mean, commute_ratio != 0.5)), aes(
    x = num(commute_ratio)*100,
    y = throughput,
    group = cc_ph, fill = cc_ph, color = cc_ph, linetype = cc_ph
  ))+
  xlab('Operation mix (% update)')+ylab('Peak throughput (txn/s)')+
  stat_summary(geom='line', fun.y=max, size=0.7)+
  #stat_summary(geom='point', fun.y=max)+
  # stat_summary(geom='text', fun.y=max, size=1.2)+
  expand_limits(y=0)+
  scale_x_continuous(breaks=c(0,20,40,60,80,100))+
  scale_y_continuous(labels=function(x){ x/1000+'k' })+
  # color_scales('', my_palette)+
  # cc_scales(title='Mode:', guide = guide_legend(nrow = 3))+
  # phasing.linetype(guide = guide_legend(nrow = 2))+
  cc_ph_scales()+
  theme.mine()+theme(
    legend.key.height = unit(20,'pt'),
    legend.key.width = unit(20,'pt')
  )
  
, 'rawmix-mix', w=6.5, h=3)
