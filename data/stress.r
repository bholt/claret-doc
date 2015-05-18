#!/usr/bin/env Rscript
source('common.r')

# my_smooth <- function() stat_smooth(method=loess) #, span=0.3)
# cc_line <- function() list(
#   stat_summary(fun.y=mean, geom='line'),
#   cc_scales()
# )

d <- data.stress(where="nclients = 4 and nshards = 4 and nthreads = 32 and duration = 60 and name like '%v0.17.1%' and approx = 0 and rate < 1000 and alpha != '-1'")
d$facet <- with(d, sprintf("%s\n%s\nlen:%s, nkeys:%d", machines, zmix, length, nkeys))

save(
  ggplot(d, aes(
    x = throughput,
    y = avg_latency_ms,
    group = cc,
    fill = cc,
    color = cc,
    label = rate,
  ))+
  theme_mine+
  # geom_point()+
  geom_text(size=1.4)+
  # geom_mean_path(d.s, throughput, avg_latency_ms, .(facet))+
  geom_path(data=mean_path(d, throughput, avg_latency_ms, .(rate,cc,facet)), aes(x=x,y=y))+
  # cc_line()+
  xlab('Throughput')+ylab('Avg latency (ms)')+
  facet_wrap(~facet)+
  theme(legend.position="top")+
  expand_limits(y=0)
, name='plot/stress_tput_v_lat', w=8, h=8)

save(
  ggplot(d, aes(
    x = rate * num(nthreads) * num(nclients),
    y = throughput,
    group = cc,
    fill = cc,
    color = cc,
    label = rate,
  ))+
  theme_mine+
  geom_point()+
  stat_summary(geom='line', fun.y=mean)+
  # geom_mean_path(d.s, throughput, avg_latency_ms, .(facet))+
  # geom_path(data=mean_path(d, throughput, avg_latency_ms, .(rate,cc,facet)), aes(x=x,y=y))+
  # cc_line()+
  xlab('Offered rate')+ylab('Throughput')+
  facet_wrap(~facet, ncol=4)+
  theme(legend.position="top")+
  expand_limits(y=0)
, name='plot/stress_tput', w=8, h=8)


# save(
#   ggplot(d, aes(
#     x = nclients,
#     y = throughput,
#     group = `Concurrency Control`,
#     fill = `Concurrency Control`,
#     color = `Concurrency Control`
#   ))+
#   theme_mine+
#   my_smooth()+
#   facet_wrap(~facet)+
#   expand_limits(y=0)
# , name='stress_throughput_explore', w=8, h=8)
#
# # save(
# #   ggplot(d.u, aes(
# #       x = nclients,
# #       y = avg_latency_ms,
# #       group = `Concurrency Control`,
# #       fill = `Concurrency Control`,
# #       color = `Concurrency Control`
# #   ))+
# #   my_smooth()+
# #   geom_hline(y=0)+
# #   facet_wrap(~opmix)+
# #   expand_limits(y=0)+
# #   theme_mine
# # , name='avg_latency', w=4, h=5)
#
# # save(
# #   ggplot(d.u, aes(
# #       x = nclients,
# #       y = prepare_retry_rate,
# #       group = `Concurrency Control`,
# #       fill = `Concurrency Control`,
# #       color = `Concurrency Control`
# #   ))+
# #   my_smooth()+
# #   facet_wrap(~facet)+
# #   theme_mine
# # , name='retry_rate', w=7, h=8)
#
# save(
#   ggplot(d.u, aes(
#       x = nclients,
#       y = server_cc_check_success,
#       group = `Concurrency Control`,
#       fill = `Concurrency Control`,
#       color = `Concurrency Control`
#   ))+
#   my_smooth()+
#   ylab('success rate')+
#   facet_wrap(~facet)+
#   theme_mine
# , name='stress_check_rate', w=7, h=12)
#
# save(
#   ggplot(d.u, aes(
#       x = nclients,
#       y = failure_rate,
#       group = cc,
#       fill = cc,
#       color = cc
#   ))+
#   my_smooth()+
#   facet_wrap(~facet)+
#   theme_mine
# , name='stress_failure_rates', w=7, h=5)
#
# # save(
# #   ggplot(d.u, aes(
# #       x = nclients,
# #       y = op_retry_ratio,
# #       group = `Concurrency Control`,
# #       fill = `Concurrency Control`,
# #       color = `Concurrency Control`
# #   ))+
# #   my_smooth()+
# #   geom_hline(y=0)+
# #   facet_wrap(~facet)+
# #   theme_mine
# # , name='op_retries', w=8, h=6)
# #
#
# d$total <- d$server_ops_read + d$server_ops_write
# d$read <- d$server_ops_read * num(d$nshards) # / d$total
# d$write <- d$server_ops_write * num(d$nshards) # / d$total
# d.rw <- melt(d, measure=c('read', 'write'))
# d.rw$op_type <- d.rw$variable
# save(
#   ggplot(subset(d.rw, nshards == 4 & nclients == 32), aes(
#     x = op_type,
#     y = value,
#     group = op_type,
#     fill = op_type,
#     label = value,
#   ))+
#   geom_meanbar()+
#   stat_summary(aes(label=round(..y..,2)), fun.y=mean, geom="text", size=2,
#                vjust = -0.5)+
#   # facet_wrap(~facet)+
#   facet_grid(cc~length~nkeys~zmix)+
#   theme_mine
# , name='op_rw', w=12, h=6)
#
# print("success!")