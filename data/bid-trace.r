#!/usr/bin/env Rscript
source('common.r')

d <- db.csv('bid_trace.csv')

c <- sql("select item, count(*) as count from d group by item")

j <- sql("select * from d join c on c.item = d.item where count > 10")

print(nrow(j))

save(
  ggplot(d, aes(
    x = time,
    y = item,
    color = action,
    fill = action
  ))+list(
    geom_point(size=0.2)
  )
, w = 20, h = 20)

