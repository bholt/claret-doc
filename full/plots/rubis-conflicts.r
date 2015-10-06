#!/usr/bin/env Rscript
source('common.r')

by.conflict.mean <- data.or.csv(
  csv = 'data/rubis-conflicts.csv',
  gen = function(){
    d <- data.rubis(where="duration = 60 and name like 'v0.28.1%' and nthreads <= 96 and server_txn_conflicts != 'NA'")

    # convert tag # to names
    parse_txn_conflicts <- function(r) {
      tags <- c(
        '0'='?',
        '1'='AddUser',
        '2'='ViewUser',
        '3'='Open',
        '4'='Close',
        '5'='Bid',
        '6'='Browse',
        '7'='View',
        '8'='Comment'
      )
      s <- r$server_txn_conflicts
      for (k in names(tags)) {
        s <- gsub('\\('+k, tags[[k]], s)
        s <- gsub(','+k, ','+tags[[k]], s)
      }
      s <- gsub('\\)','',s)
      fromJSON(jsfix(s))
    }

    by.conflict <- adply(d, 1, function(r){
        j <- parse_txn_conflicts(r)    
        z <- function(n) { v <- j[[n]]; if (is.null(v)) 0 else v }
        l <- list(
            'Open-Open'         = z('Open'), z('Open,Open'),
            'Open-Close'        = z('Open,Close') + z('Close,Open'),
            'Bid-Bid'           = z('Bid') + z('Bid,Bid'),
            # 'Bid-Close'         = z('Bid,Close') + z('Close,Bid'),
            'Bid-View'          = z('Bid,View') + z('View,Bid'),
            'Browse-Open/Close' = z('Browse,Close') + z('Close,Browse') +
                                  z('Browse,Open') + z('Open,Browse')
        )    
        f <- data.frame(
            tag = names(l),
            count = vals(l)
        )
    
        f
    })

    by.conflict <- subset(by.conflict, tag != '')

    by.conflict$tag <- factor(by.conflict$tag, levels = c(
      'Open-Open',
      'Open-Close',
      # 'Bid-Close',
      'Browse-Open/Close',
      'Bid-View',
      'Bid-Bid'
    ))

    by.conflict.mean <- ddply(by.conflict, .(cc_ph, tag), summarize, count = mean(count))
    
    by.conflict.mean
  })

rw.count <- subset(by.conflict.mean, tag == 'Bid-Bid' & cc_ph == RW+BASE)$count

tag.palette <- c(
  'Open-Open'=c.red,
  'Open-Close'=c.yellow,
  # 'Bid-Close',
  'Browse-Open/Close'=c.green,
  'Bid-View'=c.pink,
  'Bid-Bid'=c.blue
)

save(
  ggplot(by.conflict.mean, aes(
      x = cc_ph,
      y = count,
      group = x(cc_ph,tag),
      fill = tag
  ))+
  geom_bar(stat='identity', position='dodge', width = 0.8)+
  annotate(geom='text', x = 5, y = 57500, label = floor(rw.count/1000)+'k', size=8)+
  geom_segment(aes(x = 5, y = 60000, xend = 5.5, yend = 70000), arrow = arrow(length = unit(0.5, "cm")))+
  # coord_cartesian()+
  ylab('conflicts')+
  coord_flip(ylim = c(0, 70000))+
  scale_y_continuous(labels = k.labels)+
  scale_fill_manual(values=tag.palette)+
  scale_color_manual(values=tag.palette)+
  my_theme()+theme(
    axis.title.y = element_blank(),
    legend.position = c(0.86, 0.18),
    legend.key.size = unit(60,'pt'),
    legend.title = element_blank(),
    text = element_text(size=26, family="Helvetica")
  )
, w = 15, h = 7)
