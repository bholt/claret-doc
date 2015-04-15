#!/usr/bin/env Rscript
source('common.r')

data.retwis.prob <- function() {
  d <- data.retwis(where="nshards = 4 and nclients = 4 and rate != 0 and loaddir like '%12%' and rate <= 1000 and server_conflict like '%_any%'")
  d <- adply(d, 1, function(r) data.frame(fromJSON(r$server_conflict)))
  d$ccv <- revalue(d$cc, c('commutative'='comm', 'reader/writer'='r/w'))
  d$wkld <- revalue(d$workload, c('mixed'='mixed','read-heavy'='read','repost-heavy'='repost'))
  return(d)
}

data.ldbc.prob <- function() {
  subset(
    adply(
      data.ldbc(where="server_conflict is not null"), 1, 
      function(r) data.frame(fromJSON(r$server_conflict))),
    name == 'Query2'
  )
}

stat.tag <- function(d, tag){
  l <- d[grepl(sprintf('%s_|^cca|^rate$',tag), colnames(d))]
  names(l) <- gsub('potential_','pot_', gsub(sprintf('%s_',tag), '', colnames(l)))
  l
}
