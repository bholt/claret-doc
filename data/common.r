suppressPackageStartupMessages(require(RMySQL))
suppressPackageStartupMessages(require(sqldf))
suppressPackageStartupMessages(require(ggplot2))
suppressPackageStartupMessages(require(reshape))
options(RMySQL.dbname="claret") # (rest comes from $HOME/.my.cnf)

suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(require(grid))
suppressPackageStartupMessages(require(plyr))
suppressPackageStartupMessages(require(yaml))
suppressPackageStartupMessages(require(extrafont))

library('Unicode')
library('Cairo')
library('sitools')

si.labels <- function(...) { function(x) gsub(" ", "", f2si(x,...)) }

COMB <- "boosting\n + combining"
COMM <- "boosting"
RW   <- "r/w locks"
PH   <- "\n + phasing"
ALL  <- "all"
COMM_PH <- "boosting\n+\nphasing"

parse.args <- function() {
  options <- commandArgs(trailingOnly=TRUE)
  list(fname=options[1])
}


json.to.df <- function(jstr) {
  d <- fromJSON(jstr)
  return(data.frame(x=names(d),y=unlist(d)))
}

vgrep <- function(regex, values) grep(regex, values, value=T)

sql <- function(...) sqldf(..., drv="SQLite")

num <- function(var) as.numeric(as.character(var))

x <- function(...) paste(..., sep='#')
p <- function(...) paste(..., sep='')

"+" = function(x, y) {
  if(is.character(x) | is.character(y)) {
    return(paste(x , y, sep=""))
  } else {
    .Primitive("+")(x,y)
  }
}

mgsub <- function(myrepl, mystring){
  gsub2 <- function(l, x){
   do.call('gsub', list(x = x, pattern = l[1], replacement = l[2]))
  }
  Reduce(gsub2, myrepl, init = mystring, right = T) 
}

capply <- function(col, func) unlist(lapply(col, func))

vals <- function(lst) unlist(lst, use.names=F)

db <- function(query, factors=c(), numeric=c()) {
  d <- sqldf(query)
  d[factors] <- lapply(d[factors], factor)
  d[numeric] <- lapply(d[numeric], as.numeric)
  
  if ('phasing' %in% colnames(d)) {
    d$phasing <- factor(revalue(factor(d$phasing), c('0'='off','1'='on')))
  }
  
  if ( 'combining' %in% colnames(d) ) {
    
    d$cc <- factor(revalue(x(d$ccmode,d$combining), c(
      'rw#0'=RW,
      'simple#0'=COMM,
      'simple#1'=COMB,
      'rw#NA'=RW,
      'simple#NA'=COMM
    )), levels=c(RW,COMM,COMB))
    
    d$cca <- factor(revalue(x(d$ccmode,d$combining), c(
      'rw#0'='rw',
      'simple#0'='bo',
      'simple#1'='cb',
      'rw#NA'='rw',
      'simple#NA'='bo'
    )), levels=c('rw','bo','cb'))
    
    if ('phasing' %in% colnames(d)) {
      d$cc_ph <- factor(revalue(x(d$ccmode,d$combining,d$phasing), c(
        'rw#0#off'=RW,
        'simple#0#off'=COMM,
        'simple#1#off'=COMB,
        'rw#0#on'=RW+PH,
        'simple#0#on'=COMM+PH,
        'simple#1#on'=COMB+PH
      )), levels=c(RW,COMM,COMB,RW+PH,COMM+PH,COMB+PH))
    }
    
  } else if ( 'ccmode' %in% colnames(d) ) {

    d$cc <- factor(revalue(d$ccmode, c(
      'rw'=RW,
      'simple'=COMM
    )), levels=c(COMM,RW))  

    d$cca <- factor(revalue(d$ccmode, c(
      'rw'='rw',
      'simple'='co'
    )), levels=c('co','rw'))
    
  }
  
  return(d)
}

df.histogram <- function(json, version="none") {
  d <- fromJSON(json)
  dd <- data.frame(x=num(names(d)), y=num(unlist(d)), version=version)
  dd[order(dd$x),]
}

jsfix <- function(str) gsub("'", "\"", str)

to.hist <- function(j) {
  l1 <- as.list(fromJSON(j))
  df1 <- data.frame(x=num(names(l1)), y=vals(l1))
  df1 <- df1[order(df1$x),]
}

replace_txn_codes <- function(orig,codes) Reduce(function(o,k){ gsub("(['\"(,])"+codes[[k]]+"(?=['\",)])", "\\1"+k+"\\2", o, perl=T) }, names(codes), init=jsfix(orig))

parse_txn_conflicts <- function(str, codes) {
  c <- fromJSON(gsub("'","\"",str))
  names(c) <- replace_txn_codes(names(c), codes)
  c
}

unmarshal <- function(str) fromJSON(gsub("'","\"",str))
  
pp <- function(row) cat(as.yaml(row))  

save <- function(g, name=FILE_BASE, file=sprintf("%s/%s.pdf",FILE_DIR,name), w=3.3, h=3.1) {
  
  ggsave(plot=g, filename=file, width=w, height=h, device=cairo_pdf, family='Open Sans')
  # cairo_pdf(file='test.pdf', width=w, height=h, family='Helvetica Neue')
  # g
  # dev.off()
  print(sprintf("saved: %s", file))
}

# prettify <- function(str) gsub('_',' ',gsub('([a-z])([a-z]+)',"\\U\\1\\E\\2",str,perl=TRUE))

regex_match <- function(reg,str) length(grep(reg,str)) > 0

label_pretty <- function(variable, value) {
  vname <- if (regex_match('variable|value',variable)) '' else sprintf('%s:', variable)
  lapply(paste(vname, prettify(as.character(value))), paste, collapse="\n")
}

geom_mean <- function(geom) stat_summary(fun.y='mean', geom=geom, labeller=label_pretty)

geom_meanbar <- function(labeller=label_pretty) {
  return(list(
    stat_summary(fun.y=mean, geom='bar', position='dodge'),
    stat_summary(fun.data=mean_cl_normal, geom='errorbar', width=0.2, position='dodge')
  ))
}

mean_path <- function(d, x, y, grp) eval(parse(text=sprintf('ddply(d, grp, summarize, x=mean(%s), y=mean(%s))', as.character(substitute(x)), as.character(substitute(y)))))

geom_mean_path <- function(d, x, y, groupby) eval(parse(text=sprintf(
  'geom_path(data=mean_path(d, %s, %s, groupby), aes(x=x, y=y))',
  as.character(substitute(x)),
  as.character(substitute(y))
)))
 # geom_path(data=mean_path(d, x, y, groupby), aes(x=x,y=y))

c.blue   <- "#0072B2"
c.yellow <- "#E69F00"
c.green  <- "#009E73"
c.red    <- "#D55E00"
c.pink   <- "#CC79A7"
c.gray   <- "#999999"

my_palette <- c(
  'rw'=c.yellow,
  'simple'=c.blue,
  'comm'=c.blue,
  
  'approx'=c.green,
  'precise'=c.blue,
  
  'reader/writer'=c.yellow,
  'commutative: approx'=c.green,
  'commutative: precise'=c.blue,

  'Locking / OCC'=c.yellow,
  'Claret-Approx'=c.green,
  'Claret'=c.blue,
  
  'follow'=c.blue,
  'newuser'=c.yellow,
  'post'=c.green,
  'repost'=c.red,
  'timeline'=c.pink,
  
  'kronecker'=c.blue,
  
  'read'=c.pink,
  'write'=c.green,
  
  'none'=c.gray  
)

my_palette[[RW]]   <- c.gray
my_palette[[COMM]] <- c.blue
my_palette[[COMB]] <- c.green
my_palette[[PH]]   <- c.pink
my_palette[[ALL]]  <- c.yellow

# The palette with grey:
cbPalette <- c("#0072B2", "#E69F00", "#009E73", "#D55E00", "#CC79A7", "#56B4E9", "#F0E442", "#999999")

color_scales <- function(title, palette)
  list(
    scale_fill_manual(values=palette, name=title),
    scale_color_manual(values=palette, name=title)
  )

legend.bottom <- function() list(
  theme(
    legend.position = 'bottom',
    legend.key.size = unit(14,'pt'),
    legend.margin = unit(0,'pt'),
    legend.box = 'horizontal',
    legend.title.align = 1
  )
)

my_colors <- function(title="") list(
  scale_fill_manual(values=my_palette, name=title),
  # To use for line and point colors, add
  scale_color_manual(values=my_palette, name=title)
)

cc_scales <- function(title="Concurrency control:", ...) {
  linetype_map <- c()
  linetype_map[[COMM]] <- 1
  linetype_map[[RW]] <- 2
  list(
    scale_fill_manual(values=my_palette, name=title, ...),
    scale_color_manual(values=my_palette, name=title, ...),
    scale_linetype_manual(name=title, values=linetype_map, ...)
  )
}

phasing.linetype <- function(title="Phasing:", ...) {
  # scale_linetype_manual(name=title, values=c('yes'=2, 'no'=1)),
  scale_linetype_manual(name=title, values=c('on'=2, 'off'=1), ...)
    # labels=c('\u2713','\u2717'))
    # labels=c(as.u_char('2713'), as.u_char('2717')))
}

cc_ph_scales <- function(...) {
  colors <- c()
  colors[[RW]] <- c.gray
  colors[[RW+PH]] <- c.gray
  colors[[COMM]] <- c.blue
  colors[[COMM+PH]] <- c.blue
  colors[[COMB]] <- c.green
  colors[[COMB+PH]] <- c.green
  
  lines <- c()
  lines[[RW]] <- 1
  lines[[RW+PH]] <- 2
  lines[[COMM]] <- 1
  lines[[COMM+PH]] <- 2
  lines[[COMB]] <- 1
  lines[[COMB+PH]] <- 2
  
  g <- guide_legend(nrow = 6)
  
  list(
    scale_fill_manual(values=colors, name='Mode', guide = g, ...),
    scale_color_manual(values=colors, name='Mode', guide = g, ...),
    scale_linetype_manual(values=lines, name='Mode', guide = g, ...)
  )
}


my_theme <- function() theme(
  panel.background = element_rect(fill="white"),
  panel.border = element_rect(fill=NA, color="grey50"),
  panel.grid.major = element_line(color="grey80", size=0.2),
  panel.grid.minor = element_line(color="grey90", size=0.2),
  strip.background = element_rect(fill="grey90", color="grey50"),
  strip.background = element_rect(fill="grey80", color="grey50"),
  axis.ticks = element_line(colour="black"),
  panel.grid = element_line(colour="black"),
  axis.text.y = element_text(colour="black"),
  axis.text.x = element_text(colour="black"),
  text = element_text(size=9, family="Helvetica"),
  legend.key = element_rect(fill=NA, color=NA),
  legend.text = element_text(lineheight=0.9),
  legend.key.height = unit(24,'pt'),
  legend.title.align = 0.5,
  legend.title = element_text(size=10)
)

theme_mine <- list(
  scale_fill_manual(values=my_palette),
  # To use for line and point colors, add
  scale_color_manual(values=my_palette),
  # To use for fills, add
  # basic black and white theme
  my_theme()
)


data.retwis <- function(select="*", where="client = 'dsretwis'") {
  d <- 
    if(exists("DATA.MODE") && DATA.MODE == 'local') {
      d.tmp <- do.call("rbind", fromJSON("freeze/retwis.json"))
      sql(sprintf("select * from `d.tmp` where total_time is not null and %s",where))
    } else {
      suppressWarnings(db(sprintf("select * from retwis where total_time is not null and %s", where),
        factors=c('nshards', 'nclients'),
        numeric=c('total_time', 'txn_count', 'nthreads')
      ))
    }

  d$scale <- gsub('.*/(\\d+)', '\\1', d$loaddir)
  
  d$throughput <- d$txn_count * num(d$nclients) / d$total_time
  # d$throughput <- d$retwis_txn_count * num(d$nclients) / d$total_time;
  
  # d$throughput <- d$ntxns * num(d$nclients) / d$total_time
  d$avg_latency_ms <- d$txn_time / d$txn_count * 1000

  d$prepare_total <- d$prepare_retries + d$txn_count
  d$prepare_retry_rate <- d$prepare_retries / d$prepare_total
      
  d$variant <- factor(revalue(sprintf('%s:%s', d$ccmode, d$approx), c(
    # 'rw:1'='reader/writer',
    'rw:0'='Locking / OCC',
    'simple:0'='Claret',
    'simple:1'='Claret-Approx'
  )), levels=c('Locking / OCC', 'Claret', 'Claret-Approx'))
  
  
  d$graph <- mapply(function(g,d){ if(g == 'none') gsub('^.*/kronecker/([0-9]+)','kronecker:\\1',d) else g }, d$gen, d$loaddir)
  
  d$workload <- factor(revalue(d$mix, c(
    'geom_repost'='repost-heavy',
    'geom_heavy'='repost-heavy',
    'read_heavy'='read-heavy',
    'update_heavy'='mixed'
  )), levels=c('read-heavy','repost-heavy','mixed'))
  
  d$zmix <- sprintf('%s/%s', d$mix, d$alpha)

  d$facet <- sprintf('%s\n%s', d$zmix, d$graph)
  
  return(d)
}

data.rubis <- function(select="*", where="client = 'rubis'") {
  d <- 
    if(exists("DATA.MODE") && DATA.MODE == 'local') {
      d.tmp <- do.call("rbind", fromJSON("freeze/rubis.csv"))
      sql(sprintf("select * from `d.tmp` where total_time is not null and %s",where))
    } else {
      suppressWarnings(db(sprintf("select * from rubis where total_time is not null and %s", where),
        factors=c('nshards', 'nclients'),
        numeric=c('total_time', 'txn_count', 'nthreads')
      ))
    }

  d$state <- gsub('.*/(.*)', '\\1', d$loaddir)
    
  d$zmix <- sprintf('%s/%s', d$mix, d$alpha)
  
  # fields intended to be totals rather than averages should be mult. by nclients
  fields <- names(d)[grepl(".*(_count|_retries|_failed|_latency)$", names(d))]
  for (f in fields) d[[f]] <- num(d$nclients) * d[[f]]
  # cat("# computing total for fields: "); cat(fields); cat("\n")
  d$prepare_total <- d$prepare_retries + d$txn_count
  d$prepare_retry_rate <- d$prepare_retries / d$prepare_total
  
  # compute avg latencies for each txn type
  txns <- gsub('rubis_(.*)_count', '\\1', names(d[,grepl('rubis_(.*)_count',names(d))]))
  for (t in txns) d[["rubis_"+t+"_avg_latency_ms"]] <- d[["rubis_"+t+"_latency"]] / d[["rubis_"+t+"_count"]]
  
  d$throughput <- d$txn_count / d$total_time
  d$avg_latency_ms <- d$txn_time / d$txn_count * 1000
  
  d$rubis_txn_count <- with(d, rubis_AddUser_count + rubis_BrowseItems_count + rubis_CloseAuction_count + rubis_NewBid_count + rubis_NewComment_count + rubis_OpenAuction_count + rubis_UserSummary_count + rubis_ViewAuction_count)
  
  return(d)
}

data.papoc <- function(where) {
  d <- db(
    sprintf("select * from tapir where total_time is not null and %s", where),
    factors=c('nshards', 'nclients'),
    numeric=c('total_time', 'txn_count')
  )
  
  d$failure_rate <- d$txn_failed / (d$txn_count + d$txn_failed)
  d$throughput <- d$txn_count * num(d$nclients) / d$total_time
  # d$throughput <- d$ntxns * num(d$nclients) / d$total_time
  d$avg_latency_ms <- d$txn_time / d$txn_count * 1000
  
  d$prepare_total <- d$prepare_retries + d$txn_count
  d$prepare_retry_rate <- d$prepare_retries / d$prepare_total
    
  d$Graph <- capply(d$gen, function(s) gsub('kronecker:.+','kronecker',s))
  
  
  d$workload <- suppressWarnings(factor(revalue(d$mix, c(
    'geom_repost'='repost-heavy',
    'read_heavy'='read-heavy',
    'update_heavy'='mixed'
  )), levels=c('repost-heavy','read-heavy','mixed')))
  
  d$zmix <- sprintf('%s/%s', d$mix, d$alpha)
  
  d$facet <- sprintf('shards: %d\n%s\n%d users\n%s', num(d$nshards), d$zmix, d$initusers, d$Graph)

  d$gen_label <- sprintf('%d users\n%s', d$initusers, d$Graph)
  
  return(d)
}

data.ldbc <- function(where = "ldbc_config is not null", melt='ldbc_results') {
  d <- 
    if(exists("DATA.MODE") && DATA.MODE == 'local') {
      print(getwd())
      d_ldbc <- read.delim('freeze/ldbc.csv', sep=',')
      d_ldbc$ldbc_results <- as.character(d_ldbc$ldbc_results)
      d_ldbc$client_metrics <- as.character(d_ldbc$client_metrics)
      sql(sprintf("select * from `d_ldbc` where ldbc_results is not null and %s",where))
    } else {
      db(sprintf("select * from ldbc where ldbc_results is not null and ldbc_results != \"\" and %s", where))
    }
  
  if (melt == 'ldbc_results') {
    d <- adply(d, 1, function(r){
      o <- fromJSON(r$ldbc_results)
      m <- o$all_metrics
      mr <- m$run_time
      wavg_mean_latency <- sum(mr$mean * mr$count) / sum(mr$count) / 1e6
      colnames(mr) <- sprintf("time_%s", colnames(mr))
    
      cm <- fromJSON(r$client_metrics)
    
      data.frame(
        throughput=as.numeric(r$ntotal)/as.numeric(o$total_duration)*1e6,
        total_time=o$total_duration,
        wavg_mean_latency=wavg_mean_latency,
        name=o$all_metrics$name,
        count=o$all_metrics$count,
        mr
      )
    })
    d$per_tput <- as.numeric(d$count) / d$total_time
    d$name <- gsub('^.*(?:(Query\\d)|\\d(.*))$','\\1\\2',d$name)
    d
  } else if (melt == 'client_metrics') {
    d <- adply(d, 1, function(r){
      o <- fromJSON(r$ldbc_results)
      cm <- fromJSON(r$client_metrics)
      data.frame(
        throughput=as.numeric(r$ntotal)/as.numeric(o$total_duration)*1e6,
        total_time=o$total_duration,
        count=r$ntotal,
        cm
      )
    })
    d$per_tput <- as.numeric(d$count) / d$total_time
    d
  } else {
    d <- adply(d, 1, function(r){
      o <- fromJSON(r$ldbc_results)
      cm <- fromJSON(r$client_metrics)
      data.frame(
        throughput=as.numeric(r$ntotal)/as.numeric(o$total_duration)*1e6,
        total_time=o$total_duration,
        count=r$ntotal,
        cm
      )
    })
    d$per_tput <- as.numeric(d$count) / d$total_time
    d
  }
}

data.rawmix <- function(where="total_time is not null") {
  d <- db(sprintf("select * from rawmix where total_time is not null and %s", where),
    factors=c('nshards'),
    numeric=c('total_time', 'txn_count')
  )
  
  d$zmix <- sprintf('%s%% – α %s', num(d$commute_ratio)*100, d$alpha)
  
  # fields intended to be totals rather than averages should be mult. by nclients
  fields <- names(d)[grepl(".*(_count|_retries|_failed|_latency)$", names(d))]
  for (f in fields) d[[f]] <- num(d$nclients) * d[[f]]
  # cat("# computing total for fields: "); cat(fields); cat("\n")
  d$prepare_total <- d$prepare_retries + d$txn_count
  d$prepare_retry_rate <- d$prepare_retries / d$prepare_total
  
  # compute avg latencies for each txn type
  txns <- c('add','card')
  for (t in txns) 
    d[["raw_"+t+"_avg_latency_ms"]] <- d[["raw_"+t+"_time"]] / d[["raw_"+t+"_count"]]
  
  d$throughput <- d$txn_count / d$total_time
  d$avg_latency_ms <- d$txn_time / d$txn_count * 1000
  
  d$failure_rate <- d$txn_failed / (d$txn_count + d$txn_failed)  
  d$op_retry_ratio <- d$op_retries / d$op_count
    
  return(d)
}

data.stress <- function(where="clientmode = 'stress'") {
  d <- db(paste("select * from stress where total_time is not null and ", where),
    factors=c('nshards', 'nclients'),
    numeric=c('total_time', 'txn_count')
  )
  d$failure_rate <- d$txn_failed / (d$txn_count + d$txn_failed)
  d$throughput <- d$txn_count * num(d$nclients) / d$total_time
  d$avg_latency_ms <- d$txn_time / d$txn_count * 1000

  d$prepare_total <- d$prepare_retries + d$txn_count
  d$prepare_retry_rate <- d$prepare_retries / d$prepare_total

  d$op_retries_total <- d$op_retries * num(d$nclients)
  d$op_retry_ratio <- d$op_retries / d$op_count

  d$opmix <- factor(revalue(d$mix, c(
    'mostly_update'='35% read / 65% update',
    'update_heavy'='50% read / 50% update',
    'read_heavy'='90% read / 10% update'
  )))

  d$workload <- factor(revalue(d$mix, c(
    'mostly_update'='update-heavy',
    'update_heavy'='mixed',
    'read_heavy'='read-heavy'
  )))

  d$zmix <- sprintf('%s/%s', d$mix, d$alpha)
  d$facet <- sprintf('%s\n%d keys', d$zmix, d$nkeys)
  
  return(d)
}

