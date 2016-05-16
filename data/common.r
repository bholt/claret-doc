suppressPackageStartupMessages(require(RMySQL))
suppressPackageStartupMessages(require(sqldf))
suppressPackageStartupMessages(require(ggplot2))
suppressPackageStartupMessages(require(reshape2))
options(RMySQL.dbname="claret") # (rest comes from $HOME/.my.cnf)

suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(require(grid))
suppressPackageStartupMessages(require(plyr))
suppressPackageStartupMessages(require(yaml))
suppressPackageStartupMessages(require(extrafont))
suppressPackageStartupMessages(require(tcltk))

library('Unicode')
# library('Cairo')
library('sitools')

si.labels <- function(...) { function(x) gsub(" ", "", f2si(x,...)) }
k.labels <- function(x) { x/1000 + 'k' }

COMB <- "boosting\n + combining"
COMM <- "boosting"
RW   <- "r/w locks"
PH   <- "\n + phasing"
ALL  <- "all"
COMM_PH <- "boosting\n+\nphasing"
BASE <- "\n(baseline)"
NOTXN <- "without\ntransactions"
BETT <- "better "
REDIS <- "redis"

parse.args <- function() {
  options <- commandArgs(trailingOnly=TRUE)
  list(fname=options[1])
}


cat.yaml <- function(o) cat(as.yaml(o))

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

db.csv <- function(file) {
  d <- read.csv(file = file)
  d$cc_ph <- factor(d$cc_ph, levels=c(COMB+PH, COMM+PH, RW+PH, COMB, COMM, RW+BASE, NOTXN))
  d
}

data.or.csv <- function(csv, gen) {
  d <- tryCatch(
    {
      print(Sys.getenv('R_LOCAL_CSV_ONLY'))
      if (Sys.getenv('R_LOCAL_CSV_ONLY') == '1') {
        print("LOCAL CSV")
        stop("local mode")
      }
      d <- gen()
      write.csv(d, file = csv)
      d
    }, error = function(e) {
      error.database_unreachable(e)
      d <- db.cstv(file = csv)
    }
  )
}

db <- function(query, factors=c(), numeric=c()) {
  d <- sqldf(query)
  d[factors] <- lapply(d[factors], factor)
  d[numeric] <- lapply(d[numeric], as.numeric)
  
  # fields intended to be totals rather than averages should be mult. by nclients
  fields <- names(d)[grepl(".*(_count|_retries|_failed)$|server_ops_.*$", names(d))]
  for (f in fields) d[[f]] <- num(d$nclients) * d[[f]]
  
  if ('phasing' %in% colnames(d)) {
    d$phasing <- factor(revalue(factor(d$phasing), c('0'='off','1'='on')))
  }
  
  if ( 'combining' %in% colnames(d) ) {
    
    d$combining[is.na(d$combining)] <- 0
    
    d$cc <- factor(revalue(x(d$ccmode,d$combining), c(
      'rw#0'=RW,
      'simple#0'=COMM,
      'simple#1'=COMB,
      'better#0'=BETT+COMM,
      'better#1'=BETT+COMB
    )), levels=c(RW,COMM,COMB,BETT+COMM,BETT+COMB))
    
    d$cca <- factor(revalue(x(d$ccmode,d$combining), c(
      'rw#0'='rw',
      'simple#0'='bo',
      'simple#1'='cb',
      'better#0'='b+b',
      'better#1'='b+c'
    )), levels=c('rw','bo','cb','b+b','b+c'))
    
    if ('disable_txns' %in% colnames(d)) {
      # d$disable_txns <- factor(revalue(d$disable_txns, c('NA'='0','0'='0','1'='1')))
      d$disable_txns[is.na(d$disable_txns)] <- 0
      d$ccmode[d$disable_txns == 1] <- 'rw'
      
      d$cc_ph <- factor(revalue(x(d$ccmode, d$combining, d$phasing, d$disable_txns), c(
        'rw#0#off#0'=RW+BASE,
        'simple#0#off#0'=COMM,
        'simple#1#off#0'=COMB,
        'better#0#off#0'=BETT+COMM,
        'better#1#off#0'=BETT+COMB,
        'rw#0#on#0'=RW+PH,
        'simple#0#on#0'=COMM+PH,
        'simple#1#on#0'=COMB+PH,
        'better#0#on#0'=BETT+COMM+PH,
        'better#1#on#0'=BETT+COMB+PH,
        'rw#0#off#1'=NOTXN,
        'redis#0#off#0'=REDIS
      )), levels=c(REDIS, NOTXN, BETT+COMB+PH, BETT+COMM+PH, COMB+PH, COMM+PH, RW+PH, COMB, COMM, BETT+COMB, BETT+COMM, RW+BASE))
      
    } else if ('phasing' %in% colnames(d)) {
      d$cc_ph <- factor(revalue(x(d$ccmode,d$combining,d$phasing), c(
        'rw#0#off'=RW+BASE,
        'simple#0#off'=COMM,
        'simple#1#off'=COMB,
        'rw#0#on'=RW+PH,
        'simple#0#on'=COMM+PH,
        'simple#1#on'=COMB+PH,
        'better#0#on'=BETT+COMM+PH,
        'better#1#on'=BETT+COMB+PH
      )), levels=c(BETT+COMB+PH, BETT+COMM+PH, COMB+PH, COMM+PH, RW+PH, COMB, COMM, RW+BASE))
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

db.socc <- function(query, factors=c(), numeric=c()) {
  d <- sqldf(query)
  d[factors] <- lapply(d[factors], factor)
  d[numeric] <- lapply(d[numeric], as.numeric)
  
  # fields intended to be totals rather than averages should be mult. by nclients
  fields <- names(d)[grepl(".*(_count|_retries|_failed)$|server_ops_.*$", names(d))]
  for (f in fields) d[[f]] <- num(d$nclients) * d[[f]]
  
  d$phasing <- factor(revalue(factor(d$phasing), c('0'='off','1'='on')))
  
  d$combining[is.na(d$combining)] <- 0
    
  d$cc <- factor(revalue(x(d$ccmode,d$combining), c(
    'rw#0'=RW,
    'simple#0'=COMM,
    'simple#1'=COMB,
    'better#0'=BETT+COMM,
    'better#1'=BETT+COMB
  )), levels=c(RW,COMM,COMB,BETT+COMM,BETT+COMB))
    
  d$cca <- factor(revalue(x(d$ccmode,d$combining), c(
    'rw#0'='rw',
    'simple#0'='bo',
    'simple#1'='cb',
    'better#0'='b+b',
    'better#1'='b+c'
  )), levels=c('rw','bo','cb','b+b','b+c'))
    
  d$disable_txns[is.na(d$disable_txns)] <- 0
  d$ccmode[d$disable_txns == 1] <- 'rw'
  
  d$cc_ph <- factor(revalue(x(d$ccmode, d$combining, d$phasing, d$disable_txns), c(
    'rw#0#off#0'=RW+BASE,
    'simple#0#off#0'=COMM,
    'simple#1#off#0'=COMB,
    'better#0#off#0'=BETT+COMM,
    'better#1#off#0'=BETT+COMB,
    'rw#0#on#0'=RW+PH,
    'simple#0#on#0'=COMM+PH,
    'simple#1#on#0'=COMB+PH,
    'better#0#on#0'=BETT+COMM+PH,
    'better#1#on#0'=BETT+COMB+PH,
    'rw#0#off#1'=NOTXN,
    'redis#0#off#0'=REDIS,
    'redis#0#NA#0'=REDIS
  )), levels=c(REDIS, NOTXN, BETT+COMB+PH, BETT+COMM+PH, COMB+PH, COMM+PH, RW+PH, COMB, COMM, BETT+COMB, BETT+COMM, RW+BASE))
    
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

# Melt fields matching a given regex; parse out identifiers from the regex group, 
# into a new field given by `label`. Values available in the `value` field.
#
# Example:
#   ggplot(melt.by(d, 'op', 'mean_latency_(contains|size)'), 
#          aes(x=x, y=value, color=op))...
melt.by <- function(data, label, regex) {
    fields <- names(data)[grep(regex, names(data))]
    m <- melt(data, measure=fields)
    m[label] <- capply(m$variable, function(s) gsub(regex, '\\1', s))
    m
}

# Remap a factor based on an R `list` (map) and order the levels according 
# to the order of the list.
#
# Example:
#  d.workload <- factor.remap(d.mixes, c('post_heavy'='10% post'))
factor.remap <- function(col, map) factor(revalue(col, map), levels=unique(vals(map)))

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
my_palette[[RW+BASE]]   <- c.gray
my_palette[[COMM]] <- c.blue
my_palette[[COMB]] <- c.green
my_palette[[PH]]   <- c.pink
my_palette[[ALL]]  <- c.yellow
my_palette[[NOTXN]] <- c.pink
my_palette[[REDIS]] <- c.red

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

subset.modes <- function(d) subset(d, cc_ph %in% c(RW+BASE, RW+PH, COMM+PH, COMB+PH, NOTXN))

cc_ph_scales <- function(name = 'Mode', guide = guide_legend(nrow = 7), ...) {
  colors <- c()
  colors[[RW+BASE]] <- c.gray
  colors[[RW+PH]] <- c.gray
  colors[[COMM]] <- c.blue
  colors[[COMM+PH]] <- c.blue
  colors[[BETT+COMM]] <- c.red
  colors[[BETT+COMM+PH]] <- c.red
  colors[[COMB]] <- c.green
  colors[[COMB+PH]] <- c.green
  colors[[BETT+COMB]] <- c.yellow
  colors[[BETT+COMB+PH]] <- c.yellow
  colors[[NOTXN]] <- c.pink
  colors[[REDIS]] <- c.red
  
  dashed <- 2
  dotted <- 3
  lines <- c()
  lines[[RW+BASE]] <- 1
  lines[[RW+PH]] <- dashed
  lines[[COMM]] <- 1
  lines[[COMM+PH]] <- dashed
  lines[[COMB]] <- 1
  lines[[COMB+PH]] <- dashed
  lines[[BETT+COMM]] <- 1
  lines[[BETT+COMM+PH]] <- dashed
  lines[[BETT+COMB]] <- 1
  lines[[BETT+COMB+PH]] <- dashed
  lines[[NOTXN]] <- dotted
  lines[[REDIS]] <- 1
  

  shapes <- c()
  shapes[[RW+BASE]] <- 1
  shapes[[RW+PH]] <- 1
  shapes[[COMM]] <- 2
  shapes[[COMM+PH]] <- 2
  shapes[[COMB]] <- 3
  shapes[[COMB+PH]] <- 3
  shapes[[BETT+COMM]] <- 2
  shapes[[BETT+COMM+PH]] <- 2
  shapes[[BETT+COMB]] <- 3
  shapes[[BETT+COMB+PH]] <- 3
  shapes[[NOTXN]] <- 4
  shapes[[REDIS]] <- 4
  
  list(
    scale_fill_manual(values=colors, name = name, guide = guide, ...),
    scale_color_manual(values=colors, name = name, guide = guide, ...),
    scale_linetype_manual(values=lines, name = name, guide = guide, ...),
    scale_shape_manual(values=shapes, name = name, guide = guide, ...)
  )
}


my_theme <- function() theme(
  panel.background = element_rect(fill="white"),
  panel.border = element_rect(fill=NA, color="grey50"),
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.grid.major.y = element_line(color="grey80", size=0.2),
  panel.grid.minor.y = element_blank(), #element_line(color="grey90", size=0.2),
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
  legend.key.width = unit(26,'pt'),
  legend.title.align = 0.5,
  legend.margin = unit(0,'pt'),
  legend.title = element_text(size=10)
)

font_helvetica <- function() theme(
  text = element_text(size=12, family="Helvetica"),
  strip.text.x = element_text(color="black", face="bold"),
  strip.text.y = element_text(color="black", face="bold")
)

theme.mine <- function() list(
  theme_light(),
  font_helvetica(),
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.background = element_blank()
  )
)

theme_mine <- list(
  scale_fill_manual(values=my_palette),
  # To use for line and point colors, add
  scale_color_manual(values=my_palette),
  # To use for fills, add
  # basic black and white theme
  theme.mine()
)


data.retwis <- function(select="*", where="client = 'dsretwis'") {
  d <- 
    if(exists("DATA.MODE") && DATA.MODE == 'local') {
      d.tmp <- do.call("rbind", fromJSON("freeze/retwis.json"))
      sql(sprintf("select * from `d.tmp` where total_time is not null and %s",where))
    } else {
      db(sprintf("select * from retwis where total_time is not null and %s", where),
        factors=c('nshards', 'nclients'),
        numeric=c('total_time', 'txn_count', 'nthreads')
      )
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
  
  
  lvl <- c(
    'read-heavy',
    'post-heavy'
  )
  
  d$workload <- factor(revalue(d$mix, c(
    'geom_repost'=lvl[2],
    # 'geom_heavy'=lvl[2],
    # 'read_heavy'=lvl[1],
    '2read2heavy'=lvl[1],
    'update_heavy'=lvl[3]
  )), levels=lvl)
  
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

retwis.mixes <- c(
  '2read2heavy'='read-heavy (98% read)',
  'geom_repost'='post-heavy (90% read)'
)

retwis.txns <- c('newuser', 'follow', 'post', 'repost', 'timeline')

data.retwis.socc <- function(select="*", where="duration = 60") {
  d <- db.socc(sprintf("select * from socc_retwis where total_time is not null and %s", where),
        factors=c('shards', 'nclients'),
        numeric=c('total_time', 'txn_count', 'threads'))

  # d$scale <- gsub('.*/(\\d+)', '\\1', d$loaddir)
  
  d$throughput <- d$txn_count / d$total_time
  # d$throughput <- d$retwis_txn_count / d$total_time;
  d$retwis_txn_count <- d$retwis
  
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
  
  d$retwis_total <- 
    d$retwis_newuser_count + 
    d$retwis_follow_count + 
    d$retwis_post_count + 
    d$retwis_repost_count + 
    d$retwis_timeline_count
  
  d$retwis_newuser_fraction <- d$retwis_newuser_count / d$retwis_total
  d$retwis_follow_fraction <- d$retwis_follow_count / d$retwis_total
  d$retwis_post_fraction <- d$retwis_post_count / d$retwis_total
  d$retwis_repost_fraction <- d$retwis_repost_count / d$retwis_total
  d$retwis_timeline_fraction <- d$retwis_timeline_count / d$retwis_total
  
  # d$graph <- mapply(function(g,d){ if(g == 'none') gsub('^.*/kronecker/([0-9]+)','kronecker:\\1',d) else g }, d$gen, d$loaddir)
    
  d$workload <- factor.remap(d$mix, retwis.mixes)
  
  d$zmix <- sprintf('%s/%s', d$mix, d$alpha)

  # d$facet <- sprintf('%s\n%s', d$zmix, d$graph)
  
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


error.database_unreachable <- function(e) {
  write("\n!! Database unreachable. Reading stashed results from CSV.", stderr())
  print(e)
  write("", stderr())
}
