suppressPackageStartupMessages(require(RMySQL))
suppressPackageStartupMessages(require(sqldf))
suppressPackageStartupMessages(require(plotly))
suppressPackageStartupMessages(require(ggplot2))
suppressPackageStartupMessages(require(reshape2))
options(RMySQL.dbname="claret") # (rest comes from $HOME/.my.cnf)

suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(require(grid))
suppressPackageStartupMessages(require(plyr))
suppressPackageStartupMessages(require(yaml))
suppressPackageStartupMessages(library(extrafont))
suppressPackageStartupMessages(require(tcltk))

library('Unicode')
# library('Cairo')
library('sitools')

si.labels <- function(...) { function(x) gsub(" ", "", f2si(x,...)) }
k.labels <- function(x) { x/1000 + 'k' }

COMMON_DIR <- getwd()

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

db.cstv <- function(file) {
  d <- read.csv(file = file)
  d$bound <- factor(d$bound, levels=vals(bounds))
  d$condition <- factor.remap(x(d$honeycomb_mode,d$load), conds)
  d$mix <- factor(d$mix, levels=vals(mixes.counter))
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
#  
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

geom_meanbar <- function(barwidth=0.7, position='dodge') {
  return(list(
    stat_summary(fun.y=mean, geom='bar', position=position, width=barwidth),
    stat_summary(fun.data=mean_cl_normal, geom='errorbar', size=0.3, width=0.2, position=position, color='black')
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
    legend.text = element_text(size=10, margin=margin(0, unit='pt')),
    legend.margin = unit(0, 'pt'),
    legend.box = 'horizontal',
    legend.title.align = 1
  )
)

font_roboto <- function() theme(
  text = element_text(size=12, family="Roboto"),
  strip.text.x = element_text(family="Roboto", color="black", face="bold"),
  strip.text.y = element_text(family="Roboto", color="black", face="bold")
)

font_helvetica <- function() theme(
  text = element_text(size=12, family="Helvetica"),
  strip.text.x = element_text(color="black", face="bold"),
  strip.text.y = element_text(color="black", face="bold")
)

theme_mine <- function() list(
  theme_light(),
  font_helvetica(),
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.background = element_blank()
  )
)

group.legend <- function(title='Bounds') list(
    scale_fill_discrete(guide=guide_legend(title=title)),
    scale_color_discrete(guide=guide_legend(title=title))
)

# get columns of `d` by `regex`
cgrep <- function(d, regex) d[grep(regex,names(d))]


theme.bar <-function(angle=45, hjust=1, size=8) theme(
  axis.text.x = element_text(angle=angle, hjust=hjust, size=size),
  axis.title.x = element_blank(),
  legend.position="none"
)

low <- 512
high <- 4096

conds <- c()
conds[[x('fast', 4096)]] <- 'Local'
conds[[x('flat5', 128)]] <- 'Uniform\n(5ms)'
conds[[x('flat5', 4096)]] <- 'High load'
conds[[x('normal',low)]] <- 'Normal'
conds[[x('normal',high)]] <- 'Normal (high load)'
conds[[x('slowpoke_flat',low)]] <- 'Slow replica'
# conds[[x('world',low)]] <- 'Geo-distributed'
# conds[[x('google',low)]] <- 'Google (low)'
# conds[[x('google',2048)]] <- 'Google'
# conds[[x('google',high)]] <- 'Google (high)'
# conds[[x('amazon',low)]] <- 'Amazon (low)'
conds[[x('amazon',2048)]] <- 'Geo-\ndistributed'
# conds[[x('amazon',high)]] <- 'Amazon (high)'

bounds <- list(
  'consistency:strong' = 'strong',
  'consistency:strongwrite' = 'strong (write)',
  'tolerance:0' = 'error: 0%',
  'tolerance:0.01' = 'error: 1%',
  'tolerance:0.05' = 'error: 5%',
  'tolerance:0.1' = 'error: 10%',
  'latency:50ms' = 'latency: 50ms',
  'latency:20ms' = 'latency: 20ms',
  'latency:10ms' = 'latency: 10ms',
  'consistency:weak' = 'weak/strong',
  'consistency:weakwrite' = 'weak'
)

b.cst <- "consistency:strong"
b.csw <- "consistency:strongwrite"
b.cwk <- "consistency:weak"
b.l10 <- "latency:10ms"
b.l20 <- "latency:20ms"
b.l50 <- "latency:50ms"
b.t10 <- "tolerance:0.1"
b.t05 <- "tolerance:0.05"
b.t01 <- "tolerance:0.01"
b.t00 <- "tolerance:0"

mixes.counter <- list(
  'default'    = 'default',
  'no_size'    = 'default',
  'read_heavy' = 'read_heavy',
  'custom'     = 'custom'
)

ipa.scales <- function(name = 'Bounds', guide = guide_legend(nrow=8), ...) {
  c.strong <- c.yellow
  c.weak <- c.red
  c.lat <- c.blue
  c.err <- c.green

  colors <- c()
  colors[[ bounds[[b.csw]] ]] <- c.strong
  colors[[ bounds[[b.cst]] ]] <- c.strong
  colors[[ bounds[[b.cwk]] ]] <- c.weak
  colors[[ bounds[['consistency:weakwrite']] ]] <- c.weak
  colors[[ bounds[[b.l10]] ]] <- c.lat
  colors[[ bounds[[b.l20]] ]] <- c.lat
  colors[[ bounds[[b.l50]] ]] <- c.lat
  colors[[ bounds[[b.t10]] ]] <- c.err
  colors[[ bounds[[b.t05]] ]] <- c.err
  colors[[ bounds[[b.t01]] ]] <- c.err
  colors[[ bounds[[b.t00]] ]] <- c.err

  colors[[ bounds[[b.cst]]+'#0ms' ]] <- c.gray
  colors[[ bounds[[b.cst]]+'#200ms' ]] <- c.gray
  colors[[ bounds[['consistency:weakwrite']]+'#200ms' ]] <- c.weak

  colors[[ bounds[[b.t10]]+'#200ms' ]] <- c.err
  colors[[ bounds[[b.t05]]+'#200ms' ]] <- c.err
  colors[[ bounds[[b.t01]]+'#200ms' ]] <- c.err
  colors[[ bounds[[b.t00]]+'#200ms' ]] <- c.err
  colors[[ bounds[[b.t10]]+'#0ms' ]] <- c.strong
  colors[[ bounds[[b.t05]]+'#0ms' ]] <- c.strong
  colors[[ bounds[[b.t01]]+'#0ms' ]] <- c.strong
  colors[[ bounds[[b.t00]]+'#0ms' ]] <- c.strong

  
  solid  <- 1
  dashed <- 2
  dotted <- 3
  lines <- c()
  lines[[ bounds[['consistency:weakwrite']] ]] <- dotted
  lines[[ bounds[[b.csw]] ]] <- solid
  lines[[ bounds[[b.cst]] ]] <- solid
  lines[[ bounds[[b.cwk]] ]] <- solid
  lines[[ bounds[['consistency:weakwrite']] ]] <- solid
  lines[[ bounds[[b.l10]] ]] <- solid
  lines[[ bounds[[b.l20]] ]] <- solid
  lines[[ bounds[[b.l50]] ]] <- dotted
  lines[[ bounds[[b.t01]] ]] <- solid
  lines[[ bounds[[b.t05]] ]] <- dashed
  lines[[ bounds[[b.t10]] ]] <- dotted
  lines[[ bounds[[b.t00]] ]] <- solid
  
  list(
    scale_fill_manual(values=colors, name=name, guide=guide, ...),
    scale_color_manual(values=colors, name=name, guide=guide, ...),
    scale_linetype_manual(values=lines, name = name, guide = guide, ...),
    expand_limits(y = 0)
  )
}

data.ipa.common <- function(table="ipa_rawmix", where="honeycomb_mode is not null and out_actual_time_length is not null") {
  d <- db("select * from "+table+" where out_actual_time_length is not null and " + where)
  fields <- names(d)[grepl(".*(_count|_rate|_p\\d+|_max|_min|_mean|_ms|_stddev)(_\\d)?$", names(d))]
  for (f in fields) d[[f]] <- num(d[[f]])
  
  d$duration <- d$ipa_duration
  d$load <- num(d$ipa_concurrent_requests)

  d$op_rate <- d$timers_cass_op_latency_mean_rate
  d$op_lat_mean <- d$timers_cass_op_latency_mean
  d$op_lat_median <- d$timers_cass_op_latency_p50
  
  d$res_cass_op_count <- rowMeans(cgrep(d,'res_timers_cass_op_latency_count_'))
  
  d$condition <- factor.remap(x(d$honeycomb_mode,d$load), conds)
  
  d$bound <- factor.remap(d$ipa_bound, bounds)
  
  return(d)
}

data.ipa.tickets <- function(where="out_actual_time_length is not null") {
  d <- data.ipa.common(table="ipa_tickets", where=where)
  
  # compute totals for reservation counters
  counters <- c('incr', 'decr', 'init', 'read', 'cached', 'expired', 'forwards')
  
  for (f in counters) d[["res_"+f+"_total"]] <- rowSums(cgrep(d,'res_counters_'+f+'_count_'))
  
  # compute means for other reservation metrics
  timers <- c('consume', 'transfer')
  for (f in timers) d[['res_'+f+'_lat_mean']] <- rowMeans(cgrep(d,'res_timers_'+f+'_latency_mean_'))
    
  # aliases
  aliases <- c(
    lease='ipa_lease_period',
    read_strong='counters_read_strong_count',
    read_weak='counters_read_weak_count',
    purchase_rate='timers_op_purchase_mean_rate',
    purchase_lat_mean='timers_op_purchase_mean',
    purchase_lat_median='timers_op_purchase_p50',
    purchase_lat_p95='timers_op_purchase_p95',
    purchase_lat_p99='timers_op_purchase_p99',
    purchase_count='timers_op_purchase_count',
    view_rate='timers_op_view_mean_rate',
    view_lat_mean='timers_op_view_mean',
    view_lat_median='timers_op_view_p50',
    view_lat_p95='timers_op_view_p95',
    view_lat_p99='timers_op_view_p99',
    view_count='timers_op_view_count',
    create_rate='timers_op_create_mean_rate',
    create_lat_mean='timers_op_create_mean',
    create_lat_median='timers_op_create_p50',
    create_lat_p95='timers_op_create_p95',
    create_lat_p99='timers_op_create_p99',
    create_count='timers_op_create_count',
    browse_rate='timers_op_browse_mean_rate',
    browse_lat_mean='timers_op_browse_mean',
    browse_lat_median='timers_op_browse_p50',
    browse_lat_p95='timers_op_browse_p95',
    browse_lat_p99='timers_op_browse_p99',
    browse_count='timers_op_browse_count'
  )
  for (n in names(aliases)) d[[n]] <- d[[aliases[n]]]
  
  d$overall_lat_mean <- with(d,
    (purchase_lat_mean * purchase_count +
      view_lat_mean * view_count +
      create_lat_mean * create_count +
      browse_lat_mean * browse_count) /
    (purchase_count+view_count+create_count+browse_count)
  )
  
  d$overall_rate <- with(d,
    purchase_rate + view_rate + create_rate + browse_rate
  )
  
  return(d)
}

log <- function(obj) write(obj, stdout())

data.ipa.rawmix <- function(where="honeycomb_mode is not null and out_actual_time_length is not null") {
  d <- data.ipa.common(table="ipa_rawmix", where=where)
  
  d$mean_lat_add      <- d$timers_add_latency_mean
  d$mean_lat_contains <- d$timers_contains_latency_mean
  d$mean_lat_size     <- d$timers_size_latency_mean
  
  d$rate_add      <- d$timers_add_latency_mean_rate
  d$rate_contains <- d$timers_contains_latency_mean_rate
  d$rate_size     <- d$timers_size_latency_mean_rate
  
  d$overall_latency_mean <- 
      d$timers_read_latency_mean * num(d$ipa_rawmix_counter_mix_read) + 
      d$timers_incr_latency_mean * num(d$ipa_rawmix_counter_mix_incr)
  
  d$inconsistent <- d$counters_inconsistent_count
  d$consistent <- d$counters_consistent_count
  d$inconsistent_fraction <- d$inconsistent / (d$inconsistent + d$consistent)

  # d$fraction_immediate <- d$res_immediates_total / d$res_incrs_total

  d$overall_rate_mean <- d$timers_read_latency_mean_rate + d$timers_incr_latency_mean_rate
  
  d$interval_percent_mean <- d$histograms_interval_percent_mean / 100
  d$interval_percent_p99 <- d$histograms_interval_percent_p99 / 100
  d$interval_percent_max <- d$histograms_interval_percent_max / 100
  
  log("!! warning: correcting for interval percent (these experiments were done with + and - delta, which is actually incorrect)")
  d[grepl('error',d$bound),]$interval_percent_mean <- d[grepl('error',d$bound),]$interval_percent_mean / 2
  d[grepl('error',d$bound),]$interval_percent_p99 <- d[grepl('error',d$bound),]$interval_percent_p99 / 2
  d[grepl('error',d$bound),]$interval_percent_max <- d[grepl('error',d$bound),]$interval_percent_max / 2
  
  # aliases
  aliases <- c(
    read_lat_mean='timers_read_latency_mean',
    read_lat_median='timers_read_latency_p50',
    read_lat_p95='timers_read_latency_p95',
    read_lat_p99='timers_read_latency_p99',
    read_count='timers_read_latency_count',
    read_rate='timers_read_latency_mean_rate',
    incr_lat_mean='timers_incr_latency_mean',
    incr_lat_median='timers_incr_latency_p50',
    incr_lat_p95='timers_incr_latency_p95',
    incr_lat_p99='timers_incr_latency_p99',
    incr_count='timers_incr_latency_count',
    incr_rate='timers_incr_latency_mean_rate',
    read_strong='counters_read_strong_count',
    read_weak='counters_read_weak_count',
    correct='counters_correct_count',
    incorrect='counters_incorrect_count',
    contains='counters_contains_count',
    contains_not='counters_contains_not_count',
    interval_mean='histograms_interval_width_mean',
    error_mean='histograms_error_mean',
    error_max='histograms_error_max',
    error_stddev='histograms_error_stddev',
    mix_read='ipa_rawmix_counter_mix_read',
    mix_incr='ipa_rawmix_counter_mix_incr',
    lease='ipa_lease_period'
  )
  for (n in names(aliases)) d[[n]] <- d[[aliases[n]]]
  
  d$read_strong_fraction <- with(d, read_strong / (read_strong + read_weak))

  for (f in c('refreshes','out_of_bounds','immediates','incrs','reads', 'cached_reads')) {
    d[["res_"+f+"_total"]] <- rowSums(cgrep(d,'res_counters_'+f+'_count_'))
  }
  
  for (f in c('cass_op', 'weak_read', 'strong_read', 'weak_write', 'strong_write')) {
    d[['res_'+f+'_lat_mean']] <- rowMeans(cgrep(d,'res_timers_'+f+'_latency_mean_'))
  }
  
  d$mix <- factor.remap(d$mix, mixes.counter)
  
    
  return(d)
}

data.ipa.twitter <- function(where="out_actual_time_length is not null") {
  d <- data.ipa.common(table="ipa_owl", where=where)
  
  # compute totals for reservation counters
  counters <- c('allocs', 'immediates', 'out_of_bounds', 'refreshes', 'size_cached', 'size_ops')
  
  for (f in counters) d[["res_"+f+"_total"]] <- rowSums(cgrep(d,'res_counters_'+f+'_count_'))
  
  # compute means for other reservation metrics
  # timers <- c('consume', 'transfer')
  # for (f in timers) d[['res_'+f+'_lat_mean']] <- rowMeans(cgrep(d,'res_timers_'+f+'_latency_mean_'))
  
  # aliases
  aliases <- c(
    lease='ipa_lease_period',
        
    follow_lat_mean='timers_follow_mean',
    follow_lat_median='timers_follow_p50',
    follow_lat_p95='timers_follow_p95',
    follow_lat_p99='timers_follow_p99',
    follow_rate='timers_follow_mean_rate',
    follow_count='timers_follow_count',
    
    retweet_lat_mean='timers_retweet_mean',
    retweet_lat_median='timers_retweet_p50',
    retweet_lat_p95='timers_retweet_p95',
    retweet_lat_p99='timers_retweet_p99',
    retweet_rate='timers_retweet_mean_rate',
    retweet_count='timers_retweet_count',
    
    tweet_lat_mean='timers_tweet_mean',
    tweet_lat_median='timers_tweet_p50',
    tweet_lat_p95='timers_tweet_p95',
    tweet_lat_p99='timers_tweet_p99',
    tweet_rate='timers_tweet_mean_rate',
    tweet_count='timers_tweet_count',
    
    tweet_load_lat_mean='timers_tweet_load_mean',
    tweet_load_lat_median='timers_tweet_load_p50',
    tweet_load_lat_p95='timers_tweet_load_p95',
    tweet_load_lat_p99='timers_tweet_load_p99',
    tweet_load_rate='timers_tweet_load_mean_rate',
    tweet_load_count='timers_tweet_load_count',
    
    timeline_lat_mean='timers_timeline_mean',
    timeline_lat_median='timers_timeline_p50',
    timeline_lat_p95='timers_timeline_p95',
    timeline_lat_p99='timers_timeline_p99',
    timeline_rate='timers_timeline_mean_rate',
    timeline_count='timers_timeline_count',
    
    user_lat_mean='timers_user_mean',
    user_lat_median='timers_user_p50',
    user_lat_p95='timers_user_p95',
    user_lat_p99='timers_user_p99',
    user_rate='timers_user_mean_rate',
    user_count='timers_user_count'
    
  )
  for (n in names(aliases)) d[[n]] <- d[[aliases[n]]]
  
  d$overall_lat_mean <- with(d,
    (follow_lat_mean * follow_count +
    retweet_lat_mean * retweet_count +
    tweet_lat_mean * tweet_count +
    timeline_lat_mean * timeline_count +
    user_lat_mean * user_count) /
    (follow_count+retweet_count+tweet_count+timeline_count+user_count)
  )
  
  d$overall_rate <- with(d,
    follow_rate + retweet_rate + tweet_rate + timeline_rate + user_rate
  )

  d$total_count <- with(d,
    follow_count + retweet_count + tweet_count + timeline_count + user_count
  )
  
  d$follow_percent <- d$follow_count / d$total_count * 100
  d$retweet_percent <- d$retweet_count / d$total_count * 100
  d$tweet_percent <- d$tweet_count / d$total_count * 100
  d$timeline_percent <- d$timeline_count / d$total_count * 100
  d$user_percent <- d$user_count / d$total_count * 100
  
  return(d)
}

data.owl <- function(where="meters_retwis_op_count is not null") {
  d <- db("select * from ipa_owl where meters_retwis_op_count is not null and " + where)
  
  fields <- names(d)[grepl(".*(_count|_rate|_p\\d+|_max|_min|_mean)$", names(d))]
  for (f in fields) d[[f]] <- num(d[[f]])
    
  d$duration <- d$ipa_retwis_duration
  d$blockade <- d$blockade_mode
  d$c_reqs <- d$ipa_concurrent_requests
  d$retwis_op_rate <- d$meters_retwis_op_mean_rate
  d$op_rate <- d$timers_cass_op_latency_mean_rate
  d$op_lat_mean <- d$timers_cass_op_latency_mean
  d$op_lat_median <- d$timers_cass_op_latency_p50
  
  return(d)
}




error.database_unreachable <- function(e) {
  write("\n!! Database unreachable. Reading stashed results from CSV.", stderr())
  print(e)
  write("", stderr())
}
