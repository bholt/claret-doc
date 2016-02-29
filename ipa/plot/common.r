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
  d$cc_ph <- factor(d$cc_ph, levels=c(COMB+PH,COMM+PH,RW+PH,COMB,COMM,RW+BASE,NOTXN))
  d
}

data.or.csv <- function(csv, gen) {
  d <- tryCatch(
    {
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

factor.remap <- function(col, map) factor(revalue(col, map), levels=vals(map))

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
    strip.background = element_blank()
  )
)

group.legend <- function(title='Bounds') list(
    scale_fill_discrete(guide=guide_legend(title=title)),
    scale_color_discrete(guide=guide_legend(title=title))
)

# get columns of `d` by `regex`
cgrep <- function(d, regex) d[grep(regex,names(d))]


theme.bar <-function() theme(
  axis.text.x = element_text(angle = 45, hjust = 1),
  axis.title.x = element_blank(),
  legend.position="none"
)

bounds <- c(
  'consistency:strong' = 'strong (read)',
  'consistency:strongwrite' = 'strong (write)',
  'tolerance:0.05' = 'error: 5%',
  'tolerance:0.1' = 'error: 10%',
  'latency:50ms' = 'latency: 50ms',
  'latency:10ms' = 'latency: 10ms',
  'consistency:weak' = 'weak'
)

b.cst <- "consistency:strong"
b.csw <- "consistency:strongwrite"
b.cwk <- "consistency:weak"
b.l10 <- "latency:10ms"
b.l50 <- "latency:50ms"
b.t10 <- "tolerance:0.1"
b.t05 <- "tolerance:0.05"

ipa.scales <- function(name = 'Bounds', guide = guide_legend(nrow=8), ...) {
  colors <- c()
  colors[[ bounds[[b.csw]] ]] <- c.gray
  colors[[ bounds[[b.cst]] ]] <- c.gray
  colors[[ bounds[[b.cwk]] ]] <- c.red
  colors[[ bounds[[b.l10]] ]] <- c.blue
  colors[[ bounds[[b.l50]] ]] <- c.blue
  colors[[ bounds[[b.t10]] ]] <- c.green
  colors[[ bounds[[b.t05]] ]] <- c.green
  
  solid  <- 1
  dashed <- 2
  dotted <- 3
  lines <- c()
  lines[[ bounds[[b.csw]] ]] <- dashed
  lines[[ bounds[[b.cst]] ]] <- solid
  lines[[ bounds[[b.cwk]] ]] <- dotted
  lines[[ bounds[[b.l10]] ]] <- solid
  lines[[ bounds[[b.l50]] ]] <- dashed
  lines[[ bounds[[b.t05]] ]] <- solid
  lines[[ bounds[[b.t10]] ]] <- dashed
  
  list(
    scale_fill_manual(values=colors, name=name, guide=guide, ...),
    scale_color_manual(values=colors, name=name, guide=guide, ...),
    scale_linetype_manual(values=lines, name = name, guide = guide, ...),
    expand_limits(y = 0)
  )
}

data.ipa.rawmix <- function(where="honeycomb_mode is not null and out_actual_time_length is not null") data.or.csv (
  csv = COMMON_DIR+'/data/ipa_rawmix.csv',
  gen = function(){
    d <- db("select * from ipa_rawmix where timers_cass_op_latency_count is not null and " + where)
    fields <- names(d)[grepl(".*(_count|_rate|_p\\d+|_max|_min|_mean|_ms)(_\\d)?$", names(d))]
    for (f in fields) d[[f]] <- num(d[[f]])
    
    d$duration <- d$ipa_duration
    d$load <- num(d$ipa_concurrent_requests)

    d$op_rate <- d$timers_cass_op_latency_mean_rate
    d$op_lat_mean <- d$timers_cass_op_latency_mean
    d$op_lat_median <- d$timers_cass_op_latency_p50
    
    d$mean_lat_add      <- d$timers_add_latency_mean
    d$mean_lat_contains <- d$timers_contains_latency_mean
    d$mean_lat_size     <- d$timers_size_latency_mean
    
    d$rate_add      <- d$timers_add_latency_mean_rate
    d$rate_contains <- d$timers_contains_latency_mean_rate
    d$rate_size     <- d$timers_size_latency_mean_rate
    
    for (f in c('refreshes','out_of_bounds','immediates','incrs','reads')) {
      d[["res_"+f+"_total"]] <- rowSums(cgrep(d,'res_counters_'+f+'_count_'))
    }
    
    for (f in c('cass_op', 'weak_read', 'strong_read', 'weak_write', 'strong_write')) {
      d[['res_'+f+'_lat_mean']] <- rowMeans(cgrep(d,'res_timers_'+f+'_latency_mean_'))
    }
    
    low <- 128
    high <- 4096
    
    conds <- c()
    conds[[x('normal',low)]] <- 'Normal'
    conds[[x('normal',high)]] <- 'Normal (high load)'
    conds[[x('slowpoke_flat',low)]] <- 'Slow replica'
    conds[[x('world',low)]] <- 'Geo-distributed'
    
    d$condition <- factor.remap(x(d$honeycomb_mode,d$load), conds)
    
    d$bound <- factor.remap(d$ipa_bound, c(
      'consistency:strong' = 'strong (read)',
      'consistency:strongwrite' = 'strong (write)',
      'tolerance:0.05' = 'error: 5%',
      'tolerance:0.1' = 'error: 10%',
      'latency:10ms' = 'latency: 10ms',
      'latency:50ms' = 'latency: 50ms',
      'consistency:weak' = 'weak'
    ))
    
    return(d)
})

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
