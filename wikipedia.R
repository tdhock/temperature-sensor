works_with_R(
  "3.4.2",
  ggplot2="2.2.1",
  namedCapture="2017.6.1",
  htmltab="0.7.1",
  data.table="1.10.4")

pattern <- paste0(
  "(?<varname>.*)",
  " ",
  "(?<before>[^ (]+)",
  " [(]",
  "(?<inside>[^)]+)",
  "[)]")
to.numeric <- function(chr.vec){
  as.numeric(gsub(",", "", gsub("−", "-", chr.vec)))
}
abbrev.vec <- c(
  Tustin="Tustin,_California",
  Havasu="Lake_Havasu_City,_Arizona",
  Berkeley="Berkeley,_California",
  "Paris",
  "Tokyo",
  ##"Sherbrooke",
  ## SherbrookeFR="https://fr.wikipedia.org/wiki/Sherbrooke",
  ## Santa_Cruz="Santa_Cruz,_California",
  ## "Brisbane",
  ## "Singapore",
  ## "Hangzhou",
  ## "Tehran",
  ## Kuwait="Kuwait_City",
  ## "Calgary",
  ## "Toronto",
  ## "Vancouver",
  ## Halifax="Halifax,_Nova_Scotia",
  ## "Minneapolis",
  "Montreal"
 ,Flagstaff="Flagstaff,_Arizona"
 ,Waterloo="Waterloo,_Ontario"
 ,"San_Diego"
 ,"Quebec"="https://fr.wikipedia.org/wiki/Québec_(ville)"
)
url.vec <- paste0(
  ifelse(
    grepl("^http", abbrev.vec),
    "",
    "https://en.wikipedia.org/wiki/"),
  abbrev.vec)
names(url.vec) <- ifelse(
  names(abbrev.vec)=="",
  abbrev.vec,
  names(abbrev.vec))
month.str <- c(
  "Jan", "Feb", "Mar","Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
month <- factor(month.str, month.str)
parser.fun.list <- list(en=function(city.html){
  tryCatch({
    df <- htmltab(city.html, which="//th[text()='Month']/ancestor::table")
  }, error=function(e){
    unlink(city.html)
    browseURL(u)
    stop(
      "unable to find climate data in web page ",
      u, " -- either there is no table with climate data ",
      "(is that a disambiguation page?), ",
      " or we need to update the code to parse the unrecognized table")
  })
  col.name.vec <- sub(".*> ", "", names(df))
  row.indices <- -grep("^Source", df[,1])
  row.name.vec <- df[row.indices, 1]
  row.name.mat <- str_match_named(row.name.vec, pattern)
  col.indices <- 2:13
  is.C <- row.name.mat=="°C"
  keep.name <- if(any(is.C[, "inside"], na.rm=TRUE)){
    "inside"
  }else if(any(is.C[, "before"], na.rm=TRUE)){
    "before"
  }else{
    stop("no °C in or before parentheses")
  }
  chr.mat <- unname(as.matrix(df[row.indices, col.indices]))
  data.match.mat <- str_match_named(paste0(" ", chr.mat), pattern, list(
    before=to.numeric,
    inside=to.numeric))
  not.na <- !is.na(data.match.mat[,1])
  chr.mat[not.na] <- data.match.mat[not.na, keep.name]
  chr.mat[chr.mat=="trace"] <- "0"
  new.row.names <- ifelse(
    is.na(row.name.mat[, "varname"]),
    row.name.vec,
    row.name.mat[, "varname"])
  wide.dt <- data.table(
    month,
    t(matrix(
      to.numeric(chr.mat), nrow(chr.mat), ncol(chr.mat),  
      dimnames=list(new.row.names))))
  melt(wide.dt, id.vars="month")
}, fr=function(city.html){
  tryCatch({
    df <- htmltab(city.html, which="//th[text()='Mois']/ancestor::table")
  }, error=function(e){
    unlink(city.html)
    browseURL(u)
    stop(
      "unable to find climate data in web page ",
      u, " -- either there is no table with climate data ",
      "(is that a disambiguation page?), ",
      " or we need to update the code to parse the unrecognized table")
  })
  row.name.vec <- df[, 1]
  col.indices <- 2:13
  var.trans.vec <- c(
    "Température minimale moyenne (°C)"="Average low",
    "Température maximale moyenne (°C)"="Average high")
  is.found <- names(var.trans.vec) %in% row.name.vec
  not.found <- names(var.trans.vec)[!is.found]
  if(length(not.found)){
    stop(
      "need variables (",
      paste(not.found, collapse=", "),
      ") which are not present in data (",
      paste(row.name.vec, collapse=", "),
      ")")
  }
  chr.mat <- unname(as.matrix(df[, col.indices]))
  corrected.mat <- matrix(
    as.numeric(sub("−", "-", sub(",", ".", chr.mat))),
    nrow(chr.mat), ncol(chr.mat),  
    dimnames=list(row.name.vec))[names(var.trans.vec),]
  rownames(corrected.mat) <- var.trans.vec
  wide.dt <- data.table(
    month,
    t(corrected.mat))
  melt(wide.dt, id.vars="month")
})
climate.dt.list <- list()
for(city in names(url.vec)){
  city.html <- file.path("wikipedia", paste0(city, ".html"))
  u <- url.vec[[city]]
  if(!file.exists(city.html)){
    download.file(u, city.html)
  }
  language <- sub(".*//", "", sub("[.].*", "", u))
  parser.fun <- parser.fun.list[[language]]
  tall.dt <- parser.fun(city.html)
  climate.dt.list[[city]] <- data.frame(city, tall.dt)
}
climate.dt <- data.table(do.call(rbind, climate.dt.list))
temp.dt <- climate.dt[variable %in% c("Average high", "Average low")]
temp.wide <- dcast(temp.dt, city + month ~ variable)
breaks.vec <- seq(1, 12, by=2)
ggplot()+
  geom_ribbon(aes(as.numeric(month), ymax=`Average high`, ymin=`Average low`),
              alpha=0.5,
              data=temp.wide)+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(. ~ city)+
  scale_x_continuous(
    "Month",
    breaks=breaks.vec,
    labels=levels(temp.wide$month)[breaks.vec])+
  scale_y_continuous(
    "Average high and low temperatures (degrees Celsius)")
