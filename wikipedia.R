library(ggplot2)
library(namedCapture)
library(htmltab)
library(data.table)

varname.pattern <- paste0(
  "(?<varname>.*)",
  " ",
  "(?<before>[^ (]+)",
  " [(]",
  "(?<inside>[^)%]+)",
  "[)]")
data.pattern <- paste0(
  "(?<before>.*?)",
  "[(]",
  "(?<inside>[^)%]+)",
  "[)]")
options(namedCapture.engine="PCRE")
to.numeric <- function(chr.vec){
  as.numeric(gsub(",", "", gsub("−", "-", chr.vec)))
}
abbrev.vec <- c(
  NULL
 ##,Tustin="Tustin,_California"
  ## ,Havasu="Lake_Havasu_City,_Arizona"
  ## ,Berkeley="Berkeley,_California"
  ## ,"Paris"
  ## ,"Tokyo"
  ## ,"Sherbrooke"
  ## ,SherbrookeFR="https://fr.wikipedia.org/wiki/Sherbrooke"
  ## ,Santa_Cruz="Santa_Cruz,_California"
  ## ,"Brisbane"
  ## ,"Singapore"
  ## ,"Hangzhou"
  ## ,"Tehran"
 ##,Kuwait="Kuwait_City"
   ## ,"Calgary"
   ## ,"Toronto"
  ## ,"Vancouver"
  ## ,Halifax="Halifax,_Nova_Scotia"
  ## ,"Minneapolis"
  ##,"Guadalajara"
  ##,"Tromso"
  ##,"Pau"="Pau,_Pyrénées-Atlantiques"
  ,"Montreal"
 ##,"Alicante"
  ##,"Riverside"="Riverside,_California"
  ##,"Los_Angeles"
  ##,"Geneva"
 ##,"Rio_de_Janeiro"
  ##,Panama="Panama_City"
  ##,"Timbuktu"
  ##,"Dubai"
 ##,"New_Orleans"
 ##,"New_Delhi"
 ##,"Vostok"="Vostok_Station"
  ##,"Tahiti"="Papeete"
  ##,"Puerto_Rico"
  ##,"Buenos_Aires"
 ,Flagstaff="Flagstaff,_Arizona"
  ,Utica="Utica,_New_York"
 ##,Burlington="Burlington,_Vermont"
 ##,Hilo="Hilo,_Hawaii"
 ## ,"Aarhus"
 ##,"Fort_Nelson"="Fort_Nelson,_British_Columbia"
  ##,Waterloo="Waterloo,_Ontario"
 ##,"San_Diego"
  ##,"Toulouse"
  ##,"Quebec"="https://fr.wikipedia.org/wiki/Québec_(ville)"
)
url.vec <- paste0(
  ifelse(
    grepl("^http", abbrev.vec),
    "",
    "https://en.wikipedia.org/wiki/"),
  abbrev.vec)

if(is.null(names(abbrev.vec)))names(abbrev.vec) <- rep("", length(abbrev.vec))
names(url.vec) <- ifelse(
  names(abbrev.vec)=="",
  abbrev.vec,
  names(abbrev.vec))
month.str <- c(
  "Jan", "Feb", "Mar","Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
parser.fun.list <- list(en=function(city.html){
  tryCatch({
    df <- htmltab(city.html, which="//th[text()='Month\n']/ancestor::table")
  }, error=function(e){
    ##unlink(city.html)
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
  row.name.mat <- str_match_named(row.name.vec, varname.pattern)
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
  data.match.mat <- str_match_named(paste0(" ", chr.mat), data.pattern, list(
    before=to.numeric,
    inside=to.numeric))
  not.na <- !is.na(data.match.mat[,1])
  chr.mat[not.na] <- data.match.mat[not.na, keep.name]
  chr.mat[chr.mat=="trace"] <- "0"
  new.row.names <- ifelse(
    is.na(row.name.mat[, "varname"]),
    row.name.vec,
    sprintf(
      "%s (%s)",
      row.name.mat[, "varname"],
      row.name.mat[, keep.name]))
  matrix(
    to.numeric(chr.mat), nrow(chr.mat), ncol(chr.mat),  
    dimnames=list(new.row.names))
}, fr=function(city.html){
  tryCatch({
    df <- htmltab(
      city.html,
      which="//th[text()='Month']/ancestor::table")#for record
  }, error=function(e){
    unlink(city.html)
    browseURL(u)
    stop(
      "unable to find climate data in web page ",
      u, " -- either there is no table with climate data ",
      "(is that a disambiguation page?), ",
      " or we need to update the code to parse the unrecognized table")
  })
  raw.mat <- as.matrix(df[, 2:13])
  rownames(raw.mat) <- df[, 1]
  extra.year.vec <- c(
    "Record de froid (°C)date du record",
    "Record de chaleur (°C)date du record")
  extra.year.some <- extra.year.vec[extra.year.vec %in% rownames(raw.mat)]
  raw.mat[extra.year.some,] <- sub(
    "[0-9]{4}$", "", raw.mat[extra.year.some, ], perl=TRUE)
  to.rep <- rownames(raw.mat)%in%extra.year.some
  rownames(raw.mat)[to.rep] <- sub(
    "date du record$", "",
    rownames(raw.mat)[to.rep])
  var.trans.vec <- c(
    "Température minimale moyenne (°C)"="Average low (°C)",
    "Température maximale moyenne (°C)"="Average high (°C)",
    "Température moyenne (°C)"="Average temp (°C)",
    "Record de froid (°C)"="Record low (°C)",
    "Record de chaleur (°C)"="Record high (°C)",
    "Ensoleillement (h)"="Mean monthly sunshine hours",
    "Précipitations (mm)"="Average precipitation (mm)",
    "dont pluie (mm)"="Average rainfall (mm)",
    "dont neige (cm)"="Average snowfall (cm)",
    "Nombre de jours avec précipitations"="Average precipitation (days)",
    "dont nombre de jours avec précipitations ≥ 5 mm"="Average ≥5 mm rain (days)")
  is.found <- rownames(raw.mat) %in% names(var.trans.vec) 
  not.found <- rownames(raw.mat)[!is.found]
  if(length(not.found)){
    print(not.found)
    stop(
      "data contains variables that the code does not handle (printed above)")
  }
  matrix(
    as.numeric(sub("−", "-", sub(",", ".", raw.mat))),
    nrow(raw.mat), ncol(raw.mat),  
    dimnames=list(var.trans.vec[rownames(raw.mat)]))
})

climate.dt.list <- list()
for(city in names(url.vec)){
  city.html <- file.path("wikipedia", paste0(city, ".html"))
  u <- url.vec[[city]]
  language <- sub(".*//", "", sub("[.].*", "", u))
  city.csv <- file.path("wikipedia", paste0(city, ".csv"))
  if(file.exists(city.csv)){
    wide.dt <- fread(city.csv)
  }else{
    if(!file.exists(city.html)){
      download.file(u, city.html)
    }
    parser.fun <- parser.fun.list[[language]]
    climate.mat <- parser.fun(city.html)
    stopifnot(ncol(climate.mat)==12)
    wide.dt <- data.table(
      month=month.str,
      t(climate.mat))
    fwrite(wide.dt, city.csv)
  }
  wide.dt[, month.fac := factor(month, month.str)]
  tall.dt <- melt(
    wide.dt,
    id.vars=c("month", "month.fac"),
    variable.factor=FALSE)
  climate.dt.list[[city]] <- data.table(
    city=factor(city, names(url.vec)),
    tall.dt)
}
system("git add wikipedia/*")
climate.dt <- do.call(rbind, climate.dt.list)
dim(counts.dt <- dcast(climate.dt, city ~ variable, length))
if(all(c("Average rainfall (mm)", "Average precipitation (mm)") %in% names(counts.dt))){
  counts.dt[`Average precipitation (mm)`==0 & `Average rainfall (mm)`>0, list(city, `Average rainfall (mm)`,`Average precipitation (mm)`)]
  (only.prec <- counts.dt[`Average precipitation (mm)`>0 & `Average rainfall (mm)`==0, list(city, `Average rainfall (mm)`,`Average precipitation (mm)`)])
  counts.dt[`Average precipitation (mm)`>0 & `Average rainfall (mm)`>0, list(city, `Average rainfall (mm)`,`Average precipitation (mm)`)]#cities in Canada report both rainfall and precipitation.
  climate.dt[city %in% only.prec$city & variable=="Average precipitation (mm)", variable := "Average rainfall (mm)"]
}
climate.dt[, list(values=.N), by=variable][order(values)]
show.vars <- c(
  "Average high (°C)", "Average low (°C)",
  "Record high (°C)", "Record low (°C)",
  "Average rainfall (mm)", "Average snowfall (cm)",
  "Mean monthly sunshine hours")
show.dt <- climate.dt[variable %in% show.vars]
show.wide <- dcast(show.dt, city + month.fac ~ variable)
breaks.vec <- seq(1, 12, by=2)

f <- function(y, what, yname){
  yval <- show.wide[[yname]]
  if(is.numeric(yval)){
    show.wide[, data.table(
      city, month.fac, y, what, yval, yname)]
  }
}
lines.dt <- rbind(
  f("rainfall (mm)", "Monthly average", "Average rainfall (mm)"),
  f("sunshine (hours)", "Monthly average", "Mean monthly sunshine hours"),
  f("snowfall (cm)", "Monthly average", "Average snowfall (cm)"),
  f("temperature (°C)", "Record high/low", "Record high (°C)"),
  f("temperature (°C)", "Record high/low", "Record low (°C)"))

gg.panels <- ggplot()+
  geom_ribbon(aes(
    as.numeric(month.fac),
    fill=what,
    ymax=`Average high (°C)`,
    ymin=`Average low (°C)`),
    alpha=0.5,
    data=data.table(
      show.wide,
      y="temperature (°C)",
      what="Average daily high/low"))+
  geom_line(aes(
    as.numeric(month.fac),
    yval,
    group=yname,
    color=what),
    data=lines.dt)+
  theme_bw()+
  theme(
    panel.spacing=grid::unit(0, "lines"),
    legend.position="bottom")+
  facet_grid(y ~ city, scales="free")+
  scale_color_manual("", values=c(
    "Monthly average"="black",
    "Record high/low"="grey"))+
  scale_fill_manual("", values=c(
    "Average daily high/low"="black"))+
  scale_x_continuous(
    "Month",
    breaks=breaks.vec,
    labels=month.str[breaks.vec])+
  scale_y_continuous(
    "")
##png("figure-wikipedia-Montreal-Burlington-Hilo-Flagstaff.png", 8, 8, units="in", res=100)
print(gg.panels)
##dev.off()

city.colors <- c(
  Montreal="black",
  Hilo="orange",
  Burlington="green",
  Kuwait="purple",
  San_Diego="orange",
  Rio_de_Janeiro="orange",
  Aarhus="blue",
  Timbuktu="orange",
  Panama="orange",
  Tustin="orange",
  Guadalajara="red",
  Los_Angeles="blue",
  Fort_Nelson="deepskyblue",
  Pau="pink",
  Alicante="red",
  Calgary="red",
  Riverside="blue",
  Tromso="green",
  Toronto="blue",
  Geneva="red",
  Dubai="red",
  Vostok="blue",
  New_Delhi="orange",
  New_Orleans="blue",
  Utica="green",
  Flagstaff="violet")
gg <- ggplot()+
  geom_rect(aes(
    xmin=xmin, xmax=xmax,
    ymin=ymin, ymax=ymax),
    data=data.table(ymin=-Inf, ymax=Inf, xmin=5, xmax=7),
    fill="grey")+
  geom_ribbon(aes(
    as.numeric(month.fac),
    fill=city,
    ymax=`Average high (°C)`,
    ymin=`Average low (°C)`),
    alpha=0.5,
    data=data.table(
      show.wide,
      y="temperature (°C)",
      what="Average daily high/low"))+
  geom_line(aes(
    as.numeric(month.fac),
    yval,
    group=paste(city, yname),
    color=city),
    data=lines.dt)+
  theme_bw()+
  theme(
    panel.spacing=grid::unit(0, "lines"),
    legend.position="bottom")+
  facet_grid(y ~ ., scales="free")+
  scale_color_manual("", values=city.colors)+
  scale_fill_manual("", values=city.colors)+
  scale_x_continuous(
    "Month",
    breaks=breaks.vec,
    labels=month.str[breaks.vec])+
  scale_y_continuous(
    "",
    breaks=seq(-100, 1000, by=10))
png("figure-wikipedia-Montreal-Flagstaff-Utica.png", 4, 6, units="in", res=100)
print(gg)
dev.off()
