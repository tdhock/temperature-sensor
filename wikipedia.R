works_with_R(
  "3.4.2",
  ggplot2="2.2.1",
  namedCapture="2017.6.1",
  htmltab="0.7.1",
  "Rdatatable/data.table@3db6e9832e1491cb0e10f8bdcd4348f5c2b1ac8a")

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
url.vec <- c(
  Tustin="https://en.wikipedia.org/wiki/Tustin,_California",
  Berkeley="https://en.wikipedia.org/wiki/Berkeley,_California",
  Paris="https://en.wikipedia.org/wiki/Paris",
  Tokyo="https://en.wikipedia.org/wiki/Tokyo",
  Montreal="https://en.wikipedia.org/wiki/Montreal"
  ## ,Flagstaff="https://en.wikipedia.org/wiki/Flagstaff,_Arizona"
  ## ,Waterloo="https://en.wikipedia.org/wiki/Waterloo,_Ontario"
  ## ,San_Diego="https://en.wikipedia.org/wiki/San_Diego"
  )
climate.dt.list <- list()
for(city in names(url.vec)){
  city.html <- file.path("wikipedia", paste0(city, ".html"))
  if(!file.exists(city.html)){
    u <- url.vec[[city]]
    download.file(u, city.html)
  }
  df <- htmltab(city.html, which="//th[text()='Month']/ancestor::table")
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
    month=factor(col.name.vec[col.indices], col.name.vec[col.indices]),
    t(matrix(
      to.numeric(chr.mat), nrow(chr.mat), ncol(chr.mat),  
      dimnames=list(new.row.names))))
  tall.dt <- melt(wide.dt, id.vars="month")
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
