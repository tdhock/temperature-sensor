works_with_R("3.2.3", data.table="1.9.7",
             RJSONIO="1.3.0")

temp.every.minute <- fread("grep ' ' ../time_degreesC.log|tr -cd '\\11\\12\\15\\40-\\176'")
setnames(temp.every.minute, c("datetime.str", "degrees.C"))
temp.every.minute[, datetime.POSIXct := as.POSIXct(
  strptime(datetime.str, "%Y-%m-%d_%H:%M"))]
temp.every.minute[, hour.str := strftime(datetime.POSIXct, "%Y-%m-%d_%H")]
temp.every.minute[, date.str := strftime(datetime.POSIXct, "%Y-%m-%d")]
temp.every.minute[, minute.str := strftime(datetime.POSIXct, "%H:%M")]
temp.every.minute[, hour.POSIXct := as.POSIXct(strptime(hour.str, "%Y-%m-%d_%H"))]

temp.unique <- temp.every.minute[, list(
  observations=.N,
  mean.degrees.C=mean(degrees.C)
  ), by=.(datetime.POSIXct, minute.str, date.str)]
temp.wide <- dcast(temp.unique, date.str ~ minute.str, value.var="mean.degrees.C")
fwrite(temp.wide, "degreesC.csv")

outlier.list <- list(
  c("2016-11-18 18:24:00", "2016-11-18 18:46:00"),
  c("2016-10-19 03:00", "2016-10-19 06:55"))
is.outlier <- rep(FALSE, nrow(temp.every.minute))
for(outlier.vec in outlier.list){
  lim.vec <- strptime(outlier.vec, "%Y-%m-%d %H:%M")
  this.outlier <- temp.every.minute[, lim.vec[1] < datetime.POSIXct & datetime.POSIXct < lim.vec[2] ]
  is.outlier <- is.outlier | this.outlier
}
temperature <- temp.every.minute[!is.outlier, {
  data.table(degrees.C=mean(degrees.C),
             min.POSIXct=min(datetime.POSIXct),
             max.POSIXct=max(datetime.POSIXct))
}, by=hour.POSIXct]

temperature[, day.str := strftime(hour.POSIXct, "%Y-%m-%d")]
temperature[, day.POSIXct := as.POSIXct(strptime(day.str, "%Y-%m-%d"))]
temperature[, day := strftime(day.POSIXct, "%A %d %b %Y")]
get.hour <- function(time.vec){
  stopifnot(inherits(time.vec, "POSIXct"))
  hours.only <- as.integer(strftime(time.vec, "%H"))
  minutes.only <- as.integer(strftime(time.vec, "%M"))
  hours.only + minutes.only/60
}
temperature[, hours.after.midnight := get.hour(hour.POSIXct)]
temperature[, min.hours := get.hour(min.POSIXct)]
temperature[, max.hours := get.hour(max.POSIXct)]
holiday.vec <- c(
  sprintf("2015-12-%02d", 19:31),
  sprintf("2016-01-%02d", 1:3))
temperature[, is.weekend := grepl("Sat|Sun", day)]
temperature[, is.holiday := day.str %in% holiday.vec]
temperature[, work.status := ifelse(
                            is.holiday|is.weekend, "day off", "work day")]


quartiles <- temperature[, {
  q.vec <- quantile(degrees.C)
  q.list <- as.list(q.vec)
  names(q.list) <- paste0("quantile", sub("%", "", names(q.list)))
  q.list$measurements <- .N
  q.list$first.time <- min(min.hours)
  q.list$last.time <- max(max.hours)
  do.call(data.table, q.list)
}, by=.(day, day.str, day.POSIXct, work.status)]

seconds.in.a.day <- 60 * 60 * 24 
half.day <- seconds.in.a.day/2
quartiles[, half.before := day.POSIXct - half.day]
quartiles[, half.after := day.POSIXct + half.day]

day.vec <- unique(quartiles[23 < last.time, day.str])
dir.create("history", showWarnings=FALSE)
file.vec <- file.path("history", paste0(day.vec, ".json"))
names(file.vec) <- gsub("-", "", day.vec)
missing.vec <- file.vec[!file.exists(file.vec)]
for(date.str in names(missing.vec)){
  json.path <- missing.vec[[date.str]]
  ## wunderground.key is defined in my ~/.Rprofile
  json.url <- sprintf(
    "http://api.wunderground.com/api/%s/history_%s/q/Canada/Montreal.json",
    wunderground.key, date.str)
  is.valid.json <- function(){
    tryCatch({
      json.list <- fromJSON(json.path)
      TRUE
    }, error=function(e){
      system(paste("cat", json.path))
      FALSE
    })
  }
  while(!is.valid.json()){
    download.file(json.url, json.path)
    Sys.sleep(6) ## There is a limit of 10 calls per minute.
  }
}

json.file.vec <- Sys.glob(file.path("history", "*.json"))
outside.list <- list()
for(json.file.i in seq_along(json.file.vec)){
  json.file <- json.file.vec[[json.file.i]]
  cat(sprintf("%4d / %4d %s\n", json.file.i, length(json.file.vec), json.file))
  L <- fromJSON(json.file)
  date.mat <- sapply(L$history$observations, "[[", "date")
  date.dt <- data.table(t(date.mat))
  date.dt$degrees.F <- as.numeric(sapply(L$history$observations, "[[", "tempi"))
  date.dt[, degrees.C := (degrees.F - 32) * 5 / 9]
  date.dt[, time.str := paste0(year, mon, mday, hour, min)]
  date.dt[, hour.num := as.numeric(hour)]
  date.dt[, time.POSIXct := as.POSIXct(strptime(time.str, "%Y%m%d%H%M"))]
  outside.list[[json.file]] <- date.dt
}
outside <- do.call(rbind, outside.list)
outside[, day.str := strftime(time.POSIXct, "%Y-%m-%d")]
outside[, day.POSIXct := as.POSIXct(strptime(day.str, "%Y-%m-%d"))]
outside[, day := strftime(day.POSIXct, "%A %d %b %Y")]

outside.quartiles <- outside[, {
  q.vec <- quantile(degrees.C)
  q.list <- as.list(q.vec)
  names(q.list) <- paste0("quantile", sub("%", "", names(q.list)))
  q.list$measurements <- .N
  do.call(data.table, q.list)
}, by=.(day, day.str, day.POSIXct)]

save(temperature, quartiles, outside, outside.quartiles,
     file="temperature.RData")
