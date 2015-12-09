works_with_R("3.2.2", data.table="1.9.6")

temperature <- fread("../time_degreesC.log")
setnames(temperature, c("datetime.str", "degrees.C"))
temperature[, datetime.POSIXct := as.POSIXct(
  strptime(datetime.str, "%Y-%m-%d_%H:%M"))]
temperature[, day.str := strftime(datetime.POSIXct, "%Y-%m-%d")]
temperature[, day.POSIXct := as.POSIXct(strptime(day.str, "%Y-%m-%d"))]
temperature[, day := strftime(day.POSIXct, "%d %b %Y")]
temperature[, hours.only := as.integer(strftime(datetime.POSIXct, "%H"))]
temperature[, minutes.only := as.integer(strftime(datetime.POSIXct, "%M"))]
temperature[, hours.after.midnight := hours.only + minutes.only/60]

quartiles <- temperature[, {
  q.vec <- quantile(degrees.C)
  q.list <- as.list(q.vec)
  names(q.list) <- paste0("quantile", sub("%", "", names(q.list)))
  q.list$measurements <- .N
  q.list$first.time <- min(hours.after.midnight)
  q.list$last.time <- max(hours.after.midnight)
  do.call(data.table, q.list)
}, by=.(day, day.POSIXct)]

seconds.in.a.day <- 60 * 60 * 24 
half.day <- seconds.in.a.day/2
quartiles[, half.before := day.POSIXct - half.day]
quartiles[, half.after := day.POSIXct + half.day]

save(temperature, quartiles, file="temperature.RData")
