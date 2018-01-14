works_with_R("3.2.3", data.table="1.9.7", ggplot2="1.0")

temp.wide <- fread("degreesC.csv")

some.wide <- temp.wide[date.str %in% c("2016-11-18"),]
some.wide <- temp.wide[date.str %in% c("2016-10-18", "2016-10-19"),]
some.wide <- temp.wide[date.str %in% c("2017-09-28"),]
some.wide <- temp.wide[date.str %in% c("2017-10-05", "2017-10-06")]
some.wide <- temp.wide[date.str %in% c("2017-10-12","2017-10-13"),]
some.wide <- temp.wide[date.str %in% c("2017-10-26", "2017-10-27"),]
some.wide <- temp.wide[date.str %in% c("2017-12-05","2017-12-06","2017-12-07"),]

some.tall <- melt(some.wide, value.name="degreesC", variable.name="minute.str", id.vars="date.str")
some.tall[, minute.POSIXct := as.POSIXct(strptime(paste(date.str, minute.str), "%Y-%m-%d %H:%M"))]

limit.POSIXct <- strptime(c("2017-10-26_12:00", "2017-10-26_18:00"), "%Y-%m-%d_%H:%M")
lim.dt <- some.tall[limit.POSIXct[1] < minute.POSIXct & minute.POSIXct < limit.POSIXct[2],]
ggplot()+
  geom_line(aes(minute.POSIXct, degreesC), data=some.tall)+
  geom_point(aes(minute.POSIXct, degreesC), data=lim.dt, color="red")

