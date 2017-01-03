works_with_R("3.2.3", data.table="1.9.7")

temp.wide <- fread("degreesC.csv")

some.wide <- temp.wide[date.str %in% c("2016-11-18"),]
some.wide <- temp.wide[date.str %in% c("2016-10-18", "2016-10-19"),]

some.tall <- melt(some.wide, value.name="degreesC", variable.name="minute.str", id.vars="date.str")
some.tall[, minute.POSIXct := as.POSIXct(strptime(paste(date.str, minute.str), "%Y-%m-%d %H:%M"))]

limit.POSIXct <- strptime(c("2016-10-19 03:00", "2016-10-19 06:55"), "%Y-%m-%d %H:%M")
lim.dt <- some.tall[limit.POSIXct[1] < minute.POSIXct & minute.POSIXct < limit.POSIXct[2],]
ggplot()+
  geom_point(aes(minute.POSIXct, degreesC), data=lim.dt, color="red")+
  geom_line(aes(minute.POSIXct, degreesC), data=some.tall)

