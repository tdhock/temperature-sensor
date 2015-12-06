works_with_R("3.2.2",
             data.table="1.9.6",
             "tdhock/ggplot2@a8b06ddb680acdcdbd927773b1011c562134e4d2",
             "tdhock/animint@89095bc688d77ae5f48bd68ec5ebe299295d4bd9")

load("temperature.RData")

last.times <- temperature[, {
  data.table(hours.after.midnight, degrees.C)[.N, ]
}, by=.(day, day.POSIXct)]

viz <- list(
  title=list("Temperature data from Arduino + LM35 sensor"),
  days=ggplot()+
    ggtitle("Temperature quartiles, select day")+
    theme_bw()+
    theme_animint(width=1000)+
    xlab("day")+
    ylab("temperature (degrees C)")+
    geom_tallrect(aes(xmin=half.before, xmax=half.after,
                      clickSelects=day),
                  alpha=0.5,
                  data=quartiles)+
    geom_ribbon(aes(day.POSIXct, ymin=quantile0, ymax=quantile25),
                data=quartiles)+
    geom_ribbon(aes(day.POSIXct, ymin=quantile75, ymax=quantile100),
                data=quartiles)+
    geom_line(aes(day.POSIXct, quantile50),
              data=quartiles),
  selector.types=list(day="multiple"),
  oneDay=ggplot()+
    ggtitle("Details of selected days")+
    theme_bw()+
    theme_animint(width=1000)+
    coord_cartesian(xlim=c(0, 26))+
    scale_x_continuous("hours after midnight", breaks=0:24)+
    ylab("temperature (degrees C)")+
    geom_text(aes(hours.after.midnight, degrees.C,
                  label=day,
                  clickSelects=day,
                  showSelected=day),
              hjust=0,
              data=last.times)+
    geom_line(aes(hours.after.midnight, degrees.C,
                  showSelected=day,
                  group=day,
                  clickSelects=day),
              size=4,
              alpha=0.5,
              data=temperature))

animint2dir(viz, "figure-timeseries")

