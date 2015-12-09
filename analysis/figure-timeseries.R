works_with_R("3.2.2",
             data.table="1.9.6",
             "tdhock/ggplot2@a8b06ddb680acdcdbd927773b1011c562134e4d2",
             "tdhock/animint@7c1d4a7610b4f0769d6df38eccd46f69e24388ac")

load("temperature.RData")

last.times <- temperature[, {
  data.table(hours.after.midnight, data.type="sensor", degrees.C)[.N, ]
}, by=.(day, day.POSIXct)]

quartile.labels <- quartiles[.N, {
  data.table(day.POSIXct,
             data.type="sensor",
             degrees.C=c(
               quantile0, quantile25, quantile50, quantile75, quantile100),
             label=c("min", "25%", "median", "75%", "max"))
}]

full.days <- quartiles[first.time < 1 & 23 < last.time, ]

## http://www.ccohs.ca/oshanswers/phys_agents/thermal_comfort.html
recommendation <- data.table(
  min.C=21,
  max.C=23,
  data.type="recommended")
quartiles[, data.type := "sensor"]
temperature[, data.type := "sensor"]
data.colors <- c(sensor="grey50", recommended="#BEBADA")

viz <- list(
  title=list("Temperature data from Arduino + LM35 sensor"),
  days=ggplot()+
    guides(color="none")+
    scale_color_manual(values=data.colors)+
    scale_fill_manual(values=data.colors)+
    ggtitle("Temperature quartiles, select day")+
    theme_bw()+
    theme_animint(width=1000)+
    xlab("day")+
    ylab("temperature (degrees C)")+
    geom_widerect(aes(ymin=min.C, ymax=max.C, fill=data.type),
                  color=NA,
                  data=recommendation)+
    geom_ribbon(aes(day.POSIXct,
                    fill=data.type,
                    ymin=quantile0, ymax=quantile25),
                color=NA,
                alpha=0.75,
                data=quartiles)+
    geom_ribbon(aes(day.POSIXct,
                    fill=data.type,                    
                    ymin=quantile75, ymax=quantile100),
                color=NA,
                alpha=0.75,
                data=quartiles)+
    geom_line(aes(day.POSIXct, quantile50,
                  showSelected=data.type,
                  color=data.type),
              alpha=0.75,
              data=quartiles)+
    geom_text(aes(day.POSIXct, degrees.C,
                  showSelected=data.type,
                  label=label),
              data=quartile.labels,
              hjust=0)+
    geom_tallrect(aes(xmin=half.before, xmax=half.after,
                      clickSelects=day),
                  alpha=0.5,
                  data=quartiles),
  selector.types=list(day="multiple"),
  first=list(day=full.days[.N, day]),
  oneDay=ggplot()+
    ggtitle("Details of selected days")+
    theme_bw()+
    theme_animint(width=1000)+
    guides(color="none", fill="none")+
    scale_color_manual(values=data.colors)+
    scale_fill_manual(values=data.colors)+
    coord_cartesian(xlim=c(0, 26.2))+
    scale_x_continuous("hours after midnight", breaks=0:24)+
    ylab("temperature (degrees C)")+
    geom_widerect(aes(ymin=min.C, ymax=max.C,
                      showSelected=data.type,
                      fill=data.type),
                  color=NA,
                  data=recommendation)+
    geom_text(aes(hours.after.midnight, degrees.C,
                  showSelected2=data.type,
                  label=day,
                  clickSelects=day,
                  showSelected=day),
              hjust=0,
              data=last.times)+
    geom_line(aes(hours.after.midnight, degrees.C,
                  showSelected2=data.type,
                  showSelected=day,
                  group=day,
                  clickSelects=day),
              size=4,
              alpha=0.5,
              data=temperature))

animint2dir(viz, "figure-timeseries")

