works_with_R("3.2.2",
             data.table="1.9.6",
             "tdhock/ggplot2@a8b06ddb680acdcdbd927773b1011c562134e4d2",
             "tdhock/animint@7c1d4a7610b4f0769d6df38eccd46f69e24388ac")

load("temperature.RData")

outside[, data.type := "Wunderground"]
temperature[, data.type := "sensor"]

last.times <- temperature[, {
  data.table(hours.after.midnight, data.type, degrees.C)[.N,]
}, by=.(day, day.POSIXct)]

outside.last.times <- outside[, {
  data.table(hour.num, data.type="Wunderground", degrees.C)[.N,]
}, by=.(day, day.POSIXct)]

quartiles[, data.type := "sensor"]
full.days <- quartiles[first.time < 1 & 23 < last.time, ]

quartile.labels <- full.days[.N, {
  data.table(day.POSIXct,
             data.type="sensor",
             degrees.C=c(
               quantile0, quantile25, quantile50, quantile75, quantile100),
             label=c("min", "25%", "median", "75%", "max"))
}]

outside.quartile.labels <- outside.quartiles[.N, {
  data.table(day.POSIXct,
             data.type="Wunderground",
             degrees.C=c(
               quantile0, quantile25, quantile50, quantile75, quantile100),
             label=c("min", "25%", "median", "75%", "max"))
}]

## http://www.ccohs.ca/oshanswers/phys_agents/thermal_comfort.html
recommendation <- data.table(
  min.C=21,
  max.C=23,
  data.type="recommended")
outside.quartiles[, data.type := "Wunderground"]
data.colors <- c(
  sensor="grey50",
  recommended="#BEBADA",
  Wunderground="#8DD3C7")

viz <- list(
  title=list("Temperature in Bioinformatics office and outside in Montreal"),
  days=ggplot()+
    guides(color="none")+
    scale_color_manual(values=data.colors)+
    scale_fill_manual(values=data.colors)+
    ggtitle("Temperature quartiles, select day")+
    theme_bw()+
    theme_animint(width=1000)+
    theme(panel.margin=grid::unit(0, "lines"))+
    facet_grid(location ~ ., scales="free")+
    xlab("day")+
    ylab("temperature (degrees C)")+
    geom_widerect(aes(ymin=min.C, ymax=max.C, fill=data.type),
                  color=NA,
                  data=data.table(recommendation, location="inside"))+
    geom_ribbon(aes(day.POSIXct,
                    fill=data.type,
                    ymin=quantile0, ymax=quantile25),
                color=NA,
                alpha=0.75,
                data=data.table(full.days, location="inside"))+
    geom_ribbon(aes(day.POSIXct,
                    fill=data.type,                    
                    ymin=quantile75, ymax=quantile100),
                color=NA,
                alpha=0.75,
                data=data.table(full.days, location="inside"))+
    geom_line(aes(day.POSIXct, quantile50,
                  showSelected=data.type,
                  color=data.type),
              alpha=0.75,
              data=data.table(full.days, location="inside"))+
    geom_text(aes(day.POSIXct, degrees.C,
                  showSelected=data.type,
                  label=label),
              data=data.table(quartile.labels, location="inside"),
              hjust=0)+
    ## Outside:
    geom_ribbon(aes(day.POSIXct,
                    fill=data.type,
                    ymin=quantile0, ymax=quantile25),
                color=NA,
                alpha=0.75,
                data=data.table(outside.quartiles, location="outside"))+
    geom_ribbon(aes(day.POSIXct,
                    fill=data.type,                    
                    ymin=quantile75, ymax=quantile100),
                color=NA,
                alpha=0.75,
                data=data.table(outside.quartiles, location="outside"))+
    geom_line(aes(day.POSIXct, quantile50,
                  showSelected=data.type,
                  color=data.type),
              alpha=0.75,
              data=data.table(outside.quartiles, location="outside"))+
    geom_text(aes(day.POSIXct, degrees.C,
                  showSelected=data.type,
                  label=label),
              data=data.table(outside.quartile.labels, location="outside"),
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
    theme(panel.margin=grid::unit(0, "lines"))+
    facet_grid(location ~ ., scales="free")+
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
                  data=data.table(recommendation, location="inside"))+
    geom_text(aes(hours.after.midnight, degrees.C,
                  showSelected2=data.type,
                  label=day,
                  key=day,
                  clickSelects=day,
                  showSelected=day),
              hjust=0,
              data=data.table(last.times, location="inside"))+
    geom_line(aes(hours.after.midnight, degrees.C,
                  showSelected2=data.type,
                  showSelected=day,
                  group=day,
                  key=day,
                  clickSelects=day),
              size=4,
              alpha=0.5,
              data=data.table(temperature, location="inside"))+
    ## Outside:
    geom_text(aes(hour.num, degrees.C,
                  showSelected2=data.type,
                  label=day,
                  key=day,
                  clickSelects=day,
                  showSelected=day),
              hjust=0,
              data=data.table(outside.last.times, location="outside"))+
    geom_line(aes(hour.num, degrees.C,
                  color=data.type,
                  key=day,
                  showSelected2=data.type,
                  showSelected=day,
                  group=day,
                  clickSelects=day),
              size=4,
              alpha=0.5,
              data=data.table(outside, location="outside")))

animint2dir(viz, "figure-timeseries")

