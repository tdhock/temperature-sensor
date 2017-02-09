works_with_R("3.2.3",
             gistr="0.3.6",
             "faizan-khan-iit/ggplot2@5fb99d0cece13239bbbc09c6b8a7da7f86ac58e2",
             "tdhock/animint@c0db9f34c525bec35c797ccdf8be9564b67c578c",
             ## "hadley/scales@2c3edf45de56d617444dc38e47e0404173817886",
             ## "tdhock/ggplot2@a8b06ddb680acdcdbd927773b1011c562134e4d2",
             ## "tdhock/animint@6b1c9e588b03f632cd39cdec9bbcfa730db9e889",
             data.table="1.9.7")

load("temperature.RData")

outside[, data.type := "Wunderground"]
temperature[, data.type := paste("sensor", work.status)]

last.times <- temperature[, {
  data.table(hours.after.midnight, data.type, degrees.C)[.N,]
}, by=.(day, day.POSIXct)]

outside.last.times <- outside[, {
  data.table(hour.num, data.type="Wunderground", degrees.C)[.N,]
}, by=.(day, day.POSIXct)]

quartiles[, data.type := paste("sensor", work.status)]
full.days <- quartiles[first.time < 1 & 23 < last.time, ]
full.days[, location := "inside" ]

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
  "sensor work day"="grey40",
  "sensor day off"="grey60",
  "sensor"="grey50",
  recommended="#BEBADA",
  Wunderground="#8DD3C7")

min.hour <- 9
max.hour <- 17
work.hours <- data.table(
  min.hour,
  max.hour)
work.labels <- data.table(
  hour=c(min.hour, max.hour),
  hjust=c(1, 0),
  label=paste("work", c("starts", "ends")))

outside.hlines <- data.table(degreesC=0, location="outside")

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
    geom_hline(aes(yintercept=degreesC), data=outside.hlines, color="grey")+
    geom_widerect(aes(ymin=min.C, ymax=max.C, fill=data.type),
                  color=NA,
                  data=data.table(recommendation, location="inside"))+
    geom_rect(aes(xmin=half.before, xmax=half.after,
                  fill=data.type,
                  ymin=quantile0, ymax=quantile25),
              alpha=0.75,
              color=NA,
              data=full.days[quantile0 != quantile25, ])+
    geom_rect(aes(xmin=half.before, xmax=half.after,
                  fill=data.type,
                  ymin=quantile75, ymax=quantile100),
              alpha=0.75,
              color=NA,
              data=full.days[quantile75 != quantile100, ])+
    geom_point(aes(day.POSIXct, quantile50,
                   showSelected=data.type,
                   fill=data.type),
               alpha=0.75,
               shape=21,
               color=NA,
               data=full.days)+
    ## geom_ribbon(aes(day.POSIXct,
    ##                 fill=data.type,
    ##                 ymin=quantile0, ymax=quantile25),
    ##             color=NA,
    ##             alpha=0.75,
    ##             data=data.table(full.days, location="inside"))+
    ## geom_ribbon(aes(day.POSIXct,
    ##                 fill=data.type,                    
    ##                 ymin=quantile75, ymax=quantile100),
    ##             color=NA,
    ##             alpha=0.75,
    ##             data=data.table(full.days, location="inside"))+
    ## geom_line(aes(day.POSIXct, quantile50,
    ##               showSelected=data.type,
    ##               color=data.type),
    ##           alpha=0.75,
    ##           data=data.table(full.days, location="inside"))+
    ## geom_text(aes(day.POSIXct, degrees.C,
    ##               showSelected=data.type,
    ##               label=label),
    ##           data=data.table(quartile.labels, location="inside"),
    ##           hjust=0)+
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
    ## geom_text(aes(day.POSIXct, degrees.C,
    ##               showSelected=data.type,
    ##               label=label),
    ##           data=data.table(outside.quartile.labels, location="outside"),
    ##           hjust=0)+
    geom_tallrect(aes(xmin=half.before, xmax=half.after,
                      clickSelects=day),
                  alpha=0.5,
                  data=quartiles),
  selector.types=list(day="multiple"),
  first=list(day=full.days[.N, day]),
  oneDay=ggplot()+
    geom_hline(aes(yintercept=degreesC), data=outside.hlines, color="grey")+
    geom_widerect(aes(ymin=min.C, ymax=max.C,
                      showSelected=data.type,
                      fill=data.type),
                  color=NA,
                  data=data.table(recommendation, location="inside"))+
    geom_tallrect(aes(xmin=min.hour, xmax=max.hour),
                  alpha=0.25,
                  data=work.hours)+
    geom_text(aes(hour, 21, hjust=hjust, label=label),
              data=data.table(work.labels, location="inside"))+
    ggtitle("Details of selected days")+
    theme_bw()+
    theme_animint(width=1150)+
    theme(panel.margin=grid::unit(0, "lines"))+
    facet_grid(location ~ ., scales="free")+
    guides(color="none", fill="none")+
    scale_color_manual(values=data.colors)+
    scale_fill_manual(values=data.colors)+
    coord_cartesian(xlim=c(0, 28))+
    scale_x_continuous("hours after midnight", breaks=0:24)+
    ylab("temperature (degrees C)")+
    geom_text(aes(hours.after.midnight, degrees.C,
                  showSelected2=data.type,
                  label=day,
                  key=day,
                  clickSelects=day,
                  showSelected=day),
              hjust=0,
              data=data.table(last.times, location="inside"))+
    geom_line(aes(hours.after.midnight, degrees.C,
                  color=data.type,
                  showSelected2=data.type,
                  showSelected=day,
                  group=day,
                  key=day,
                  clickSelects=day),
              size=4,
              alpha=0.75,
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
seg.if.equal <- list(
  paste0("quantile", c(0, 25)),
  paste0("quantile", c(75, 100)))
for(two.cols in seg.if.equal){
  col1 <- two.cols[[1]]
  col2 <- two.cols[[2]]
  vec1 <- full.days[[col1]]
  vec2 <- full.days[[col2]]
  is.same <- vec1 == vec2
  if(any(is.same)){
    sub.dt <- full.days[is.same, ]
    viz$days <- viz$days +
      geom_segment(aes_string(
        x="half.before", y=col1, 
        color="data.type",
        showSelected="data.type",
        xend="half.after", yend=col1),
                   alpha=0.75,
                   data=sub.dt)
  }
}

animint2dir(viz, "figure-timeseries")

