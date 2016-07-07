works_with_R("3.2.3", data.table="1.9.7")

bids <- fread("bid_data.txt")
bids[, time.POSIXct := as.POSIXct(strptime(time, "%Y-%m-%d %H:%M:%S"))]
bids[, date.str := strftime(time.POSIXct, "%Y-%m-%d")]
bids[, table(date.str)]
one.day <- bids[date.str=="2014-06-26", ]
one.day[, table(age, gender)]
bids[date.str=="2014-06-26" & age==13, ]
table(bids$gender)
table(bids$age)
bids[, table(gender, age)]
length(unique(bids$time))

male13 <- bids[age==13 & gender=="male",]
bids[time%in%unique(time)[1:2] & age==13,]

ggplot()+
  geom_ribbon(aes(time.POSIXct, ymin=cpm_min, ymax=cpm_max),
              data=male13,
              alpha=0.5)+
  geom_line(aes(time.POSIXct, cpm_median),
            data=male13)

only13 <- bids[age==13,]
ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(gender ~ .)+
  geom_ribbon(aes(time.POSIXct, ymin=cpm_min, ymax=cpm_max),
              data=only13,
              alpha=0.5)+
  geom_line(aes(time.POSIXct, cpm_median),
            data=only13)

ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(age ~ gender)+
  geom_ribbon(aes(time.POSIXct, ymin=cpm_min, ymax=cpm_max),
              data=one.day,
              alpha=0.5)+
  geom_line(aes(time.POSIXct, cpm_median),
            data=one.day)

ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(time ~ gender)+
  geom_ribbon(aes(age, ymin=cpm_min, ymax=cpm_max),
              data=one.day,
              alpha=0.5)+
  geom_line(aes(age, cpm_median),
            data=one.day)

bids[, year := as.numeric(strftime(time.POSIXct, "%Y"))]
bids[, week := as.numeric(strftime(time.POSIXct, "%V"))]
bids[, table(sign(diff(time.POSIXct)))]
bids[, table(sign(diff(week)))]
bids[year==2014 & week==1, week := 53] #TODO:generalize
bids[, year.week := year + week/52]
seconds.per.day <- 60 * 60 * 24
week.first <- bids[, list(
  time.POSIXct=.SD[weekday=="Wednesday", time.POSIXct][1]),
                        by=year.week]
week.first[, year.week.str :=  strftime(time.POSIXct, "%Y-%m-%d")]
week.first[, year.week.POSIXct := as.POSIXct(strptime(time.POSIXct, "%Y-%m-%d"))]
setkey(week.first, year.week)
bids[, table(year.week)]
bids[, plot(time.POSIXct, year.week)]
bids[year==2014 & week == 1,]
bids[year==2014 & week == 52,]
bids[year==2015 & week == 1,]
weekday.levs <-
  c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
    "Saturday", "Sunday")
bids[, weekday.str := strftime(time.POSIXct, "%A")]
bids[, weekday := factor(weekday.str, weekday.levs)]
bids[, date.POSIXct := as.POSIXct(strptime(date.str, "%Y-%m-%d"))]

days <- bids[, list(
  total.variation=sum(cpm_max-cpm_min),
  total.displacement=sum(abs(diff(cpm_median))),
  cpm_median=as.numeric(median(cpm_median)),
  cpm_min=min(cpm_min),
  cpm_max=max(cpm_max)),
             by=.(gender, age, year.week, weekday, date.str, date.POSIXct)]
setkey(days, year.week)
days.ann <- days[week.first]
days.ann[, month.str := strftime(date.POSIXct, "%m")]
fem13 <- days.ann[gender=="female" & age==13,]
first.days <- fem13[grepl("-01$", date.str),]
days.stats <- days.ann[, list(
  cpm_median=median(cpm_median),
  mean.total.displacement=mean(total.displacement),
  mean.total.variation=mean(total.variation)
  ), by=.(year.week.POSIXct, weekday, date.POSIXct, date.str)]

ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(. ~ gender)+
  scale_fill_gradient(low="white", high=scales::muted("red"))+
  geom_point(aes(total.variation, total.displacement, fill=age),
             shape=21,
             color="black",
             data=days[date.str==date.str[1],])

ages <- data.table(age=unique(days$age))


bids[, hours.num := as.numeric(strftime(time.POSIXct, "%H"))]
bids[, minutes.num := as.numeric(strftime(time.POSIXct, "%M"))]
bids[, hours.past.midnight := hours.num + minutes.num/60]

viz <- list(
  dayTiles=ggplot()+
    theme_bw()+
    theme_animint(width=1000, height=200)+
    ggtitle("Calendar, select day")+
    geom_tile(aes(year.week.POSIXct, weekday, fill=cpm_median,
                  clickSelects=date.str),
              data=days.stats)+
    scale_fill_gradient(low="white", high="black")+
    ## geom_point(aes(year.week.POSIXct, weekday),
    ##            shape=21,
    ##            fill="white",
    ##            size=12,
    ##            data=first.days)+
    geom_text(aes(year.week.POSIXct, weekday, label=month.str),
              color="orange",
              data=first.days),
    dayScatter=ggplot()+
      ggtitle("Daily variation, select day")+
      theme_bw()+
      theme(panel.margin=grid::unit(0, "lines"))+
      scale_fill_gradient(low="white", high="black")+
      geom_point(aes(mean.total.variation, mean.total.displacement, fill=cpm_median,
                     clickSelects=date.str),
                 size=4,
                 alpha=0.7,
                 shape=21,
                 color="black",
                 data=days.stats),
  oneDay=ggplot()+
    ggtitle("Details for selected day")+
    theme_bw()+
    theme_animint(width=600)+
    theme(panel.margin=grid::unit(0, "lines"))+
    facet_grid(. ~ gender)+
    scale_fill_gradient(low="white", high=scales::muted("red"))+
    geom_point(aes(total.variation, total.displacement, fill=age,
                   clickSelects=age,
                   key=paste(age, gender),
                   showSelected=date.str),
               shape=21,
               alpha=0.7,
               size=4,
               color="black",
               data=days),
  time=list(variable="date.str", ms=1000),
  selectAge=ggplot()+
    ggtitle("Age profile for one day, select age")+
    theme_bw()+
    theme_animint(width=600)+
    theme(panel.margin=grid::unit(0, "lines"))+
    geom_tallrect(aes(xmin=age-0.5, xmax=age+0.5, clickSelects=age),
                  alpha=0.5,
                  data=ages)+
    facet_grid(gender ~ .)+
    geom_ribbon(aes(age, ymin=cpm_min, ymax=cpm_max,
                    key=gender,
                    showSelected=date.str),
                data=days,
                alpha=0.5)+
    geom_line(aes(age, cpm_median,
                  key=gender,
                  showSelected=date.str),
              data=days),
  oneAge=ggplot()+
    ggtitle("Details for one day and one age group")+
    theme_bw()+
    theme_animint(width=600)+
    theme(panel.margin=grid::unit(0, "lines"))+
    facet_grid(gender ~ .)+
    geom_ribbon(aes(hours.past.midnight, ymin=cpm_min, ymax=cpm_max,
                    key=gender,
                    showSelected=date.str,
                    showSelected2=age),
                data=bids,
                alpha=0.5)+
    geom_line(aes(hours.past.midnight, cpm_median,
                  key=gender,
                  showSelected=date.str,
                  showSelected2=age),
              data=bids),
  duration=list(date.str=500))
library(animint)
animint2dir(viz, "figure-bids")

