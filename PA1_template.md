# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
setwd("~/Documents/Coursera R/5. Reproducible Research/RepData_PeerAssessment1")
dat <- read.csv("activity.csv")
dat <- tbl_df(dat)
```

## What is mean total number of steps taken per day?

```r
gp_dat <- group_by(dat, date)
totalStep <- summarize(gp_dat, total=sum(steps))
mean(totalStep$total, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(totalStep$total, na.rm = TRUE)
```

```
## [1] 10765
```

```r
png("figures/plot1.png", width = 480, height = 480, units = "px")
hist(totalStep$total, xlab = "Total Step per Day", main = "Histogram")
dev.off()
```

```
## quartz_off_screen 
##                 2
```

From the code, we saw that mean of total step per day is 10766.19 and median is
10765. They are almost the same.

## What is the average daily activity pattern?

```r
inter <- group_by(dat, interval)
steps_per_interval <- summarize(inter, avg=mean(steps, na.rm = TRUE))
png("figures/plot2.png", width = 480, height = 480, units = "px")
with(steps_per_interval, plot(interval, avg, type = "l", ylab = "average step",
                              main = "average steps per time interval"))
dev.off()
```

```
## quartz_off_screen 
##                 2
```

```r
which.max(steps_per_interval$avg)
```

```
## [1] 104
```

```r
steps_per_interval[104,]
```

```
## Source: local data frame [1 x 2]
## 
##   interval      avg
##      (int)    (dbl)
## 1      835 206.1698
```

835th interval has steps 206 whichis maximum.

## Imputing missing values

```r
sum(is.na(dat$steps))
```

```
## [1] 2304
```

2304 rows contain missing values.


```r
impute <- dat
for(i in 1:nrow(impute)) {
  if(is.na(impute[i,]$steps)) {
    impute[i,]$steps <- subset(steps_per_interval,interval==impute[i,]$interval)[,2]
  }
}
impute$steps <- as.integer(impute$steps)
newgp <- group_by(impute, date)
newtotal <- summarize(newgp, total=sum(steps))
mean(newtotal$total)
```

```
## [1] 10749.77
```

```r
median(newtotal$total)
```

```
## [1] 10641
```

```r
png("figures/plot3.png", width = 480, height = 480, units = "px")
hist(newtotal$total, xlab = "total steps per day", 
     main = "Histogram of total steps per day")
dev.off()
```

```
## quartz_off_screen 
##                 2
```

We impute missing values by average steps at that interval. After imputing missing
values, new mean is 10749.77, new median is 10641. Not a very big change.

## Are there differences in activity patterns between weekdays and weekends?


```r
dat$date <- as.character(dat$date)
dat$date <- as.POSIXct(dat$date, format="%Y-%m-%d")
week <- mutate(dat, level = weekdays(dat$date))
for(i in 1:nrow(week)) {
  if(week[i,4] == "Saturday") {
    week[i,4] <- "weekend"
  }
  else if(week[i,4] == "Sunday") {
    week[i,4] <- "weekend"
  }
  else {
    week[i,4] <- "weekday"
  }
}

wd <- group_by(week, level, interval)
steps_per_groupInterval <- summarize(wd, avg = mean(steps, na.rm = TRUE))
weekday <- filter(steps_per_groupInterval,level == "weekday")
weekend <- filter(steps_per_groupInterval,level == "weekend")
png("figures/plot4.png", width = 900, height = 480, units = "px")
par(mfrow=c(1,2))
with(weekday,plot(interval,avg,type="l", main = "Weekday"))
with(weekend,plot(interval,avg,type="l", main = "Weekend"))
dev.off()
```

```
## quartz_off_screen 
##                 2
```
