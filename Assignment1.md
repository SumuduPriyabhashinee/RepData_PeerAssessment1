---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
setwd("D:/DataScience/DataScienceCourse/Reproducable Research")
unzip(zipfile="activity.zip")
dataset <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 4.0.2
```

```r
total.steps <- tapply(dataset$steps, dataset$date, FUN=sum, na.rm=TRUE)
qplot(total.steps, binwidth=1000, xlab="total number of steps per day",main="Histogram of Total Steps by day")
```

![](Assignment1_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
mean(total.steps, na.rm=TRUE)
```

```
## [1] 9354.23
```

```r
median(total.steps, na.rm=TRUE)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

```r
library(ggplot2)
averageact <- aggregate(x=list(steps=dataset$steps), by=list(interval=dataset$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=averageact, aes(x=interval, y=steps)) +
    geom_line() +
    xlab("5-min interval") +
    ylab("average number of steps")
```

![](Assignment1_files/figure-html/unnamed-chunk-2-1.png)<!-- -->
the maximum number of steps

```r
averageact[which.max(averageact$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values

```r
missing <- is.na(dataset$steps)
table(missing)
```

```
## missing
## FALSE  TRUE 
## 15264  2304
```


```r
# Replace missing value with the mean value of 5-minute interval
fill.value <- function(steps, interval) {
    filled <- NA
    if (!is.na(steps))
        filled <- c(steps)
    else
        filled <- (averageact[averageact$interval==interval, "steps"])
    return(filled)
}
filled.data <- dataset
filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)
```
Use the filled data set for make a histogram of the total number of steps per day and calculate the mean and median total number of steps.


```r
total.steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
qplot(total.steps, binwidth=1000, xlab="total number of steps per day")
```

![](Assignment1_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
mean(total.steps)
```

```
## [1] 10766.19
```

```r
median(total.steps)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

The day of the week for each measurement


```r
wkday.or.wkend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
    else
        stop("invalid date")
}
filled.data$date <- as.Date(filled.data$date)
filled.data$day <- sapply(filled.data$date, FUN=wkday.or.wkend)
```

Average number of steps taken on weekdays and weekends


```r
averages <- aggregate(steps ~ interval + day, data=filled.data, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
    xlab("5-minute interval") + ylab("Number of steps")
```

![](Assignment1_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
