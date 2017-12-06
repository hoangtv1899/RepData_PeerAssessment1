---
title: 'Reproducible Research: Peer Assessment 1'
output:
  html_document:
    keep_md: yes
  pdf_document: default
  word_document: default
---

##Loading necessary libraries

```r
library(ggplot2)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(scales)
```
## Loading and preprocessing the data

```r
if (!file.exists("activity.csv")) {
  unzip("activity.zip")
}

activity <- read.csv("activity.csv")

totalStep <- activity %>% group_by(date) %>% 
              summarize(totalStep = sum(steps, na.rm=FALSE))
head(totalStep)
```

```
## # A tibble: 6 x 2
##         date totalStep
##       <fctr>     <int>
## 1 2012-10-01        NA
## 2 2012-10-02       126
## 3 2012-10-03     11352
## 4 2012-10-04     12116
## 5 2012-10-05     13294
## 6 2012-10-06     15420
```
###Here is the distribution of total number of steps taken per day

```r
g<-ggplot(totalStep,aes(totalStep))+geom_histogram()+ylab("Number of Days")+
    xlab("Total Number of Steps per Day")
g
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->


## What is mean total number of steps taken per day?
###the mean

```r
mean(totalStep$totalStep,na.rm=TRUE)
```

```
## [1] 10766.19
```
###the median

```r
median(totalStep$totalStep,na.rm=TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?


```r
intervalStep <- activity %>% group_by(interval) %>%
                    summarize(avgStep=mean(steps, na.rm=TRUE))
g1<-ggplot(intervalStep,aes(x=interval,y=avgStep))+geom_line()+ylab("Average Number of Days")+
    xlab("Time of Day")+
      scale_x_discrete(labels="")
g1
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


## Inputing missing values
###Total number of missing values:

```r
sum(is.na(activity))
```

```
## [1] 2304
```
###We can fill missing data by the mean number of steps for the given interval and replot the histogram


```r
imputedActivity <- inner_join(activity,
                              intervalStep,
                              by="interval") %>%
                    mutate(steps=ifelse(is.na(steps),avgStep,steps)) %>%
                      select(date,interval,steps)
imputedActivity$date <- as.Date(imputedActivity$date)

totalStepImputed <- imputedActivity %>%
                    group_by(date) %>%
                    summarize(totalStep=sum(steps,na.rm=FALSE))

g2<-ggplot(totalStepImputed,aes(totalStep))+geom_histogram()+ylab("Number of Days")+
    xlab("Total Number of Steps per Day")
g2
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->


## Are there differences in activity patterns between weekdays and weekends?


```r
imputedActivity <- imputedActivity %>%
    mutate(weekendIndicator =
               as.factor(ifelse(weekdays(date) %in% c("Saturday","Sunday"),
                                     "weekend","weekday")))

activityPattern <- imputedActivity %>%
                    group_by(weekendIndicator,interval) %>%
                      summarize(avgStep=mean(steps))
```
###Plot the average daily activity

```r
ggplot(activityPattern, aes(strptime(sprintf("%04d", interval), format="%H%M"),
                             avgStep)) +
    geom_line() +
    facet_wrap(~ weekendIndicator, ncol=1) +
    xlab("Time of Day") +
    scale_x_datetime(labels = date_format("%H:%M"), 
                     breaks = date_breaks("2 hour")) +
    scale_y_continuous("Average Number of Steps") +
    theme(plot.title = element_text(vjust=2))
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->


###The following conclusions can be drawn from this plot:
* People seem to be active earlier during weekday days vs. weekend days
* The activity is quite intense around 8:30AM in the weekday days (going to work)
* The activity is more spread out during the weekend days (less sedentarity)

