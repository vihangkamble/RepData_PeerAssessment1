---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
This is Course Project 1 for Reproducible Research Course. This assignments is post processing activity monitoring data and answering various questions related to the assignments.

First we read the csv file and remove all the rows which have NAs as steps values.

```r
library(dplyr)
activityDataInit <- read.csv("activity.csv")
activityData <- activityDataInit[complete.cases(activityDataInit),]
```

## What is mean total number of steps taken per day?
We use summarize to calculate the total number of steps and average number of steps per day.


```r
totalStepsperDay <- summarise(group_by(activityData, date), totalSteps = sum(steps, na.rm = "TRUE"))
meanStepsperDay <- summarise(group_by(activityData, date), meanSteps = mean(steps, na.rm = "TRUE"))
```
we plot the histogram of the total steps taken per day and also calculate the mean and meadian of the total number of steps taken per day.

```r
hist(totalStepsperDay$totalSteps, breaks = 10)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
meanTotalSteps <- mean(totalStepsperDay$totalSteps)
medianTotalSteps <- median(totalStepsperDay$totalSteps)
```


```
## [1] "Mean number of steps taken per Day is"
## [2] "10766.1886792453"
```

```
## [1] "Median number of steps taken per Day is"
## [2] "10765"
```

## What is the average daily activity pattern?

We calculate the mean and the total number of steps per interval and also find out the interval which has the maximum total steps.We also plot a time series of the average number of steps per interval.

```r
meanStepsperInterval <- summarise(group_by(activityData, interval),meanstepsInterval = mean(steps, na.rm = "TRUE"))
totalStepsperInterval <- summarise(group_by(activityData, interval), stepsInterval = sum(steps, na.rm = "TRUE"))
stepsHighestInterval <- totalStepsperInterval[which(totalStepsperInterval$stepsInterval == max(totalStepsperInterval)),]
plot(meanStepsperInterval$interval, meanStepsperInterval$meanstepsInterval , type = "l", xlab = "Interval", ylab = "Average number of Steps" )
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 


## Imputing missing values

In this part we calculate the total number of missing values in the dateset. All the missing values we replace with the average steps taken per interval. We then calculate the mean and median for the new data frame and also plot histogram for the total steps taken per day.


```r
NAcount <- sum(is.na(activityDataInit$steps))
meanStepInterval <- mean(activityData$steps, na.rm = "TRUE")
activityDataImp <- transform(activityDataInit, newSteps = ifelse(is.na(steps),meanStepInterval , steps))
totalStepsperDay <- summarise(group_by(activityDataImp, date), totalSteps = sum(newSteps, na.rm = "TRUE"))

hist(totalStepsperDay$totalSteps, breaks = 10)
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

```r
meanTotalStepsImp <- mean(totalStepsperDay$totalSteps)
medianTotalStepsImp <- median(totalStepsperDay$totalSteps)
```


```
## [1] "Total number of rows with missing values are "
## [2] "2304"
```

```
## [1] "Mean number of steps taken per Day is"
## [2] "10766.1886792453"
```

```
## [1] "Median number of steps taken per Day is"
## [2] "10766.1886792453"
```

## Are there differences in activity patterns between weekdays and weekends?
We create a new factor variable weekday/weekend indicating wheather the given day is weekday or weekend.

```r
activityDataWeekday <- transform(activityDataImp, Day = ifelse(weekdays(as.Date(activityDataImp$date)) == "Saturday" | weekdays(as.Date(activityDataImp$date)) == "Sunday", "Weekend","Weekday"))  
```
