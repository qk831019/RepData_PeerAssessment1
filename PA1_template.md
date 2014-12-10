# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
setwd("C:/Users/ub55123/Desktop/Kaggle/repdata")
library(ggplot2)
data = read.csv("activity.csv")
data$date <- as.Date(data$date)
```

## What is mean total number of steps taken per day?

1. Get total number of steps for every day, ignoring NAs.

```r
total = aggregate(steps~date,data,sum)
```
2. Get mean and median total number of steps taken per day:

```r
meanTotal = mean(total$steps)
medianTotal = median(total$steps)
```
Mean:

```r
print(meanTotal)
```

```
## [1] 10766.19
```
Median

```r
print(medianTotal)
```

```
## [1] 10765
```

3. Histogram of total number of steps per day

```r
g = ggplot(total,aes(steps))+geom_histogram(binwidth = diff(range(total$steps))/30)
print(g)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

## What is the average daily activity pattern?

Time Series average steps plot over daily 


```r
meanInt = aggregate(steps~interval,data,mean)
meanDaily = aggregate(steps~date,data,mean)
print(ggplot(meanInt,aes(interval,steps)) + geom_line())
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

Find Interval has that Maximum number of steps

```r
meanInt$interval[meanInt$steps==max(meanInt$steps)]
```

```
## [1] 835
```

## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

2. Stratergy: Use the mean of each interval to impute missing values and create a new dataset

```r
dataNA = data[is.na(data1$steps),]
dataNA$steps<- NULL
dataAve = data[!(is.na(data$steps)),]
temp = merge(dataNA,meanInt,by="interval",all.x=1)
newData = rbind(temp,dataAve)
newData = newData[order(newData$date,newData$interval),]
```

3.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

From histrogram, after imputation, distribution is not significantly changed.

```r
totalNew = aggregate(steps~date,newData,sum)
g = ggplot(totalNew,aes(steps))+geom_histogram(binwidth = diff(range(total$steps))/30)
print(g)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 

Mean and Median after imputation

```r
meanTotalNew  = mean(totalNew$steps)
medianTotalNew = median(totalNew$steps)
```

Impact:

```r
sum(totalNew$steps) - sum(total$steps)
```

```
## [1] 86129.51
```


## Are there differences in activity patterns between weekdays and weekends?
- Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
head(newData)
```

```
##    interval       date     steps
## 1         0 2012-10-01 1.7169811
## 10        5 2012-10-01 0.3396226
## 17       10 2012-10-01 0.1320755
## 29       15 2012-10-01 0.1509434
## 33       20 2012-10-01 0.0754717
## 45       25 2012-10-01 2.0943396
```

```r
newData$weekdays <- factor(format(newData$date, "%A"))
levels(newData$weekdays)
```

```
## [1] "Friday"    "Monday"    "Saturday"  "Sunday"    "Thursday"  "Tuesday"  
## [7] "Wednesday"
```

```r
levels(newData$weekdays) <- list(weekday = c("Monday", "Tuesday",
                                             "Wednesday", 
                                             "Thursday", "Friday"),
                                 weekend = c("Saturday", "Sunday"))
levels(newData$weekdays)
```

```
## [1] "weekday" "weekend"
```

```r
table(newData$weekdays)
```

```
## 
## weekday weekend 
##   12960    4608
```

-Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
avgSteps <- aggregate(newData$steps, 
                      list(interval = as.numeric(as.character(newData$interval)), 
                           weekdays = newData$weekdays),
                      FUN = "mean")
names(avgSteps)[3] <- "meanOfSteps"
library(lattice)
xyplot(avgSteps$meanOfSteps ~ avgSteps$interval | avgSteps$weekdays, 
       layout = c(1, 2), type = "l", 
       xlab = "Interval", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png) 
