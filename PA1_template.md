Peer Assessment 1
========================================================

## Loading and preprocessing the data

Load the data

```r
df <- read.csv("activity.csv")
```

Process/transform the data into a format suitable for your analysis

```r
Sys.setlocale("LC_TIME", "English")
```

```
## [1] "English_United States.1252"
```

```r
df$date <- as.Date(df$date)
```

## What is mean total number of steps taken per day?
Calculate the total number of steps taken per day

```r
step_day <- tapply(df$steps, df$date, sum)
```

Make a histogram of the total number of steps taken each day

```r
hist(step_day, breaks=50)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

Calculate and report the mean and median of the total number of steps taken per day

```r
mean(step_day, na.rm = T)
```

```
## [1] 10766.19
```

```r
median(step_day, na.rm = T)
```

```
## [1] 10765
```

# What is the average daily activity pattern?
Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
mean_interval <- tapply(df$steps, df$interval, mean, na.rm = T)
plot(names(mean_interval), mean_interval, type="l", xlab="5-minute interval", ylab="Average number of steps taken, averaged across all days")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
names(which.max(mean_interval))
```

```
## [1] "835"
```

## Imputing missing values
Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(df$steps))
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. 
The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc. 
Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
for (i in 1:length(df$steps)) {
    if(is.na(df$steps[i])) {
        interval <- as.character(df$interval[i])
        df$steps[i] <- mean_interval[interval]
    }
}
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 
Do these values differ from the estimates from the first part of the assignment? 
What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
step_day_mod <- tapply(df$steps, df$date, sum)
hist(step_day_mod, breaks=50)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

```r
mean(step_day_mod)
```

```
## [1] 10766.19
```

```r
median(step_day_mod)
```

```
## [1] 10766.19
```

# Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
df$wend <- as.factor(ifelse(weekdays(df$date) %in% c("Saturday", "Sunday"), "weekend", "weekday"))
tapply(df$steps, df$wend, mean)
```

```
##  weekday  weekend 
## 35.61058 42.36640
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and 
the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
mean_interval_mod <- tapply(df$steps, df$interval, mean)

library(plyr)
data <- ddply(df, .(interval, wend), summarise, mean=mean(steps))

library(lattice)
xyplot(mean ~ interval | wend, data=data, type="l", ylab = "Number of steps", layout=c(1,2))
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 
