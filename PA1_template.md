# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data


```r
activity <- read.csv("activity.csv", colClasses = c("numeric", "Date", "numeric"))
```

## What is mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day


```r
daily_steps <- rowsum(activity$steps, activity$date, na.rm=TRUE)
hist(daily_steps, breaks=10, main="Total Daily Steps", col="orange", xlab="Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

2. Calculate and report the mean and median total number of steps taken per day


```r
mean_daily_steps <- mean(daily_steps, na.rm=TRUE)
median_daily_steps <- median(daily_steps, na.rm=TRUE)
```

The **mean** number of steps taken each day is **9354**  
The **median** number of steps taken each day is **10395**

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, # averaged across all days (y-axis)


```r
mean_steps_by_interval <- aggregate(steps ~ interval, data=activity, mean)
plot(mean_steps_by_interval, type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
max_row <- which.max(mean_steps_by_interval$steps)
max_interval <- mean_steps_by_interval[max_row,]$interval
```

Interval **835** has the highest number of steps across all days.

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
missing_row_count <- sum(!complete.cases(activity))
```

There are **2304** missing values in the dataset.

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

I have decided to fill in the missing values with the mean number of steps for that interval.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
library(plyr)
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
complete_activity <- ddply(activity, ~ interval, transform, steps = impute.mean(steps))
```

3. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
complete_daily_steps <- rowsum(complete_activity$steps, complete_activity$date, na.rm=TRUE)
hist(complete_daily_steps, breaks=10, main="Total Daily Steps (Imputed)", col="blue", xlab="Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

```r
mean_complete_daily_steps <- mean(complete_daily_steps)
median_complete_daily_steps <- median(complete_daily_steps)
```

The **mean** and **median** values for the imputed data do differ from the original values.  

The **mean** number of steps taken each day is **10766** compared to **9354** in the original data.     
The **median** number of steps taken each day is **10766** compared to **10395** in the original data.  

Imputing the missing values has increased the mean and the median, as can be seen in the following plot the imputed data has less variance from the mean. 


```r
boxplot(daily_steps[,1], complete_daily_steps[,1],
       main="Daily Steps Comparison",
       names=c("Original", "Imputed"),
       col=c("orange", "blue")
       )
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
complete_activity$day <- ifelse(weekdays(complete_activity$date) %in% c("Saturday", "Sunday"), 
                                'weekend', 'weekday')
complete_activity$day <- as.factor(complete_activity$day)
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data:


```r
library(lattice)
steps_by_day <- aggregate(steps ~ interval * day, data=complete_activity, mean)
xyplot(steps ~ interval | day, data=steps_by_day, type="l", layout=c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 
