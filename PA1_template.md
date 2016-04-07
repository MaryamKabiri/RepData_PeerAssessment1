Reproducible Research: Course Project 1
=======================================

#Loading and preprocessing the data

1. Load the data (i.e. read.csv())
2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
unzip("repdata-data-activity.zip")
data<-read.table("activity.csv", sep=",",  header=TRUE)
```

#What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day
2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
3. Calculate and report the mean and median of the total number of steps taken per day


```r
TotalstepsPerDay<-tapply(data$steps, data$date, sum)
hist(TotalstepsPerDay, col="blue", xlab="Total number of steps per day", ylab="Frequency", main="Total number of steps per day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)
The mean of the total number of steps taken per day:


```r
mean(TotalstepsPerDay, na.rm=TRUE)
```

```
## [1] 10766.19
```

The median of the total number of steps taken per day:


```r
median(TotalstepsPerDay, na.rm = TRUE)
```

```
## [1] 10765
```

#What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
AverageStepsPerInterval<-aggregate(steps~interval, data, mean)
plot(AverageStepsPerInterval, type="l", xlab="Interval", ylab="Average number of steps", main="Average number of steps average across all days")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

The maximum number of steps in 5-minute interval:


```r
MaxSteps<-which.max(AverageStepsPerInterval$steps)
AverageStepsPerInterval[MaxSteps, ]
```

```
##     interval    steps
## 104      835 206.1698
```

#Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Number of missing values:


```r
sum(is.na(data))
```

```
## [1] 2304
```

The missing data in steps are replaced by the mean for that 5-minutes interval:


```r
for(i in 1:nrow(data)){
  if(is.na(data$steps[i])){
    fill<-AverageStepsPerInterval$steps[which(AverageStepsPerInterval$interval==data$interval[i])]
    data$steps[i]<-fill
  }
}

TotalstepsPerDayFill<-aggregate(steps~date, data, sum)
hist(TotalstepsPerDayFill$steps, col="blue", xlab="Total number of steps per day", ylab="Frequency", main="Total number of steps per day")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

The mean of the total number of steps taken per day (missing replaced by the mean for that 5-minutes interval):


```r
mean(TotalstepsPerDayFill$steps)
```

```
## [1] 10766.19
```

The median of the total number of steps taken per day (missing replaced by the mean for that 5-minutes interval):


```r
median(TotalstepsPerDayFill$steps)
```

```
## [1] 10766.19
```

#Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
WeekDay<-function(DateValuse){
  week_day<-weekdays(as.Date(DateValuse, '%Y-%m-%d'))
  if (!(week_day=='Saturday' || week_day=='Sunday')){
    x<-'Weekdays'
  } else {
      x<-'Weekend'
  }
  x
} 
 
data$day_type<-as.factor(sapply(data$date, WeekDay))
TotalstepsPerDayType<-aggregate(steps~interval+day_type, data, mean)
library(ggplot2)
qplot(interval, steps, data=TotalstepsPerDayType, geom=c("line"), xlab="Interval", ylab="Steps", main="Steps per Interval by Day Type") + facet_wrap(~day_type, ncol=1)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)


