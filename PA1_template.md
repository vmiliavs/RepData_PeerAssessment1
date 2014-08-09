# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
unzip("activity.zip")
df <- read.csv("activity.csv")
df$date <- as.Date(as.character(df$date))
```
## What is mean total number of steps taken per day?

```r
agg<-aggregate(df$steps, by = list(df$date), FUN = sum, na.rm=T)
names(agg) <- c("date", "steps")
hist(agg$steps, main = "Histogram of the total number of steps taken each day",
     xlab = "Total number of steps taken each day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
dailyStepsMean <- mean(agg$steps)
dailyStepsMedian <- median(agg$steps)
```
The mean total number of steps taken per day is 9354.2295.  
The median total number of steps taken per day is 10395.  

## What is the average daily activity pattern?

```r
agg2<-aggregate(df$steps, by = list(df$interval), FUN = mean, na.rm=T)
names(agg2) <- c("interval", "meanSteps")
plot(agg2$interval, agg2$meanSteps, type='l',
     main = "The average number of steps taken, averaged across all days",
     xlab = "Interval", ylab = "Number of Steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
maxSteps <- max(agg2$meanSteps)
maxInterval <- agg2[match(maxSteps, agg2$meanSteps), 'interval']
```
The 5-minute interval 835, on average across all the days in the dataset, contains the maximum number of steps (206.1698).  

## Imputing missing values

```r
mis <- is.na(df$steps)
numOfMissingValues <- sum(mis)
```
The total number of missing values in the dataset is 2304.  
Fill all of the missing values in the dataset by the mean for that 5-minute interval.  

```r
df2 <- df
df2[mis, 'steps'] <- agg2[match(df2[mis,'interval'], agg2$interval), 'meanSteps']
agg1<-aggregate(df2$steps, by = list(df2$date), FUN = sum, na.rm=T)
names(agg1) <- c("date", "steps")
hist(agg1$steps, main = "Histogram of the total number of steps taken each day",
     xlab = "Total number of steps taken each day")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

```r
dailyStepsMean <- mean(agg1$steps)
dailyStepsMedian <- median(agg1$steps)
```
After imputing missing values the new results are as following.  
The mean total number of steps taken per day is 1.0766 &times; 10<sup>4</sup>.  
The median total number of steps taken per day is 1.0766 &times; 10<sup>4</sup>.  
The impact of imputing missing data on the estimates of the total daily number of steps is that both mean and median are slightly increasing.  

## Are there differences in activity patterns between weekdays and weekends?

```r
df2$day <- ifelse(weekdays(df2$date, abbreviate = TRUE) %in% c("Sat", "Sun"), "weekend", "weekday")
df2$day <- as.factor(df2$day)
dfWeekDay <- subset(df2, day == "weekday")
dfWeekEnd <- subset(df2, day == "weekend")
aggWeekDay<-aggregate(dfWeekDay$steps, by = list(dfWeekDay$interval), FUN = mean, na.rm=T)
aggWeekEnd<-aggregate(dfWeekEnd$steps, by = list(dfWeekEnd$interval), FUN = mean, na.rm=T)
names(aggWeekDay) <- c("interval", "meanSteps")
names(aggWeekEnd) <- c("interval", "meanSteps")
par(mfcol = c(2, 1))
plot(aggWeekEnd$interval, aggWeekEnd$meanSteps, type='l',
     main = "Weekend",
     xlab = "Interval", ylab = "Number of Steps")
plot(aggWeekDay$interval, aggWeekDay$meanSteps, type='l',
     main = "Weekday",
     xlab = "Interval", ylab = "Number of Steps")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

As can be observed from the above panel plot, there is a difference in activity patterns between weekdays and weekends.  
