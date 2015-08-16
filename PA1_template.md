# Reproducible Research: Peer Assessment 1
  
Load needed libraries
*dplyr
*lattice

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.1.3
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
library(lattice)
```
  
## Loading and preprocessing the data
Unzip activity source .csv file and read it into the activity data frame.  
Ensure the second column is treated as a Date type going forward.

```r
activity_data <- read.csv("activity.csv", header=TRUE, colClasses=c(NA,"Date",NA))
str(activity_data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
  
## What is mean total number of steps taken per day?
Show the step frequency counts of total steps by date.

```r
summary_by_date <- activity_data %>% 
    group_by(date) %>%
    summarize(totalSteps = sum(steps,na.rm=TRUE)) 

hist(summary_by_date$totalSteps, main = "Histogram of Total Steps", xlab="Total Steps", breaks=10)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 
  
The mean and median of the total steps are:

```r
mean(summary_by_date$totalSteps)
```

```
## [1] 9354.23
```

```r
median(summary_by_date$totalSteps)
```

```
## [1] 10395
```
  
## What is the average daily activity pattern?

```r
average_by_5min_interval <- activity_data %>% 
    group_by(interval) %>%
    summarize(meanSteps = mean(steps,na.rm=TRUE)) 

plot(average_by_5min_interval, type="l", xlab= "Interval", ylab= "Average # of Steps", col="green" , lwd=2)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 
  
Determine which 5 minute interval has the maximum steps on average.

```r
average_by_5min_interval %>% filter(meanSteps == max(meanSteps))
```

```
## Source: local data frame [1 x 2]
## 
##   interval meanSteps
## 1      835  206.1698
```
  
## Imputing missing values  
  
The total number of missing values in the source dataset is:

```r
nrow(activity_data[is.na(activity_data$steps),])
```

```
## [1] 2304
```
  
Impute missing values from the mean # of steps for that 5 minute interval.
A re-sort of the data is needed after the merge.

```r
filled_activity_data <- merge(activity_data, average_by_5min_interval, by="interval")
filled_activity_data <- filled_activity_data %>% 
    mutate(steps = ifelse(is.na(steps), meanSteps, steps) ) %>% 
    select(steps,date,interval) %>%
    arrange(date,interval)
```
  
Show the step frequency counts by date with imputed values

```r
filled_sum_steps_by_date <- filled_activity_data %>% 
    group_by(date) %>%
    summarize(totalSteps = sum(steps,na.rm=TRUE)) 

hist(filled_sum_steps_by_date$totalSteps, main = "Histogram of Total Steps (Imputed)", xlab="Total Steps", breaks=10)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 
  

```r
mean(filled_sum_steps_by_date$totalSteps)
```

```
## [1] 10766.19
```

```r
median(filled_sum_steps_by_date$totalSteps)
```

```
## [1] 10766.19
```
The impact of using imputed values has increased the average number of steps per day as well as the median. 
  
## Are there differences in activity patterns between weekdays and weekends?
First we add a new factor for whether the date is on a weekday or a weekend.
Then we'll compare the two date types using a panel time series plot of average # of steps taken by 5 minute interval.

```r
weekday_activity_data <- mutate(filled_activity_data, 
     weekdayType = ifelse(weekdays(filled_activity_data$date) %in% c("Saturday","Sunday"),
                           "weekend", "weekday")
                          )

average_by_5min_interval_with_weekday <- weekday_activity_data %>% 
    group_by(interval, weekdayType) %>%
    summarize(averageSteps = mean(steps,na.rm=TRUE)) 

xyplot(averageSteps ~ interval | weekdayType,
                data = average_by_5min_interval_with_weekday,
                type = "l",
                lty = c(1, 2, 2, 1),
                lwd = c(1, 1, 1, 3),
                col.line = c(rep("black",3), "red"))
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 
  
From the chart we can see that on weekdays most steps are taken in the morning around 8:00am,  
while on weekends the steps are spread out across the daytime.

