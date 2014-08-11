# Reproducible Research: Peer Assessment 1

download  data from:
  [http://github.com/mdrzazga/RepData_PeerAssessment1/raw/master/activity.zip]

## Loading and preprocessing the data

```r
activity.z <- unz("activity.zip","activity.csv")
setAs("character","myDate", function(from) as.Date(from, format="%Y-%m-%d") )
```

```
## in method for 'coerce' with signature '"character","myDate"': no definition for class "myDate"
```

```r
activity <- read.csv(activity.z, colClasses=c("integer","myDate","integer"), na.strings="NA")
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


## What is mean total number of steps taken per day?


```r
activity.day <- tapply(activity$steps, activity$date, sum)
hist(activity.day)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
summary(activity.day)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8840   10800   10800   13300   21200       8
```

```r
avr <- mean(activity.day,na.rm=T)
med <- median(activity.day,na.rm=T)
```
more precisely: mean = 1.0766 &times; 10<sup>4</sup> and mediana = 10765
## What is the average daily activity pattern?

```r
activity.interval <- aggregate(activity$steps, list(activity$interval), mean, na.rm=T)
plot( activity.interval$Group.1, activity.interval$x, type="l", xlab="interval", ylab="average number of steps taken")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
maxStepsInterval <- activity.interval[which.max(activity.interval$x),1]
```
We have maximum at 835 interval .

## Imputing missing values

```r
cc <- complete.cases(activity)
```
There are 2304 missing values in the dataset (i.e. the total number of rows with NAs).
We will update missing value using mean for that 5-minute interval from active.interval and stor it in activity.clean dataframe.

```r
activity.clean <- activity
for(i in 1: nrow(activity.clean)){ 
  if(is.na(activity.clean[i,1])){
    activity.clean[i,1]<- activity.interval[activity.interval$Group.1==activity[i,3],2]
    }
  }
str(activity.clean)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
sum(!complete.cases(activity.clean))
```

```
## [1] 0
```

```r
activity.clean.day <- tapply(activity.clean$steps, activity.clean$date, sum)
hist(activity.clean.day)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

```r
summary(activity.clean.day)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9820   10800   10800   12800   21200
```


## Are there differences in activity patterns between weekdays and weekends?

```r
# for international issue
Sys.setlocale("LC_TIME","en_GB.UTF-8")
```

```
## [1] "en_GB.UTF-8"
```

```r
library(lattice)
activity$dayclass <- ifelse(weekdays(activity$date) %in% c("Saturday","Sunday"), "weekend", "weekday")
activity.interval4dayclass <-aggregate(list(steps=activity$steps), list(dayclass = activity$dayclass, interval = activity$interval), mean, na.rm=T)

xyplot(steps ~ interval | dayclass, data = activity.interval4dayclass, layout = c(1, 2), type="l", ylab="Number of steps")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 
