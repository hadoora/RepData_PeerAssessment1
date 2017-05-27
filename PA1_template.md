<!-- rmarkdown v1 -->

---
title: "Peer Review for Reproducible Research"
---



### To read in and preprocess the data:


```r
a<-read.csv('C:\\Program Files\\R\\R-3.3.3\\bin\\x64\\activity.csv')
a$dates<-as.numeric(as.Date(a$date))
mx <- a$dates
my <- a$steps
h <- hist(a$dates, xlab='distribution',ylab='dates', main='this is the date distribution')
```

![plot of chunk input](figure/input-1.png)

###To calculate the total number of steps taken per day:


```r
breaks <- data.frame("beg"=h$breaks[-length(h$breaks)], "end"=h$breaks[-1])
sums <- apply(breaks, MARGIN=1, FUN=function(x) { sum(my[ mx >= x[1] & mx < x[2] ], na.rm = T) })
h$counts <- sums
plot(h, xlab="dates", ylab="Steps", main="total steps by days")
```

![plot of chunk total_steps_per_day](figure/total_steps_per_day-1.png)

```r
b<-tapply(a$steps,a$date,sum)
```

And here we can see the mean and median number of steps taken per day:


```r
summary(b)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8841   10760   10770   13290   21190       8
```

This chunk shows the 5-minute interval and the average number of steps taken, averaged across all days.


```r
c<-tapply(a$steps,a$interval,mean, na.rm=T)
c<-data.frame(interval=as.numeric(names(c)),mean=c)
plot(c$interval,c$mean,type='l', xlab='interval',ylab='average steps', main='Average steps by interval')
```

![plot of chunk average_steps_per_interval](figure/average_steps_per_interval-1.png)


```r
c[c$mean==max(c$mean),]
```

```
##     interval     mean
## 835      835 206.1698
```
We can also see that on average, the 835 interval contains the maximum number of steps.


```r
sum(is.na(a$steps))
```

```
## [1] 2304
```
The total number of missing values in the dataset is 2304.

To fill in all of the missing values in the dataset, i use the impute function from the Hmisc package.


```r
library(Hmisc)
a$steps <- with(a, impute(steps, mean))
```
This has created a new dataset equal to the original dataset but with the missing data filled in.


```r
i <- hist(a$dates, xlab='distribution',ylab='dates', main='this is the date distribution')
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)

Now to calculate the total number of steps taken per day (NA's filled in):

```r
d<-tapply(a$steps,a$date,sum)
nx <- a$dates
ny <- a$steps
breaks <- data.frame("beg"=i$breaks[-length(i$breaks)], "end"=i$breaks[-1])
sums <- apply(breaks, MARGIN=1, FUN=function(x) { sum(ny[ nx >= x[1] & nx < x[2] ], na.rm = T) })
i$counts <- sums
plot(i, xlab="dates", ylab="Steps", main="total steps by days")
```

![plot of chunk total_steps_no_NAs](figure/total_steps_no_NAs-1.png)

```r
summary(d)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10770   10770   12810   21190
```
By virtue of replacing the missing values, there is slight difference in the median value, but since all missing values were replaced by one value (the old mean) this results in zero displacement of the old mean.


Now we create a new factor variable a$day that classifies all dates as weekdays or weekends:

```r
a$day<-weekdays(as.Date(a$date))
a$day<-gsub('Monday','Weekday',a$day)
a$day<-gsub('Tuesday','Weekday',a$day)
a$day<-gsub('Wednesday','Weekday',a$day)
a$day<-gsub('Thursday','Weekday',a$day)
a$day<-gsub('Friday','Weekday',a$day)
a$day<-gsub('Saturday','Weekend',a$day)
a$day<-gsub('Sunday','Weekend',a$day)
```

Then we make a  time series panel plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days:

```r
#split into 2 frames
weekday<-a[a$day=='Weekday',]
weekend<-a[a$day=='Weekend',]

#compute means for both frames
e<-tapply(weekday$steps,weekday$interval,mean)
f<-tapply(weekend$steps,weekend$interval,mean)
e<-data.frame(interval=as.numeric(names(e)),mean=e)
f<-data.frame(interval=as.numeric(names(f)),mean=f)
e$day='Weekday'
f$day='Weekend'

#re-unite both frames
g<-rbind(e,f)

#plot means
xyplot(mean ~ interval | day , data=g , type='l', layout=c(1,2), ylab='average no. of steps')
```

![plot of chunk interval_by_day_class](figure/interval_by_day_class-1.png)
