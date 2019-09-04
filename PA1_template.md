---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:lubridate':
## 
##     intersect, setdiff, union
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
library(ggplot2)
library(tidyr)
```

#Loading and preprocessing the data
*Show any code that is needed to*

1. Load the data (i.e. read.csv()

```r
setwd("C:/Users/Ken/OneDrive - Kimpton Hotels & Restaurants/Assignment")
    data<-read.csv("activity.csv")
```

2. Process/transform the data (if necessary) into a format suitable for your analysis
    
    ```r
    data$date<-ymd(data$date) #parse dates
    ```

#What is mean total number of steps taken per day?
*For this part of the assignment, you can ignore the missing values in the dataset.*

1. Calculate the total number of steps taken per day
    
    ```r
    steps.data<-filter(data,!is.na(steps)) #filter out records where steps is NA
    total<-sum(steps.data$steps)
    paste(total, "steps total")
    ```
    
    ```
    ## [1] "570608 steps total"
    ```
2. Make a histogram of the total number of steps taken each day
    
    
    ```r
    steps.data<-filter(data,!is.na(steps)) #filter out records where steps is NA
    #plot histogram - total number of steps taken each day
    ##create steps by day table
    steps.data.by.day<-steps.data %>% group_by(date) %>% summarise(steps=sum(steps))
    ggplot(data = steps.data.by.day) + geom_histogram(aes(x=steps),color="light blue",fill="dark blue",bins=30)
    ```
    
    ![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
3. Calculate and report the mean and median of the total number of steps taken per day

```r
#create steps by day table
steps.data.by.day<-steps.data %>% group_by(date) %>% summarise(steps=sum(steps))
mean.steps<-mean(steps.data.by.day$steps)
median.steps<-median(steps.data.by.day$steps)

paste(mean.steps, "mean steps per day")
```

```
## [1] "10766.1886792453 mean steps per day"
```

```r
paste(median.steps, "median steps per day")
```

```
## [1] "10765 median steps per day"
```

#What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
mean.steps.data<-data %>% #summarize mean steps over intervals
    group_by(interval) %>% 
    summarize(mean.steps=mean(steps,na.rm=TRUE))

#plot
time.series.plot<-ggplot(mean.steps.data,aes(x=interval,y=mean.steps))+ 
    geom_line()
time.series.plot
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
 

```r
mean.steps.data<-data %>% #summarize mean steps over intervals
    group_by(interval) %>% 
    summarize(mean.steps=mean(steps,na.rm=TRUE))

maximum.step.interval<-which.max(mean.steps.data$mean.steps)

paste("Maximum average steps at interval",mean.steps.data$interval[maximum.step.interval])
```

```
## [1] "Maximum average steps at interval 835"
```

#Imputing missing values

*Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.*

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
na.count<-sum(is.na(data$steps))
paste("Number of missing values is",na.count)
```

```
## [1] "Number of missing values is 2304"
```
    
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
## print strategy
    print("strategy is to use the mean of each 5-minute interval to fill missing values in the dataset")
```

```
## [1] "strategy is to use the mean of each 5-minute interval to fill missing values in the dataset"
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
   

```r
## calculate median for each 5-minute interval
    ### calculate mean for each interval removing NA values
        interval.mean<-data %>% select(steps,interval) %>% 
                                group_by(interval) %>% 
                                summarize(mean.steps=mean(steps,na.rm=TRUE))

## fill missing dataset values
    ### join median values
        complete.data<-left_join(data,interval.mean,by=c("interval"))
    ### replace NA values with median values
        complete.data$steps[is.na(complete.data$steps)]<-complete.data$mean.steps[is.na(complete.data$steps)]
    ### remove median column
        complete.data<-select(complete.data,c("steps","date","interval"))
```
  
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
# create histogram of total number of steps taken each day
    ## summarize steps over days
        complete.steps.data.by.day<-complete.data %>% group_by(date) %>% summarize(total.steps.per.day = sum(steps))
    ## create histogram
        day.hist<-ggplot(data = complete.steps.data.by.day) +         
            geom_histogram(aes(x=total.steps.per.day),color="pink",fill="red",bins=30)
        day.hist
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

```r
# activity patter differences weekday vs weekend
    ## weekday histogram
        weekday.day.hist<-ggplot(data = filter(complete.steps.data.by.day,
            wday(date)==2|wday(date)==3|wday(date)==4|wday(date)==5))+         
            geom_histogram(aes(x=total.steps.per.day),color="black", fill="green",bins=30)
        weekday.day.hist
```

![](PA1_template_files/figure-html/unnamed-chunk-12-2.png)<!-- -->

```r
    ## weekend histogram
        weekend.day.hist<-ggplot(data = filter(complete.steps.data.by.day,
            wday(date)==1|wday(date)==6|wday(date)==7))+         
            geom_histogram(aes(x=total.steps.per.day),color="black", fill="orange",bins=30)
        weekend.day.hist
```

![](PA1_template_files/figure-html/unnamed-chunk-12-3.png)<!-- -->

```r
    ## calculate means and medians per day
        weekday.mean<-mean(filter(complete.steps.data.by.day,
            wday(date)==2|wday(date)==3|wday(date)==4|wday(date)==5)$total.steps.per.day)
        weekend.mean<-mean(filter(complete.steps.data.by.day,
            wday(date)==1|wday(date)==6|wday(date)==7)$total.steps.per.day)
        
        weekday.median<-median(filter(complete.steps.data.by.day,
            wday(date)==2|wday(date)==3|wday(date)==4|wday(date)==5)$total.steps.per.day)
        weekend.median<-median(filter(complete.steps.data.by.day,
            wday(date)==1|wday(date)==6|wday(date)==7)$total.steps.per.day)
        
        ### print results
            print(paste("Weekend per day activity is higher with a mean that is",
                        weekend.mean-weekday.mean,"steps higher and a median that is",
                        weekend.median-weekday.median,"steps higher."))
```

```
## [1] "Weekend per day activity is higher with a mean that is 2312.58033542977 steps higher and a median that is 1086.5 steps higher."
```

```r
# calculate and report the mean and median total number of steps taken per day
    complete.mean.steps<-mean(complete.steps.data.by.day$total.steps.per.day)
    complete.median.steps<-mean(complete.steps.data.by.day$total.steps.per.day)

    paste(complete.mean.steps, "mean steps per day. This is a",
          complete.mean.steps-mean.steps,"difference after filling in NA values")
```

```
## [1] "10766.1886792453 mean steps per day. This is a 0 difference after filling in NA values"
```

```r
    paste(complete.median.steps, "median steps per day. This is a", 
          complete.median.steps-median.steps,"difference after filling in NA values")
```

```
## [1] "10766.1886792453 median steps per day. This is a 1.1886792452824 difference after filling in NA values"
```

```r
    # show difference of total steps before and after imputing data
    print(paste(sum(steps.data.by.day$steps),"total steps before imputing data.",
        sum(complete.steps.data.by.day$total.steps.per.day),"total steps after imputing data."))
```

```
## [1] "570608 total steps before imputing data. 656737.509433962 total steps after imputing data."
```

```r
#print results
    print(paste("There is no difference in the mean after imputation.",
        "There is however a difference in the median after imputation.",
        "Imputing missing data has increased total daily number of steps",
        "from",sum(steps.data.by.day$steps),
        "to", sum(complete.steps.data.by.day$total.steps.per.day),"."))
```

```
## [1] "There is no difference in the mean after imputation. There is however a difference in the median after imputation. Imputing missing data has increased total daily number of steps from 570608 to 656737.509433962 ."
```

#Are there differences in activity patterns between weekdays and weekends?

*For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.*

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
complete.data$wday<-"weekday"
complete.data$wday[wday(complete.data$date)==1]<-"weekend" #Sundays
complete.data$wday[wday(complete.data$date)>=6]<-"weekend" #Fridays and Saturdsays
complete.data$wday<-as.factor(complete.data$wday)
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
#new df summarize over wdays and interval 
complete.data.by.wday<-complete.data %>% group_by(wday,interval) %>% summarize(avg.steps = mean(steps))

#plot
panel.plot<-ggplot(complete.data.by.wday, aes(x=interval, y=avg.steps)) + geom_line() +
    facet_grid(wday~.)
panel.plot
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

