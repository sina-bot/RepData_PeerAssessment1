---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

Load respective R-packages.


```r
library(dplyr)
library(ggplot2)
```

## Loading and preprocessing the data
First, the data are loaded from the same gitHub repository.  
The csv-file is unzipped and read.

```r
activity <- read.csv(unz("activity.zip", "activity.csv"), 
                     header = TRUE, sep = ",", na.strings = "NA", stringsAsFactors = FALSE)
```

Second, the variable 'date' is coerced to the format 'date'.

```r
activity$date <- as.Date(activity$date, "%Y-%m-%d")
```
The resulting data frame looks like:

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## What is mean total number of steps taken per day?
*For the following calculations, missing values are ignored.*  

1. Calculate the total number of steps taken per day

```r
activityDay <- activity %>% group_by(date) %>% 
                summarise(steps = sum(steps, na.rm = TRUE))
```

2. Make a histogram of the total number of steps taken each day

```r
hist(activityDay$steps, col = "green", 
     main = "Total Number of Steps per Day", xlab = "Steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day

```r
activityMean <- round(mean(activityDay$steps))
activityMedian <- format(round(median(activityDay$steps)), scientific = FALSE)
```
  
  
**Mean of the total number of steps: 9354**  
**Median of the total number of steps: 10395**  
  
  
## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)  

First, calculate the average number of steps grouped by intervals  

```r
activityPattern <- activity %>% group_by(interval) %>% 
                    summarise(steps = round(mean(steps, na.rm = TRUE)))
```

Second, plot a line-diagram for visualizing the average activity pattern.  

```r
with(activityPattern, plot(interval, steps, type = "l", lty = 1, xaxt = "n", 
                           main = "Average Daily Activity Pattern", xlab = "Daytime", ylab = "Number of Steps"))
axis(side = 1, at = activityPattern$interval)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

2. Calculate, which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?  



```r
activityMax <- max(activityPattern$steps)
intervalMax <- format(subset(activityPattern$interval, 
                             activityPattern$steps == activityMax), scientific = FALSE)
```
  
    
**The 5-minute interval 835 contains on average the maximung number of steps (206).**  
  
    
## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)  


```r
activityNA <- format(sum(!complete.cases(activity)), scientific = FALSE)
print(paste("There are", activityNA, "rows with NA."))
```

```
## [1] "There are 2304 rows with NA."
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.  
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

Calcualte the mean steps of every interval and impute the missing values  


```r
activityNoNA <- activity %>% group_by(interval) %>% 
                  mutate(steps = ifelse(is.na(steps), mean(steps, na.rm = TRUE), steps))
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?  

First, calculate a new dataframe with imputed missing data

```r
activityNoNADay <- activityNoNA %>% group_by(date) %>% 
                    summarise(steps = sum(steps, na.rm = TRUE), type = c("imputed"))
```
 
Calculate the median and mean values with imputed missing data  

```r
activityMeanImp <- round(mean(activityNoNADay$steps))
activityMedianImp <- format(round(median(activityNoNADay$steps)), scientific = FALSE)
```

Calculate the dataframe with both, values from the first part of the assignment and imputed missing data

```r
activityNoNADay <- activityNoNADay %>% rbind(mutate(activityDay, type = "original"))
```

Second, plot the histograms in multiple panels.

```r
ggplot(activityNoNADay, aes(x=steps)) + 
    geom_histogram(data=subset(activityNoNADay,type == "imputed"),fill = "red", alpha = 0.5) +
    geom_histogram(data=subset(activityNoNADay,type == "original"),fill = "blue", alpha = 0.5) +
    geom_text(x=17000, y=12, color = "blue", alpha = 0.5, label= paste("Mean (original) =", activityMean)) +
    geom_text(x=17000, y=11, color = "red", alpha = 0.5, label= paste("Mean (imputed) =", activityMeanImp)) +
    geom_text(x=17000, y=10, color = "blue", alpha = 0.5, label= paste("Median (original) =", activityMedian)) +
    geom_text(x=17000, y=9, color = "red", alpha = 0.5, label= paste("Median (imputed) =", activityMedianImp))
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

**The mean and median values are higher when missing values are imputed.**  


## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.  

```r
activityNoNA <- activityNoNA %>% mutate(daytype = weekdays(date))
activityNoNA <- activityNoNA %>% mutate(daytype = ifelse(daytype == "Samstag", "weekend",
                                        ifelse(daytype == "Sonntag", "weekend", "weekday")))
```
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.  

First, calculate the average number of steps grouped by intervals  

```r
activityPatternNoNA <- activityNoNA %>% group_by(daytype, interval) %>% 
                        summarise(steps = round(mean(steps, na.rm = TRUE)))
```
Second, plot the line chart  

```r
ggplot(activityPatternNoNA, aes(interval, steps, group= daytype)) + 
  geom_line() + 
  facet_wrap(~daytype, ncol=1)
```

![](PA1_template_files/figure-html/unnamed-chunk-19-1.png)<!-- -->
