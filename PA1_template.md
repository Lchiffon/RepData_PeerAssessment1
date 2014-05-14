PA1_template
========================================================

tips: the ggplot2 package is used for my programming.So ,make sure your R has the ggplot2 package.
## Loading and preprocessing the data


```r
setwd("C:/Users/Administrator/Desktop/repdata_data_activity")
raw.data <- read.csv("activity.csv")
library(ggplot2)
ch_date <- as.character(raw.data$date)
re_date <- strptime(ch_date, "%Y-%m-%d")
raw.data$re_date <- re_date
### change the date variance to the date format
```


----------------------------------------------------------

## What is mean total number of steps taken per day?


```r
##### question1###### What is mean total number of steps taken per day?
level <- levels(raw.data$date)
steps = 0
for (i in 1:length(level)) {
    steps[i] = sum(raw.data$steps[raw.data$date == level[i]], na.rm = T)
}
g1 <- ggplot(, aes(x = steps))
g1 + geom_histogram()
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
paste("The mean of the steps per day is:", mean(steps))
```

```
## [1] "The mean of the steps per day is: 9354.22950819672"
```

```r
paste("The median of the steps per day is:", median(steps))
```

```
## [1] "The median of the steps per day is: 10395"
```

```r
###### end of question1######
```

### The mean of the steps per day is: 9354.22950819672
### The median of the steps per day is: 10395

------------------------------------------------------

## What is the average daily activity pattern?



```r
##### question 2####### What is the average daily activity pattern?
int_level <- int_level <- levels(factor(raw.data$interval))
int_steps <- rep(0, length(int_level))
for (i in 1:length(int_level)) {
    int_steps[i] <- mean(raw.data$steps[raw.data$interval == int_level[i]], 
        na.rm = T)
}
int <- data.frame(level = as.numeric(int_level), steps = int_steps)
g2 <- ggplot(int, mapping = aes(level, steps))
g2 <- g2 + geom_path(col = "blue")
g2
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
ti <- int[int$steps == max(int$steps), 1]
paste(ti, "contains the maximum number of steps")
```

```
## [1] "835 contains the maximum number of steps"
```

```r
### end of question2######
```

### 835 contains the maximum number of steps

-------------------------------------------------------

## Imputing missing values
### I used the mean for 5-minutes interval instead of NA

```r
##### question 3#######
na_num <- sum(is.na(raw.data$steps))
paste("The number of missing value is", na_num)
```

```
## [1] "The number of missing value is 2304"
```

```r
### report the number of missing value

### use mean for that 5-minute interval
new_steps = rep(0, length(raw.data$steps))
for (i in 1:length(raw.data$steps)) {
    if (is.na(raw.data$steps[i]) == T) {
        new_steps[i] <- int$steps[raw.data$interval[i] == int$level]
    } else {
        new_steps[i] <- raw.data$steps[i]
    }
}
new_hist = 0

for (i in 1:length(level)) {
    new_hist[i] = sum(new_steps[raw.data$date == level[i]])
}
g3 <- ggplot(, aes(x = new_hist))
g3 <- g3 + geom_histogram()
g3
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```r

##### end of question 3#######
```

### The number of missing value is 2304
### It is easy to see that impact of imputing missing data on the estimates will increase  the num of zeros of the total steps.

-------------------------------------------------------

## Are there differences in activity patterns between weekdays and weekends?

```r
#### question4#### Are there differences in activity patterns between
#### weekdays and weekends?
weekday <- weekdays(raw.data$re_date)
factor_week <- 0
for (i in 1:length(weekday)) {
    if (weekday[i] == "Sunday" | weekday[i] == "Saturday") {
        factor_week[i] <- "weekend"
    } else {
        factor_week[i] <- "weekday"
    }
}

int_steps1 <- rep(0, length(int_level))
int_steps2 <- rep(0, length(int_level))
for (i in 1:length(int_level)) {
    fac1 <- (factor_week == "weekend" & raw.data$interval == int_level[i])
    int_steps1[i] <- mean(raw.data$steps[fac1], na.rm = T)
    fac2 <- (factor_week == "weekday" & raw.data$interval == int_level[i])
    int_steps2[i] <- mean(raw.data$steps[fac2], na.rm = T)
}
int2 <- data.frame(level = rep(as.numeric(int_level), 2), steps = c(int_steps1, 
    int_steps2), week = c(rep("weekend", length(int_level)), rep("weekday", 
    length(int_level))))
g4 <- ggplot(int2, mapping = aes(level, steps))
g4 <- g4 + geom_path(col = "blue") + facet_grid(week ~ .)
g4
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

```r
#### end of question 4###
```


### It show that there are differences in activity patterns between weekdays and weekends.
