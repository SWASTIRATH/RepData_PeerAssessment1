#Reproducible research Peer Assignment 1 
##Swasti Srivastva

###Loading the library
```{r, echo=TRUE, results='hide', warning=FALSE, message=FALSE}
library(ggplot2)
```

###Loading and processing the data
```{r,echo= TRUE}
data_raw<-read.csv("C:/Users/user/Desktop/activity.csv",header=TRUE)
data<-data_raw
head(data,10)
```

##### 2. Process/transform the data (if necessary) into a format suitable for analysis
```{r,echo=TRUE}
data<-data[complete.cases(data),]
data$date<-as.POSIXct(data$date) #Set the date format
head(data,10)
```

###What is mean total number of steps taken per day?
```{r,echo=TRUE}
totalSteps <- aggregate(steps ~ date, data = data, sum, na.rm = TRUE)
hist(totalSteps$steps) #Make a histogram of total steps taken per day
```

###Calculate and report the mean and median total number of steps taken per day

```{r,echo=TRUE}
mean(totalSteps$steps)

median(totalSteps$steps)
```

######The mean total number of steps taken per day is 10766.19 steps.
######The median total number of steps taken per day is 10765 steps.

###What is the average daily activity pattern?
Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).

```{r,echo=TRUE}
stepsInterval <- aggregate(steps ~ interval, data = data, mean, na.rm = TRUE)
plot(steps ~ interval, data = stepsInterval, type = "l")
```
Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r,echo=TRUE}
stepsInterval[which.max(stepsInterval$steps), ]$interval
```
###Imputing missing values

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```{r,echo=TRUE}
sum(is.na(data_raw$steps))
```
######Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

I used a strategy for filing in all of the missing values with the mean for that 5-minute interval. First of all, I made a function "interval2steps" to get the mean steps for particular 5-minute interval.

```{r,echo=TRUE}
interval2steps <- function(interval) {
    stepsInterval[stepsInterval$interval == interval, ]$steps}
```
Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r,echo=TRUE}
activityFilled <- data_raw  # Make a new dataset with the original data
count = 0  # Count the number of data filled in
for (i in 1:nrow(activityFilled)) {
    if (is.na(activityFilled[i, ]$steps)) {
        activityFilled[i, ]$steps <- interval2steps(activityFilled[i, ]$interval)
        count = count + 1
    }
}
cat("Total ", count, "NA values were filled.\n\r")
```
Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

```{r,echo=TRUE}
totalSteps2 <- aggregate(steps ~ date, data = activityFilled, sum)
hist(totalSteps2$steps)
```

```{r,echo=TRUE}
mean(totalSteps2$steps)
```

####Are there differences in activity patterns between weekdays and weekends?
Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```{r,echo=TRUE}
activityFilled$day = ifelse(as.POSIXlt(as.Date(activityFilled$date))$wday%%6 == 
    0, "weekend", "weekday")
# For Sunday and Saturday : weekend, Other days : weekday
activityFilled$day = factor(activityFilled$day, levels = c("weekday", "weekend"))
```
Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:

```{r,echo=TRUE}
stepsInterval2 = aggregate(steps ~ interval + day, activityFilled, mean)
library(lattice)
xyplot(steps ~ interval | factor(day), data = stepsInterval2, aspect = 1/2, 
    type = "l")
```
