## Read data into data frame from csv file
activity_data <- read.csv("activity.csv")

## Calculate total steps per day stored in a data frame
steps_date <- aggregate(steps ~ date, data = activity_data, FUN = sum)

## Plot total steps per day
barplot(steps_date$steps, names.arg = steps_date$date, xlab = "date", ylab = "steps")

## Calculate mean and median total steps/day
mean(steps_date$steps)
median(steps_date$steps)

## Calculate total steps per interval and store in data frame
steps_interval <- aggregate(steps ~ interval, data = activity_data, FUN = mean)

## Plot total steps per interval 
plot(steps_interval, type = "l")
dev.off()

## Calculates total # steps by intervel across all days
steps_interval$interval[which.max(steps_interval$steps)]

## Total # of NA values
sum(is.na(activity))

## Replacing NA values with the mean of tsteps take by interval
activity_merged <- merge(activity_data, steps_interval, by = "interval", suffixes = c("", ".y"))
nas <- is.na(activity_merged$steps)
activity_merged$steps[nas] <- activity_merged$steps.y[nas]
activity_merged <- activity_merged[, c(1:3)]

## Aggregating the sum of steps taken by day
steps_date <- aggregate(steps ~ date, data = activity_merged, FUN = sum)

## Plotting the total sum of the steps taken daily
barplot(steps_date$steps, names.arg = steps_date$date, xlab = "date", ylab = "steps")

## Calculating mean/median of total daily steps taken
mean(steps_date$steps)
median(steps_date$steps)

## Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a 
##given date is a weekday or weekend day.
daytype <- function(date) {
    if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
        "weekend"
    } else {
        "weekday"
    }
}
activity_merged$daytype <- as.factor(sapply(activity_merged$date, daytype))


## Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the 
## average number of steps taken, averaged across all weekday days or weekend days (y-axis) 
par(mfrow = c(2, 1))
for (type in c("weekend", "weekday")) {
    steps_type <- aggregate(steps ~ interval, data = activity_merged, subset = activity_merged$daytype == type, FUN = mean)
    plot(steps_type, type = "l", main = type)
}
dev.off()
