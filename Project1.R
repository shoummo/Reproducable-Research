library(knitr)
library(ggplot2)

# Dataset
data <- read.csv('activity.csv')

# Transform
data <- transform(data, date = as.Date(date))

## What is mean total number of steps taken per day?
# Agrregate, remove NA
steps_per_day <- aggregate(steps ~ date, data, sum, na.rm = TRUE)

# Histogram
hist(steps_per_day$steps, main = "Total Steps per day", xlab = "Steps", ylim = c(0,40), labels = TRUE)

# Obtain Mean
mean(steps_per_day$steps)

# Obtain Median
median(steps_per_day$steps)

## What is the average daily activity pattern?
# Agrregate, remove NA
mean_steps_interval <- aggregate(steps ~ interval, data, mean, na.rm = TRUE)

# Time Series Plot
plot(mean_steps_interval$interval, mean_steps_interval$steps, type = "l", main = "Average number of steps per 5-min interval", xlab = "Interval", ylab = "Average Steps")


## Which 5-minute interval, on average across all days, contains the max number of steps?
max_interval <- mean_steps_interval$interval[which(mean_steps_interval$steps == max(mean_steps_interval$steps))]
max_interval


## Imputing missing values
sum(is.na(data))

imputed_data <- data
for (i in 1:length(imputed_data$steps)) {
  if (is.na(imputed_data$steps[i])) {
    imputed_data$steps[i] <- mean_steps_interval$steps[mean_steps_interval$interval == imputed_data$interval[i]]
  }
}

## Aggregate the filled data and make sure there are no NA steps
imp_steps_per_day <- aggregate(steps ~ date, imputed_data, sum, na.rm = TRUE)
sum(is.na(imp_steps_per_day$steps))

# Create the plot with the imputed data
hist(imp_steps_per_day$steps, main = "Total Steps per day", xlab = "Steps", ylim = c(0,40), labels = TRUE)

## Mean and median are now same
mean(imp_steps_per_day$steps)
median(imp_steps_per_day$steps)

## Are there differences in activity patterns between weekdays and weekends?
imputed_data$date <- as.Date(imputed_data$date)
imputed_data$wkdy <- "weekday"
imputed_data$wkdy[weekdays(imputed_data$date) == "Saturday" | weekdays(imputed_data$date) == "Sunday"] <- "weekend"
imputed_data$wkdy <- as.factor(imputed_data$wkdy)
imputed_data_interval <- aggregate(steps ~ interval + wkdy, imputed_data, mean, na.rm = TRUE)

## Plot
g <- ggplot(imputed_data_interval, aes(interval, steps))
g + facet_grid(wkdy ~ .) + geom_line() + ggtitle("Average number of steps per 5-min interval")