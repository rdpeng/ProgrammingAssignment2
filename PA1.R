setwd("C:/Users/peper/OneDrive/Documents/Coursera JHU Data Science Specialization/Reproducible Research/RepData_PeerAssessment1")
unzip("activity.zip")
activity <- read.csv("activity.csv")


day_sum <- with(activity, tapply(steps, date, sum))
hist(day_sum, xlab = "Steps", main = "Steps Per Day")

mean(day_sum, na.rm = TRUE)
median(day_sum, na.rm = TRUE)


interval_mean <- with(activity, tapply(steps, interval, mean, na.rm = TRUE))
plot(names(interval_mean), interval_mean, type = "l", xlab = "Interval", ylab = "Means Steps", main = "Mean Steps by Time Interval")

max_steps <- which(interval_mean == max(interval_mean))
interval_mean[max_steps]

nrow(activity[!complete.cases(activity),])
activity_impute <- activity
activity_impute$steps <- ave(activity_impute$steps, activity_impute$interval, FUN = function(x){ifelse(is.na(x), mean(x, na.rm = TRUE), x)})

day_sum_impute <- with(activity_impute, tapply(steps, date, sum))

hist(day_sum_impute, xlab = "Steps", main = "Steps Per Day (with imputing)")
mean(day_sum_impute, na.rm = TRUE)
median(day_sum_impute, na.rm = TRUE)

activity_impute$day.type <- factor(weekdays(as.POSIXlt(activity_impute$date)) %in% c("Saturday", "Sunday"), c(FALSE, TRUE), c("weekday", "weekend")) 

library(lattice)
library(dplyr)

mean_interval_type <- activity_impute %>% group_by(day.type, interval) %>% summarize(steps = mean(steps))
xyplot(steps ~ interval | day.type, mean_interval_type, layout = c(1,2), type = "l")
