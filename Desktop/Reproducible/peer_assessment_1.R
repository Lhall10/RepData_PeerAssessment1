#loading data#
setwd("C:/Users/user/Desktop/Coursera Data Science Sequence/Reproducible")
data <- read.csv("./activity.csv")
data2 <- na.omit(data)

#Histogram of steps per day, median steps per day, mean steps per day
totalsteps<-tapply(data2$steps, data2$date, sum)

hist(totalsteps)

mean(data2$steps)

median(data2$steps)

#average daily activity pattern
avg <- tapply(data2$steps, data2$interval, mean)
intavg<-as.data.frame(avg)
intavg$interval <- data2[1:288,3]
plot(x = intavg$interval, y = intavg$avg, type = "l", xlab = "Interval", ylab = "Average Steps")

#highest average steps
subset(intavg, intavg$avg == max(intavg$avg))

#total number of NAs
sum(is.na(data))

#changing the NAs to average steps
  data[is.na(data)]<- avg

#histogram, median, and mean for new data set
totalsteps2<-tapply(data$steps, data$date, sum)

hist(totalsteps2)

mean(data$steps)

median(data$steps)

# not really different, the histogram has no holes after the data was filled i

# weekdays thing

data$date <- as.Date(data$date)
wedays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
enddays <- c("Saturday", "Sunday")
data$day <- weekdays(data$date)
data$day[data$day %in% wedays] <- "weekday"
data$day[data$day %in% enddays] <- "weekend"
data$day <- as.factor(data$day)

#panel plot, average steps per weekday or weekend day as y axis

library(lattice)

xyplot(steps ~ interval|day, data = data, type = "a")

