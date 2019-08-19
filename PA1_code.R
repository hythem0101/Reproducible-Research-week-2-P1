## ----loaddata------------------------------------------------------------
  unzip(zipfile="repdata_data_activity.zip")
  data <- read.csv("activity.csv")

##------------------------------------------------------------------------
  library(ggplot2)
  total.steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
  qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")
  mean(total.steps, na.rm=TRUE)
  median(total.steps, na.rm=TRUE)

## ------------------------------------------------------------------------
library(ggplot2)
  averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                      FUN=mean, na.rm=TRUE)
  ggplot(data=averages, aes(x=interval, y=steps)) +
  geom_line() +
  xlab("5-minute interval") +
  ylab("average number of steps taken")


## ------------------------------------------------------------------------
  averages[which.max(averages$steps),]


## ----how_many_missing Value----------------------------------------------------
  missing <- is.na(data$steps)
# How many missing
  table(missing)
#------------------------------------------------------------------------------
#check if all the NAs have been replaced

  library(magrittr)
  library(dplyr)

  replacewithmean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
  meandata <- data%>% group_by(interval) %>% mutate(steps= replacewithmean(steps))
  head(meandata)
#-----------------------------------------------------------------------------

  FullSummedDataByDay <- aggregate(meandata$steps, by=list(meandata$date), sum)

  names(FullSummedDataByDay)[1] ="date"
  names(FullSummedDataByDay)[2] ="totalsteps"
  head(FullSummedDataByDay,15)

  summary(FullSummedDataByDay)

  hist(FullSummedDataByDay$totalsteps, xlab = "Steps", ylab = "Frequency", main = "Total Daily Steps", breaks = 20,col=c("blue"))

## ------------------------------------------------------------------------
  meandata$date <- as.Date(meandata$date)
  meandata$weekday <- weekdays(meandata$date)
  meandata$weekend <- ifelse(meandata$weekday=="Saturday" | meandata$weekday=="Sunday", "Weekend", "Weekday" )

## ------------------------------------------------------------------------
  library(ggplot2)
  meandataweekendweekday <- aggregate(meandata$steps , by= list(meandata$weekend, meandata$interval), na.omit(mean))
  names(meandataweekendweekday) <- c("weekend", "interval", "steps")
  
  ggplot(meandataweekendweekday, aes(x=interval, y=steps, color=weekend)) + geom_line()+
    facet_grid(weekend ~.) + xlab("Interval") + ylab("Mean of Steps") +
    ggtitle("Comparison of Average Number of Steps in Each Interval")