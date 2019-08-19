\documentclass{article}

\begin{document}
\SweaveOpts{concordance=TRUE}

---
output:
  html_document: default
  pdf_document: default
---
# Reproducible Research:  Assessment 1

## Loading and preprocessing the data
```{r loaddata}
unzip(zipfile="activity.zip")
data <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?
```{r}
 library(ggplot2)
  total.steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
  qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")
  mean(total.steps, na.rm=TRUE)
  median(total.steps, na.rm=TRUE)
```

## What is the average daily activity pattern?
```{r}
library(ggplot2)
averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=averages, aes(x=interval, y=steps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps taken")
```

On average across all the days in the dataset, the 5-minute interval contains
the maximum number of steps?
```{r}
averages[which.max(averages$steps),]
```

## Imputing missing values

There are many days/intervals where there are missing values (coded as `NA`). The presence of missing days may introduce bias into some calculations or summaries of the data.

```{r how_many_missing}
missing <- is.na(data$steps)
# How many missing
table(missing)
```

All of the missing values are filled in with mean value for that 5-minute
interval.

```{r}
# Replace each missing value with the mean value of its 5-minute interval
library(magrittr)
  library(dplyr)

  replacewithmean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
  meandata <- data%>% group_by(interval) %>% mutate(steps= replacewithmean(steps))
  head(meandata)
```
Now, using the filled data set, let's make a histogram of the total number of steps taken each day and calculate the mean and median total number of steps.

```{r}
 FullSummedDataByDay <- aggregate(meandata$steps, by=list(meandata$date), sum)

  names(FullSummedDataByDay)[1] ="date"
  names(FullSummedDataByDay)[2] ="totalsteps"
  head(FullSummedDataByDay,15)

  summary(FullSummedDataByDay)

  hist(FullSummedDataByDay$totalsteps, xlab = "Steps", ylab = "Frequency", main = "Total Daily Steps", breaks = 20,col=c("blue"))

```

Mean and median values are higher after imputing missing data. The reason is
that in the original data, there are some days with `steps` values `NA` for 
any `interval`. The total number of steps taken in such days are set to 0s by
default. However, after replacing missing `steps` values with the mean `steps`
of associated `interval` value, these 0 values are removed from the histogram
of total number of steps taken each day.

## Are there differences in activity patterns between weekdays and weekends?
First, let's find the day of the week for each measurement in the dataset. In
this part, we use the dataset with the filled-in values.

```{r}
meandata$date <- as.Date(meandata$date)
  meandata$weekday <- weekdays(meandata$date)
  meandata$weekend <- ifelse(meandata$weekday=="Saturday" | meandata$weekday=="Sunday", "Weekend", "Weekday" )

```

Now, let's make a panel plot containing plots of average number of steps taken
on weekdays and weekends.
```{r}
library(ggplot2)
  meandataweekendweekday <- aggregate(meandata$steps , by= list(meandata$weekend, meandata$interval), na.omit(mean))
  names(meandataweekendweekday) <- c("weekend", "interval", "steps")
  
  ggplot(meandataweekendweekday, aes(x=interval, y=steps, color=weekend)) + geom_line()+
    facet_grid(weekend ~.) + xlab("Interval") + ylab("Mean of Steps") +
    ggtitle("Comparison of Average Number of Steps in Each Interval")
```



\end{document}