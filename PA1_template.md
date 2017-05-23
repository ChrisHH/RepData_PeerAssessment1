---
title: "Reproducible_Research_Course_Project_1"
author: "Sandeep Muttha"
date: "May 23, 2017"
output:
  html_document: default
  pdf_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Load required libraries
```{r libraries, message=FALSE, warning=FALSE}
library(dplyr)
library(ggplot2)
```

## 1. Loading and preprocessing the data

Code for reading in the dataset and/or processing the data

```{r loading, include=TRUE, cache = TRUE}
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile = "data.zip")
activity <- read.csv(unzip("data.zip"))

steps_per_day <- activity %>%
    group_by(date) %>%
    summarise(sum_steps = sum(steps, na.rm = TRUE))
```


## 2. What is mean total number of steps taken per day?

Histogram of the total number of steps taken each day

```{r steps_per_day, fig.align = 'center'}
hist(steps_per_day$sum_steps, col = "salmon", border = "white", main = "Histogram of number of steps taken per day", xlab = "Number of steps per day")
```

Mean and median number of steps taken each day

```{r}
mean_steps_per_day <- mean(steps_per_day$sum_steps)
median_steps_per_day <- median(steps_per_day$sum_steps)
```

Mean number of steps taken per day over the period of analysis is `r round(mean_steps_per_day,0)`  
Median number of steps taken per day over the period of analysis is `r format(round(median_steps_per_day,0), nsmall =0)`

## 3. What is the average daily activity pattern?

``` {r steps_per_interval, fig.align = 'center', fig.width = 10}
avg_steps_in_interval <- activity %>%
    group_by(interval) %>%
    summarise(mean_steps = mean(steps, na.rm = TRUE))

plot(avg_steps_in_interval$mean_steps, type = "l", xlab = "5 min Interval", ylab = "Average steps taken during the intereval", main = "Average pattern of steps taken during a day by interval", lwd = 2, col = "salmon")

max_step_interval <- which.max(avg_steps_in_interval$mean_steps)

```

5 minute Interval "`r max_step_interval`" on average across all the days in the dataset, contains the maximum number of steps.   

## 4. Imputing missing values

Check for rows with missing data

``` {r imputing}
count_missing <- sum(!complete.cases(activity))
imputed_activity <- transform(activity, 
                          steps = ifelse(is.na(activity$steps), avg_steps_in_interval$mean_steps[match(activity$interval, avg_steps_in_interval$interval)], activity$steps))

```

Number of records with missing data is `r count_missing`  

Missing values were imputed with average values for that interval across all days.

Replotting histogram of total steps by day and recalculating mean and median steps per day

```{r steps_per_day_new, fig.align = 'center', fig.width=10}
steps_per_day_new <- imputed_activity %>%
    group_by(date) %>%
    summarise(sum_steps = sum(steps, na.rm = TRUE))

par(mfrow = c(1,2))

hist(steps_per_day$sum_steps, col = "salmon", border = "white", main = "Histogram before imputing missing values", xlab = "Number of steps per day")

hist(steps_per_day_new$sum_steps, col = "purple", border = "white", main = "Histogram after imputing missing values", xlab = "Number of steps per day")


```

Mean and median number of steps taken each day recalculated after imputing missing values

```{r}
mean_steps_per_day_new <- mean(steps_per_day_new$sum_steps)
median_steps_per_day_new <- median(steps_per_day_new$sum_steps)
```

Mean number of steps taken per day over the period of analysis is `r format(round(mean_steps_per_day_new,0), nsmall = 0)` which is an increase of `r round(mean_steps_per_day_new,0) - round(mean_steps_per_day,0)` after imputation.  
Median number of steps taken per day over the period of analysis is `r format(round(median_steps_per_day_new,0), nsmall =0)` which is an increase of `r format(round(median_steps_per_day_new - median_steps_per_day,0), nsmall = 0)` after imputation.


## 5. Are there differences in activity patterns between weekdays and weekends?

Creating a factor variable to classify weekdays and weekends and compute means in each interval comparing weekdays and weekends.  

``` {r weekdays}
imputed_activity$weekday <- as.factor(ifelse(is.element(weekdays(as.Date(imputed_activity$date)), c("Saturday", "Sunday")), "weekend", "weekday"))

avg_steps_in_interval_new <- imputed_activity %>%
    group_by(interval, weekday) %>%
    summarise(mean_steps = mean(steps, na.rm = TRUE))

```



``` {r plotweekdays, fig.height = 10, fig.width = 10, fig.align = "center"}
par(mfrow = c(2,1))

plot(avg_steps_in_interval_new[avg_steps_in_interval_new$weekday == "weekday",]$mean_steps, type = "l", xlab = "5 min Interval", ylab = "Average steps taken during the intereval", main = "Average pattern of steps taken during a day on weekdays", lwd = 2, col = "salmon")

plot(avg_steps_in_interval_new[avg_steps_in_interval_new$weekday == "weekend",]$mean_steps, type = "l", xlab = "5 min Interval", ylab = "Average steps taken during the intereval", main = "Average pattern of steps taken during a day on weekends", lwd = 2, col = "purple")

```