library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(mice)

# function for sapply with proper names
sapplyNames = function(List, FUN, Names, ...){
    a = sapply(List, FUN, ...)%>%
        as.data.frame()
    a$x = row.names(a)
    row.names(a) = NULL
    names(a) = Names
    a
}

#Loading and processing the data
activity = read.csv('activity.csv', stringsAsFactors = F)
activity = mutate(activity, date = ymd(date))
interval = activity$interval
activity = separate(activity, interval, into = c('hours', 'minutes'), sep = -2)
activity$hours[activity$hours == ''] = 0
activity = mutate(activity ,time = paste(hours, minutes, sep = ':'))%>%
    mutate(dateTime = ymd_hm(paste(date,time)))

# Histogram of the total number of steps taken each day
activityDate = split(activity$steps, activity$date)
dailyActivity = sapplyNames(activityDate, sum, c('steps', 'date'), na.rm = T)
dailyActivity = mutate(dailyActivity, date = ymd(date))

# Mean and median number of steps taken each day
avg = mean(dailyActivity$steps, na.rm = T)
mdin = median(dailyActivity$steps, na.rm = T)

ggplot(dailyActivity, aes(steps)) + 
    geom_histogram(fill = 'steelblue', bins = 50) + 
    labs(x = 'Steps', title = 'Histogram of Steps', subtitle = 'Steps taken each day')+
    geom_vline(aes(xintercept = avg, color = 'mean'), linetype="dashed", size = 1) + 
    geom_vline(aes(xintercept = mdin, color = 'median'), linetype="dashed", size = 1) +
    scale_color_manual(name = 'Statistics', values = c(mean = 'green', median = 'darkgreen'))+ 
    theme(legend.position = c(0.9,0.85))

# Time series plot of the average number of steps taken
activity = mutate(activity, time = as.numeric(hour(dateTime))+as.numeric(minute(dateTime))/60)

## calculate the average steps taken, by time interval
activityInterval = split(activity$steps, interval)
intervalActivity = sapplyNames(activityInterval, mean, c('steps', 'interval'), na.rm = T)

## extract time only from the dateTime variable
intervalActivity = separate(intervalActivity, interval, into = c('hours', 'minutes'), sep = -2)
intervalActivity$hours[intervalActivity$hours == ''] = 0
intervalActivity = mutate(intervalActivity ,time = paste(hours, minutes, sep = ':'))
intervalActivity$dateTime = ymd_hm(paste(activity$date[1:nrow(intervalActivity)],
                                         intervalActivity$time,
                                         sep = ' '))
intervalActivity = mutate(intervalActivity, 
                          time = as.numeric(hour(dateTime))+as.numeric(minute(dateTime))/60)

intervalActivity$time = as.POSIXct(strftime(intervalActivity$dateTime, format="%H:%M:%S", tz = 'GMT'), format ="%H:%M:%S")

xticks = paste(round(0:23, digits = 2),'00', sep = ':') %>%
    parse_date_time2('HM')
xlabels = round(0:23, digits = 2)

activity = mutate(activity, Time = format(as.POSIXct(time, format = '%H:%M'), '%H:%M'))
activity$Time =parse_date_time2(activity$Time, orders = 'HM',tz = 'GMT')

ggplot(activity, aes(x = Time, y = steps, color = as.factor(activity$date), group = 61)) + 
    geom_line(show.legend = F) + 
    geom_line(aes(x = activity$Time, y = rep(intervalActivity$steps, times = 61),
                  color = 'Average'), color = 'black', show.legend = T)+
    labs(x= 'Hour', y = 'Steps', title = 'Steps taken on each day',
         subtitle = 'Average as black line') +
    theme_bw() + theme(legend.position = 'none')+
    scale_x_time(breaks = xticks, labels= xlabels)

# The 5-minute interval that, on average, contains the maximum number of steps
int = with(intervalActivity, dateTime[which.max(steps)])
steps = max(intervalActivity$steps)
c(strftime(int, format = "%H:%M:%S", tz= 'GMT'), steps)

# Code to describe and show a strategy for imputing missing data
## table of combination of missing data
md.pattern(activity, plot = F) 
## percent of missing data
mean(is.na(activity$steps))*100
## calculating the multiple imputations using method predictive mean matching
tempActivity = mice(activity, method = 'pmm', seed = 1)
## picking the best iteration based on the summary values
with(activity, summary(steps))
with(tempActivity$imp, summary(steps))
impActivity = complete(tempActivity, 4)

# Histogram of the total number of steps taken each day;
# after missing values are imputed
impActivityDate = split(impActivity$steps, impActivity$date)
dailyImpActivity = sapplyNames(impActivityDate, sum, c('steps', "date")) 
dailyImpActivity = mutate(dailyImpActivity, date = ymd(date))

# Mean and median number of steps taken each day
avgimp = mean(dailyImpActivity$steps)
mdinimp = median(dailyImpActivity$steps)

ggplot(dailyImpActivity, aes(steps)) + 
    geom_histogram(fill = 'steelblue', bins = 50) + 
    labs(x = 'Steps', title = 'Histogram of Steps (Imputed data)', subtitle = 'Steps taken each day')+
    geom_vline(aes(xintercept = avg, color = 'mean'), linetype="dashed", size = 1) + 
    geom_vline(aes(xintercept = mdin, color = 'median'), linetype="dashed", size = 1) +
    scale_color_manual(name = 'Statistics', values = c(mean = 'green', median = 'darkgreen'))+ 
    theme(legend.position = c(0.9,0.85))

# Panel plot comparing the average number of steps taken per 5-minute interval;
# across weekdays and weekends
a = weekdays(impActivity$date)
b = as.numeric(a %in% c('Saturday', 'Sunday'))
impActivity$Weekday = 'Weekday'
impActivity$Weekday[b == 1] = 'Weekend'
impActivity = mutate(impActivity, Weekday = as.factor(Weekday))
impActivityS = split(impActivity[,1], impActivity[c('Weekday', 'interval')])
avgStepsWeekday = sapplyNames(impActivityS, mean, c('Steps', 'Weekday.interval'))%>%
    separate(Weekday.interval, into = c('Weekday', 'interval'), sep = '\\.')%>%
    mutate(interval = as.numeric(interval))%>%
    mutate(Weekday = as.factor(Weekday))
ggplot(avgStepsWeekday, aes(interval, Steps))+
    geom_line(col = 'steelblue') + 
    facet_grid(rows = avgStepsWeekday$Weekday) + 
    labs(x = 'Interval', y = 'Number of Steps', title = 'Average number of steps', subtitle = 'On weekdays vs weekends')

# All of the R code needed to reproduce the results (numbers, plots, etc.) in the report