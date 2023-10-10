# Loading Packages

library(tidyverse)
library(lubridate)
library(janitor)

# importing data set

setwd("C:/Users/Bunny/Desktop/fitabase_data_4.12.16-5.12.16")
activity <- read_csv("daily_activity.csv")
sleep <- read_csv("sleepDay_merged.csv")
weight <- read_csv("weightLogInfo_merged.csv")


# Viewing data structure
str(activity)
str(sleep)
str(weight)

# Checking number of entries
n_distinct(activity$id)
n_distinct(sleep$id)
n_distinct(weight$id)

# Cleaning column names (from Upper case to lower case)

activity <- clean_names(activity)
sleep <- clean_names(sleep)
weight <- clean_names(weight)

# Changing data type from chr(dmy) to date

activity$activity_date <- as.Date(activity$activity_date, '%m/%d/%y')
sleep$sleep_day <- as.Date(sleep$sleep_day, '%m/%d/%y')
weight$date <- parse_date_time(weight$date, '%m/%d/%y %H:%M:%S %p')

# Adding days column

activity$day_of_week <- wday(activity$activity_date, label = T, abbr = T)

sleep$sleep_day_in_week <- wday(sleep$sleep_day, label = T, abbr = T)

# Turning active hours and sedentary minutes in hour and rounding 

activity$total_active_hours =
  round((activity$very_active_minutes + 
           activity$fairly_active_minutes + 
           activity$lightly_active_minutes)/60, digits = 2)


activity$sedentary_hours =
  round(activity$sedentary_minutes/60, digits = 2)

sleep$total_minutes_asleep =
  round(sleep$total_minutes_asleep/60 , digits = 2) 
sleep <- rename(sleep, hours_asleep = total_minutes_asleep)

sleep$total_time_in_bed =
  round(sleep$total_time_in_bed/60 , digits = 2) 
sleep <- rename(sleep, hours_in_bed = total_time_in_bed)

# Time taken to sleep
sleep$time_take_to_sleep <- (sleep$hours_in_bed - sleep$hours_asleep)

# Remove fat column in weight
weight <- weight %>% 
  select(-c(fat))

# Adding bmi rating

weight <- weight %>% 
  mutate(bmi2 = case_when(bmi > 24.9 ~ "overweight", bmi < 18.5 ~ "underweight", TRUE ~ "healthy"))



# Removing zero values from calories and active hours column

activity_cleaned <- activity[!(activity$calories<=0),]
activity_cleaned <- activity_cleaned[!(activity_cleaned$total_active_hours<=0.00),]

# Analysis

weight %>%
  group_by(id) %>%
  summarise(min(weight_kg),max(weight_kg))

activity_cleaned %>% 
  select(total_steps, total_active_hours, calories, sedentary_hours) %>% 
  summary()

sleep %>% 
  group_by(sleep_day_in_week) %>% 
  reframe(mean(hours_in_bed))

count(activity_cleaned, total_steps = 6000)

count(activity_cleaned, sedentary_hours < 8)


