# Case Study: Bellabeat Data Analysis
 _This case study follows the six step data analysis process:_
#### ❓ [Ask](#1-ask)
#### 💻 [Prepare](#2-prepare)
#### 🛠 [Process](#3-process)
#### 📊 [Analyze](#4-analyze)
#### 📋 [Share](#5-share)
#### 🧗‍♀️ [Act](#6-act)


### Introduction
Bellabeat is a data-centered wellness femtech company behind some of the most fashionably designed tech-powered wellness products and one of the fastest-growing wellness subscription services for women. Throughout the years, Bellabeat built the go-to wellness brand with a compelling ecosystem of solutions and services focused on women’s health. The success of the Leaf and Ivy wearables enabled the brand to expand to international markets and kickstart industry discussions on the importance of innovating in the design and development of tech products.
### Scenario
While Bellabeat is currently a successful small company, Urška Sršen, the co-founder and Chief Creative Officer, envisions the company becoming a major player in the global smart device market. She believes that harnessing the power of data analysis could hold the key to unlocking new growth opportunities for the company.

## 1. Ask
**Key Task**: Analyze smart device usage data in order to gain insight and help guide marketing strategy for Bellabeat to reveal more opportunities for growth. 

**Key Stakeholders**
* Urška Sršen: Bellabeat’s cofounder and Chief Creative Officer
* Bellabeat marketing analytics team

## 2. Prepare
**Data Source:** FitBit Fitness Tracker Data by MÖBIUS
Thirty eligible Fitbit users consented to the submission of personal tracker data, including minute-level output for physical activity, heart rate, and sleep monitoring. Individual reports can be parsed by export session ID (column A) or timestamp (column B). Variation between output represents use of different types of Fitbit trackers and individual tracking behaviors/preferences. The data set contains 18 csv files.                          

**Roccc Analysis**
* Reliability : Low – dataset was collected from 30 individuals whose gender is unknown.
* Originality : Low – third party data collect using Amazon Mechanical Turk.
* Comprehensive : Medium – dataset contains multiple fields on daily activity intensity, calories used, daily steps taken, daily sleep time and weight record.
* Current : Low – This data is from May 2016, that means data is not current and user habits may have changed over the period of time. 
* Cited : Low  – Unkown

**Limitations of dataset**
* Data of only 30 users is availale, a larger sample size is preferred for the more accurate analysis.
* Age of the users is unkown. Therefore ideal judgement criteria can not be defined.
* Only 8 users have submitted their weight.



## 3. Process
Import the data set and load necessary packages required for the cleaning process.

```
# Importing data set
setwd("C:/Users/Public/Downloads/fitabase_data")
activity <- read_csv("daily_activity.csv")
sleep <- read_csv("sleepDay_merged.csv")
weight <- read_csv("weightLogInfo_merged.csv")

# Loading Packages
library(tidyverse)
library(lubridate)
library(janitor)

#### Examine the data and check for distinct values

# Viewing data structure
str(activity)
str(sleep)
str(weight)

# Checking number of entries
n_distinct(activity$id)
n_distinct(sleep$id)
n_distinct(weight$id)

```

The dataset has 33 user data from daily activity, 24 from sleep and only 8 from weight. It was further noted that the **Date** in all the three data frames (activity, seleep and weight) is formated as **CHR** not as date format.

```
# Cleaning column names (from Upper case to lower case)
activity <- clean_names(activity)
sleep <- clean_names(sleep)
weight <- clean_names(weight)

# Changing data type from chr(dmy) to date
activity$activity_date <- as.Date(activity$activity_date, '%m/%d/%y')
sleep$sleep_day <- as.Date(sleep$sleep_day, '%m/%d/%y')
weight$date <- parse_date_time(weight$date, '%m/%d/%y %H:%M:%S %p')
```
In preparation for a thorough analysis of Bellabeat's smart device data, several data cleaning steps must be undertaken to ensure the accuracy and reliability of our findings. 

```
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

```
## 4. Analyze

```
activity_cleaned %>% 
  select(total_steps, total_active_hours, calories, sedentary_hours) %>% 
  summary()

 total_steps    total_active_hours    calories    sedentary_hours
 Min.   :    0   Min.   :0.02       Min.   :  52   Min.   : 0.00  
 1st Qu.: 4920   1st Qu.:3.08       1st Qu.:1854   1st Qu.:12.02  
 Median : 8053   Median :4.30       Median :2218   Median :17.00  
 Mean   : 8319   Mean   :4.16       Mean   :2359   Mean   :15.87  
 3rd Qu.:11100   3rd Qu.:5.38       3rd Qu.:2830   3rd Qu.:19.80  
 Max.   :36019   Max.   :9.20       Max.   :4900   Max.   :23.98


count(activity_cleaned, total_steps >= 10000)

  `total_steps >= 10000`     n
  <lgl>                  <int>
1 FALSE                    556
2 TRUE                     301


sleep %>% 
  group_by(sleep_day_in_week) %>% 
  reframe(mean(hours_asleep))

  sleep_day_in_week `mean(hours_asleep)`
  <ord>                            <dbl>
1 Sun                               6.74
2 Mon                               7.25
3 Tue                               6.71
4 Wed                               6.76
5 Thu                               7.01
6 Fri                               7.55
7 Sat                               6.98


weight %>%
  group_by(id) %>%
  summarise(min(weight_kg),max(weight_kg))
 
id `min(weight_kg)` `max(weight_kg)`
       <dbl>            <dbl>            <dbl>
1 1503960366             52.6             52.6
2 1927972279            134.             134. 
3 2873212765             56.7             57.3
4 4319703577             72.3             72.4
5 4558609924             69.1             70.3
6 5577150313             90.7             90.7
7 6962181067             61               62.5
8 8877689391             84               85.8

```

#### Key findings
* The average daily step count for users was 8,319, which falls below the National Institutes of Health (NIH) recommended daily goal of 10,000 steps, with 65% of individuals not achieving this recommended target.
* The average sedentary hours amounted to 15.87 hours, significantly exceeding the National Institutes of Health (NIH) recommended target of less than 8 hours.
* All the users are lightly active
* The average hours of sleep recorded are 6.9, just slightly meeting the minimum recommended sleep duration of 7–9 hours by the NIH.
* No significant weight changes have been observed among the 8 users, which may be attributed to the small sample size.

## 5. Share

#### Total steps taken by weekday
![download](https://github.com/anirudh4747/Data-Analytics-Case-Study/assets/147338430/8131f48d-7adc-417f-9516-54dd2682ed64)

#### Highly active hour by weekday
![download](https://github.com/anirudh4747/Data-Analytics-Case-Study/assets/147338430/e58f5526-d3c3-4ba3-8f86-c1906c2b6915)

#### Total calories burned by weekday
![download](https://github.com/anirudh4747/Data-Analytics-Case-Study/assets/147338430/c5285d32-ff1c-4ee9-81cb-a29434e71f56)

A noticeable trend emerges where user activity is highest on Mondays, gradually declining as the week progresses. This trend might be associated with elevated motivation levels toward the end and beginning of the week.

#### Calories vs Total Steps
![download](https://github.com/anirudh4747/Data-Analytics-Case-Study/assets/147338430/fb0827f0-d822-43c0-8f76-d903e9fbf1e5)

#### Calories vs Active Hours
![download](https://github.com/anirudh4747/Data-Analytics-Case-Study/assets/147338430/e5c17221-6ada-4b27-b662-76c35f38eb15)

From the two charts above, it can be observed that users who have higher active hours and take more steps tend to burn more calories in a day.

### Interesting Finds:

#### Sedentary Hours vs Hours Asleep
![download](https://github.com/anirudh4747/Data-Analytics-Case-Study/assets/147338430/448c21ff-5afa-401c-90c3-caa44663d08a)

This chart illustrates the comparison between users' sedentary hours and hours spent asleep. It becomes apparent that as individuals spend more time in non-active states, their hours of sleep decrease. This negative correlation suggests that non-activity has an adverse effect on sleep quality.

#### Calories vs Sedentary hrs
![download](https://github.com/anirudh4747/Data-Analytics-Case-Study/assets/147338430/384cc186-f783-4924-8bec-2db6decf3e73)

Contrary to the common belief that non-activity implies no calorie burning, it is evident that there is no significant correlation between sedentary hours and calorie expenditure. This outcome may be influenced by hidden factors, such as individuals with high sedentary hours potentially engaging in vigorous activities in hot weather, or it could be simply a consequence of a relatively small sample size.

#### Avg steps/activity hrs vs Weight
![download](https://github.com/anirudh4747/Data-Analytics-Case-Study/assets/147338430/0180c280-157b-45d2-a34c-636ed94f2f4d)

The users who are most active and those with higher step counts  fall within the range of 50 kg–85 kg. Furthermore, there is a noticeable drop in activity among users weighing over 85 kg.

## 6. Act
### Conclusion
* There is a noticeable trend where user activity is highest on Mondays, gradually declining as the week progresses with a slight peak on Thursdays. This pattern could be associated with variations in motivation levels throughout the week.
* Users are most inactive on Fridays, on an average, they spend 15.87 hours in sedentary activities, significantly exceeding the NIH's recommended target of less than 8 hours. This indicates a high level of inactivity, which can have adverse health implications.
* There is a negative correlation between sedentary hours and sleep duration, suggesting that increased sedentary time is associated with reduced sleep quality.
* The most active users and those with higher step counts tend to have weights within the range of 50 kg to 85 kg. Activity levels decline among users weighing over 85 kg.

### Recommendations for Bellabeat Marketing Strategy:
* Develop features and notifications in the fitness watch and app that encourage users to increase their daily step counts. Offer challenges, rewards, and reminders to meet the personalized activity goals based on each user's fitness level and progress.

* Implement features that help users reduce sedentary hours. These could include reminders to stand or move after prolonged inactivity. Offer features that track and display sedentary time, helping users become more aware of their habits.

* Highlight the sleep tracking features of the fitness watches in marketing materials. Promote the connection between reduced sedentary time and improved sleep quality. Educate users about the benefits of adequate sleep for overall health.

* Develop marketing campaigns and resources aimed at heavy users. Showcase specialized fitness plans, nutritional guidance, and support services tailored to their needs.

* Encourage user referrals and offer incentives for users to invite friends and family to join the fitness watch community. A larger user base can provide more robust data for analysis and better insights.


