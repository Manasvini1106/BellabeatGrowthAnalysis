## LOAD PACKAGES

library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(janitor)

## IMPORTING DATA
activity = read_csv("/Users/manasvininittala/Desktop/Rutgers/JOBS/Certifications/Google Data Analytics/Course 8 - Capstone - A Complete Case Study/Bellabeat/archive/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
calories = read_csv("/Users/manasvininittala/Desktop/Rutgers/JOBS/Certifications/Google Data Analytics/Course 8 - Capstone - A Complete Case Study/Bellabeat/archive/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/dailyCalories_merged.csv")
intensities = read_csv("/Users/manasvininittala/Desktop/Rutgers/JOBS/Certifications/Google Data Analytics/Course 8 - Capstone - A Complete Case Study/Bellabeat/archive/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/dailyIntensities_merged.csv")
sleep = read_csv("/Users/manasvininittala/Desktop/Rutgers/JOBS/Certifications/Google Data Analytics/Course 8 - Capstone - A Complete Case Study/Bellabeat/archive/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
weight = read_csv("/Users/manasvininittala/Desktop/Rutgers/JOBS/Certifications/Google Data Analytics/Course 8 - Capstone - A Complete Case Study/Bellabeat/archive/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")

## UNDERSTANDING DATA
head(activity)
colnames(activity)
head(weight)
colnames(weight)

## CLEAN DATA
activity$ActivityDate=as.POSIXct(activity$ActivityDate, format="%m/%d/%Y", tz=Sys.timezone())
activity$date <- format(activity$ActivityDate, format = "%m/%d/%y")

sleep$SleepDay=as.POSIXct(sleep$SleepDay, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
sleep$date <- format(sleep$SleepDay, format = "%m/%d/%y")

## ANALYZE DATA
# Finding number of participants in each category
n_distinct(activity$Id)  
n_distinct(calories$Id)   
n_distinct(intensities$Id)
n_distinct(sleep$Id)
n_distinct(weight$Id)

# checking for significant change in weight
weight%>%
  group_by(Id)%>%
  summarise(min(WeightKg),max(WeightKg))

# activity
activity %>%  
  select(TotalSteps,
         TotalDistance,
         SedentaryMinutes, Calories) %>%
  summary()

# active minutes per category
activity %>%
  select(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes) %>%
  summary()

# calories
calories %>%
  select(Calories) %>%
  summary()
# sleep
sleep %>%
  select(TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed) %>%
  summary()
# weight
weight %>%
  select(WeightKg, BMI) %>%
  summary()

## MERGE DATA
merged_data <- merge(sleep, activity, by = c('Id', 'date'))
head(merged_data) 


## SHARE
ggplot(data = activity, aes(x = TotalSteps, y = Calories)) + geom_point() + geom_smooth() + labs(title = "Total Steps vs. Calories")
ggplot(data = sleep, aes(x = TotalMinutesAsleep, y = TotalTimeInBed)) + geom_point() + labs(title = "Total time asleep vs Total time in bed")
ggplot(data = merged_data, mapping = aes(x = SedentaryMinutes, y = TotalMinutesAsleep)) +
  geom_point() + labs(title= "Sleep Duration and Sedentary Time") 
cor(merged_data$TotalMinutesAsleep,merged_data$SedentaryMinutes)

# aggregate data by day of week to summarize averages
merged_data <- mutate(merged_data, day = wday(SleepDay, label = TRUE))
summarized_activity_sleep <- merged_data %>% 
  group_by(day) %>% 
  summarise(AvgDailySteps = mean(TotalSteps),
            AvgAsleepMinutes = mean(TotalMinutesAsleep),
            AvgAwakeTimeInBed = mean(TotalTimeInBed), 
            AvgSedentaryMinutes = mean(SedentaryMinutes),
            AvgLightlyActiveMinutes = mean(LightlyActiveMinutes),
            AvgFairlyActiveMinutes = mean(FairlyActiveMinutes),
            AvgVeryActiveMinutes = mean(VeryActiveMinutes), 
            AvgCalories = mean(Calories))
head(summarized_activity_sleep) 

ggplot(data = summarized_activity_sleep, mapping = aes(x = day, y = AvgDailySteps)) +
  geom_col(fill = "orange") + labs(title = "Daily Step Count")
