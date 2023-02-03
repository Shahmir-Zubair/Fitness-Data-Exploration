# Installing packages
install.packages("tidyverse")
install.packages("ggplot2")

# Loading packages
library(tidyverse)
library(ggplot2)


# Reading all the necessary csv files into program

daily_activity <- read_csv('E:/Code/Google data analytics certificate/Capstone/Case Studies/Fitness Data Exploration/Data/dailyActivity_merged.csv')

weight_log <- read_csv('E:/Code/Google data analytics certificate/Capstone/Case Studies/Fitness Data Exploration/Data/weightLogInfo_merged.csv')

sleep_day <- read_csv('E:/Code/Google data analytics certificate/Capstone/Case Studies/Fitness Data Exploration/Data/sleepDay_merged.csv')

# Understanding the dataframes
head(daily_activity)
n_distinct(daily_activity$Id)
nrow(daily_activity)

head(weight_log)
n_distinct(weight_log$Id)
nrow(weight_log)

head(sleep_day)
n_distinct(sleep_day$Id)
nrow(sleep_day)

# Corelation b/w excercise and sedentary

ggplot(data=daily_activity, aes(x=TotalSteps, y=SedentaryMinutes)) + 
  geom_point(color = "#004F98") +  
  geom_smooth(method=lm)

# Correlation b/w exercise and calories burned
ggplot(daily_activity, aes(TotalSteps, Calories)) +
  geom_point(color = "#004F98") +
  geom_smooth(method=lm)


# Correlation b/w excercise and weight

steps_vs_weight <- merge(daily_activity, weight_log) %>% 
  group_by(Id) %>% 
  summarize(steps = mean(TotalSteps), weight = mean(WeightKg))

steps_vs_weight

ggplot(steps_vs_weight, aes(steps,weight)) + 
  geom_point(color="#004F98", size = 3) + 
  geom_smooth(method = lm, level = 0.4, color = "#004F98")


# Correlation b/w very active people and sleep (No Relation found)
activity_vs_sleep <- merge(daily_activity, sleep_day) %>%
  select(Id, VeryActiveMinutes, TotalMinutesAsleep) %>% 
  group_by(Id) %>% 
  summarize(very_active_minutes = mean(VeryActiveMinutes), total_minutes_asleep = mean(TotalMinutesAsleep))

activity_vs_sleep

ggplot(activity_vs_sleep, aes(very_active_minutes, total_minutes_asleep)) +
  geom_point(color = "#004F98", size = 3) +
  geom_smooth(method=lm)

# Making a dayname column in daily_activity
# Since ActivityDate is char datatype, first i will convert it into date

daily_activity$ActivityDate <- as.Date(daily_activity$ActivityDate,"%m/%d/%y")
daily_activity$dayname <- weekdays(daily_activity$ActivityDate)

daily_activity_df
# Correlation b/w VeryActiveMinutes vs day of the week


ggplot(daily_activity, aes( x = fct_relevel(dayname, "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), VeryActiveMinutes)) + 
  geom_col(fill = "#004F98") + 
  labs(x="DayName") 

# How many people are overweight? (using BMI)

BMI <- weight_log %>% 
  group_by(Id) %>% 
  summarise(mean_BMI = mean(BMI, na.rm = TRUE))

BMI <- BMI %>%  mutate(case_when(
  mean_BMI < 25 ~ "Healthy",
  mean_BMI >=25 & mean_BMI < 30 ~ "Overweight",
  mean_BMI > 30 ~ "Obese"
))
colnames(BMI)[3] <- "health_condition"

BMI


# BMI less than 25 is healthy. BMI b/w 25-25.9 is overweight and greater than 30 is obese

ggplot(BMI, aes(fct_relevel(health_condition, "Obese", "Overweight", "Healthy"))) +
  geom_bar(fill = "#004F98") +
  labs(title = "Condition of 8 people based on BMI",x = "Condition", y = "Number of people")
