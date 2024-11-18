# load libraries to use 
library(tidyverse)
library(lubridate)
library(janitor)
library(dplyr)
library(ggplot2)
library(hms)

#IMPORT DATASETS

#The number represent January to December (0-12) in 2021
trip01 <- read.csv('Cyclistic/202101-divvy-tripdata.csv')
trip02 <- read.csv('Cyclistic/202102-divvy-tripdata.csv')
trip03 <- read.csv('Cyclistic/202103-divvy-tripdata.csv')
trip04 <- read.csv('Cyclistic/202104-divvy-tripdata.csv')
trip05 <- read.csv('Cyclistic/202105-divvy-tripdata.csv')
trip06 <- read.csv('Cyclistic/202106-divvy-tripdata.csv')
trip07 <- read.csv('Cyclistic/202107-divvy-tripdata.csv')
trip08 <- read.csv('Cyclistic/202108-divvy-tripdata.csv')
trip09 <- read.csv('Cyclistic/202109-divvy-tripdata.csv')
trip10 <- read.csv('Cyclistic/202110-divvy-tripdata.csv')
trip11 <- read.csv('Cyclistic/202111-divvy-tripdata.csv')
trip12 <- read.csv('Cyclistic/202112-divvy-tripdata.csv')

#Combine all trip datasets
all_trips <- rbind(trip01, trip02, trip03, trip04, trip05, trip06, trip07, trip08, trip09, trip10, trip11, trip12)

#DATA WRANGLING AND TRANSFORMATION

# I use distinct function to check distinct values in the columns
all_trips %>% distinct(rideable_type)

# Select the columns I need for the analyses
all_trips <- all_trips %>%
  select(ride_id, rideable_type, started_at, ended_at, start_station_name, end_station_name, member_casual)

#Split the started_at and ended_at columns into separate columns
all_trips <- all_trips %>% 
  separate(started_at, into = c('started_date', 'started_time'), sep= " ")

all_trips <- all_trips %>% 
  separate(ended_at, into = c('ended_date', 'ended_time'), sep= " ") 

#Change datatypes of date and time columns 
all_trips$started_date <- as.Date(all_trips$started_date, format = "%Y-%m-%d")
all_trips$ended_date <- as.Date(all_trips$ended_date, format = "%Y-%m-%d")

all_trips$started_time <- hms::as.hms(all_trips$started_time)
all_trips$ended_time <- hms::as.hms(all_trips$ended_time)

#This takes of edge cases when the end time ride spans into next day. This will help our ride duration calculation
all_trips$ended_time[all_trips$ended_time < all_trips$started_time] <- 
  all_trips$ended_time[all_trips$ended_time < all_trips$started_time] + as.hms(24 * 60 * 60)

#Get the time duration of each ride in minutes (rounded down to 2 decimal places)
all_trips$ride_duration <- as.numeric(round(difftime(all_trips$ended_time,all_trips$started_time, units = "mins"), digits = 2))

#Get the name of month and day of bike usage
all_trips$month <- format(all_trips$started_date, "%B")
all_trips$day <- weekdays(all_trips$started_date)

#Structure of the dataset
str(all_trips)
head(all_trips)
colnames(all_trips)
View(all_trips)


#I used this to output the dataset to a csv to make sure the new data types were applied.
write.csv(all_trips, 'Cyclistic/output.csv', row.names = FALSE)


# DATA ANALYSIS AND VISUALIZATION

#Average duration of all rides
all_trips %>% summarise(avg_duration = mean(ride_duration))

#Average ride duration for different member types
avg_ride_duration <- all_trips %>% 
  group_by(member_casual) %>% 
  summarise(avg_ride_duration = mean(ride_duration))

ggplot(avg_ride_duration, aes(x = member_casual,y = avg_ride_duration, fill = member_casual)) + 
  geom_bar(position = 'dodge', stat = 'identity', width = 0.5) + 
  #scale_fill_manual(values = c("casual" = "skyblue", "member" = "coral")) +
  geom_text(aes(label = round(avg_ride_duration, 1)), vjust = 1.5, size = 3) +
  labs(title = "Average ride between membership types", x = "Membership type", y = "Average ride duration", fill = "Membership type") +
  theme_minimal()


# Different types of rideable bikes 
bikes <- table(all_trips$rideable_type)
percent_of_biketypes <- round(bikes / sum(bikes) * 100, 1)
pie(bikes, main = "Rented Bike Types distribution", labels = paste(names(bikes), percent_of_biketypes, "%"), col = rainbow(n=3, alpha = 0.2))

#Ride-able bikes usage by membership.
bikes_type_by_members <- all_trips %>% 
  group_by(member_casual) %>% 
  count(rideable_type)

ggplot(bikes_type_by_members, aes(fill=member_casual, x=rideable_type, y = n)) + 
  geom_bar(position='dodge', stat='identity', width = 0.5) +
  labs(title = "Bike type usage by membership type", x = "Bike Types", y = "Bike count", fill = "Membership type") +
  theme_minimal()


# Month Rides by different membership
month_usage <- all_trips %>% 
  group_by(member_casual) %>% 
  count(month)

ggplot(month_usage, aes(fill=member_casual, x= factor(month, levels = month.name), y = n)) + 
  geom_bar(position='dodge', stat='identity', width = 0.5) +
  scale_y_continuous(breaks = c(100000, 200000, 300000, 400000),labels = c("100K", "200K", "300K", "400K")) +
  labs(title = "Rides per month", x = "Month", y = "Ride count", fill = "Membership type") +
  theme_minimal()

#Weekly Rides by different membership
daily_usage <- all_trips %>% 
  group_by(member_casual) %>% 
  count(day)

#Arrange the days of the week in order
daily_usage$day <- factor(daily_usage$day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

ggplot(daily_usage, aes(fill=member_casual, x= day, y = n)) + 
  geom_bar(position='dodge', stat='identity', width = 0.5) +
  scale_y_continuous(breaks = c(100000, 200000, 300000, 400000, 500000),labels = c("100K", "200K", "300K", "400K", "500k")) +
  labs(title = "Rides per month", x = "Day of the week", y = "Ride count", fill = "Membership type") +
  theme_minimal()

#Find number of empty end stations.
empty_end_trips <- all_trips %>% 
  filter(is.na(end_station_name) | end_station_name == "")

table(empty_end_trips$member_casual)
