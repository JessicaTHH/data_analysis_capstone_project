
install.packages("tidyverse")
install.packages("readr")
install.packages("lubridate")
install.packages("ggplot2")
library(tidyverse)
library("readr")
library("lubridate")
library("ggplot2")



# Process
## Load data to R studio...
jan_22 <-read_csv("/cloud/project/divvy-tripdata/202201-divvy-tripdata.csv")
feb_22 <-read_csv("/cloud/project/divvy-tripdata/202202-divvy-tripdata.csv")
mar_22 <-read_csv("/cloud/project/divvy-tripdata/202203-divvy-tripdata.csv")


## Combine into a single file...
### Compare comlumn names before merging...
colnames(jan_22)
colnames(feb_22)
colnames(mar_22)

### Since the column names are same across different csv files, let's then check whether the data in columns are the same data type in each table.
str(jan_22)
str(feb_22)
str(mar_22)

### Stack individual quarter's data frames into one big data frame
all_trips<-bind_rows(jan_22,feb_22,mar_22)
colnames(all_trips)


## Clean data for analysis
### Delete the columns that will not be used in this analysis
all_trips<-all_trips %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))
colnames(all_trips)

### Check usertype under the member_casual column
table(all_trips$member_casual)

### Create columns in all_trips to list the date, month, day, and year of each ride
all_trips$date<-as.Date(all_trips$started_at) # The default format will be in yyyy-mm-dd
all_trips$month<-format(as.Date(all_trips$date),"%m")
all_trips$day<-format(as.Date(all_trips$date),"%d")
all_trips$year<-format(as.Date(all_trips$date),"%Y")
all_trips$day_of_week<-format(as.Date(all_trips$date),"%A")

### Add a ride_length column to all_trips
all_trips$ride_length<-difftime(all_trips$ended_at,all_trips$started_at)

### Quick check on new-added columns
str(all_trips)

### Convert ride_length to numeric for calculations on data
all_trips$ride_length<-as.numeric(as.character(all_trips$ride_length))
#### You can check whether the data has converted successfully by requesting is.numeric(all_trips$ride_length)




# Analyze
## Processing to analysis
### Create a new data frame to exclude bad data(those with negative ride_length data)
all_trips_v2 <- all_trips[!(all_trips$ride_length<0),]

### Getting descriptive analysis on ride_length
#### mean(all_trips_v2$ride_length)
#### median(all_trips_v2$ride_length)
#### max(all_trips_v2$ride_length)
#### min(all_trups_v2$ride_length)
summary(all_trips_v2$ride_length)

### Comparing the difference between memeber and casual riders
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

#### aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
#### Since the days of week were out of order, set them into orders before listing the data
all_trips_v2$day_of_week <-ordered(all_trips_v2$day_of_week, levels = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)



# Share
## Create data viz about the difference between member and casual riders
### Presenting casual riders rides more than member riders everyday
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y= number_of_rides, fill= member_casual))+geom_col(position = "dodge")+
  labs(title="Number of rides comparison", subtitle=paste0("between casual and member riders"))

### Showing the casual riders spend more time riding than member riders. 
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y= average_duration, fill= member_casual))+geom_col(position = "dodge")+
  labs(title="Average duration comparison", subtitle=paste0("between casual and member riders"))



# Act
## Export final results for sharing
counts<-aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = '/cloud/project/avg_ride_length.csv')


