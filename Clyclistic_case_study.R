####################### 
# Install required packages
# tidyverse for data import and wrangling
# lubridate for date functions
# ggplot for visualization 
#######################
install.packages("rmarkdown")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")
setwd("/Users/akshaysinha/Desktop/001_Clyclistic")

#load packages
library(rmarkdown)
library(tidyverse)
library(lubridate)
library(ggplot2)

#=====================
# STEP 1: COLLECT DATA 
#=====================
# Upload Divvy datasets (csv files) here
# Load quarterly dataframes
q2_2019 <- read.csv("Divvy_Trips_2019_Q2.csv")
q3_2019 <- read.csv("Divvy_Trips_2019_Q3.csv")
q4_2019 <- read.csv("Divvy_Trips_2019_Q4.csv")
q1_2020 <- read.csv("Divvy_Trips_2020_Q1.csv")


#==================================================== 
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE 
#==================================================== 
# Compare column names each of the files
# While the names don't have to be in the same order, they DO need to 
# match perfectly before we can use a command to join them into one file


colnames(q2_2019)
colnames(q3_2019)
colnames(q4_2019)
colnames(q1_2020)


# Rename columns to make them consistent with q1_2020 (as this will be the 
# supposed going-forward table design for Divvy)

q2_2019 <- rename(q2_2019, 
                  ride_id = X01...Rental.Details.Rental.ID,
                  rideable_type = X01...Rental.Details.Bike.ID,
                  started_at = X01...Rental.Details.Local.Start.Time,            
                  ended_at = X01...Rental.Details.Local.End.Time,              
                  start_station_id = X03...Rental.Start.Station.ID,                    
                  start_station_name = X03...Rental.Start.Station.Name,                  
                  end_station_id = X02...Rental.End.Station.ID,                      
                  end_station_name = X02...Rental.End.Station.Name,                    
                  member_casual = User.Type)

q3_2019 <- rename(q3_2019,
                  ride_id = trip_id,
                  rideable_type = bikeid,
                  started_at = start_time,
                  ended_at = end_time,
                  start_station_id = from_station_id,
                  start_station_name = from_station_name,
                  end_station_id = to_station_id,
                  end_station_name = to_station_name,
                  member_casual = usertype)
                  
q4_2019 <- rename(q4_2019, 
                  ride_id = trip_id,
                  rideable_type = bikeid,
                  started_at = start_time,
                  ended_at = end_time,
                  start_station_id = from_station_id,
                  start_station_name = from_station_name,
                  end_station_id = to_station_id,
                  end_station_name = to_station_name,
                  member_casual = usertype)

# Inspect the dataframes and look for incongruencies
str(q1_2020)
str(q4_2019)
str(q3_2019)
str(q2_2019)

# Convert ride_id and rideable_type to character so that they can stack correctly 
q4_2019 <- mutate(q4_2019, ride_id = as.character(ride_id), 
                  rideable_type = as.character(rideable_type)) 

q3_2019 <- mutate(q3_2019, ride_id = as.character(ride_id), 
                  rideable_type = as.character(rideable_type)) 

q2_2019 <- mutate(q2_2019, ride_id = as.character(ride_id), 
                  rideable_type = as.character(rideable_type))
                                                                
# Stack individual quarter's dataframes into one big data frame
all_trips <- bind_rows(q2_2019,q3_2019,q4_2019,q1_2020)

# Remove lat, long, birthyear and gender fields from the data as it was dropped
# starting 2020
colnames(all_trips)

all_trips <- all_trips %>% 
  select(-c(X01...Rental.Details.Duration.In.Seconds.Uncapped, Member.Gender, 
            X05...Member.Details.Member.Birthday.Year, gender, start_lat, start_lng, 
            end_lat, end_lng, tripduration, birthyear))

#====================================================== 
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS 
#======================================================

# Inspect the new table that has been created
colnames(all_trips) #List of column names
nrow(all_trips) #How many rows are in data frame?
dim(all_trips) #Dimensions of the data frame?
head(all_trips) #See the first 6 rows of data frame. Also tail(all_trips) 
str(all_trips) #See list of columns and data types (numeric, character, etc) 
summary(all_trips) #Statistical summary of data. Mainly for numeric

# There are a few problems we will need to fix:
# 1). in the "member_casual" column, there are two names for members ("member and 
# and "subscriber") and two names for casual riders ("Customer" and "casual"). 
# We will need to consolidate that from four to two labels. 
unique(all_trips$member_casual)
table(all_trips$member_casual)

# In the "member_casual" column, replace "Subscriber" with "member" and 
# "Customer" with "casual"
all_trips <- all_trips %>% 
  mutate(member_casual = recode(member_casual, 
                                "Subscriber" = "member",
                                "Customer" = "casual"))

# Lets recheck
unique(all_trips$member_casual)
table(all_trips$member_casual)

# 2). The data can only be aggregated at the ride-level, which is too granular.
# We will want to add some additional columns of data -- such as day, month, year -- 
# that provide additional opportunities to aggregate the data.

# Adding date column
all_trips$date <- as.Date(all_trips$started_at)
#Adding year, month, day and day of week column 
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$month <- format(as.Date(all_trips$date), "%B")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# 3). We will want to add a calculated field for length of ride since the q1_2020
# data did not have the "tripduration" column. We will add "ride_length" to the entire 
# dataframe for consistency. 

all_trips$ride_length <- difftime(all_trips$ended_at, all_trips$started_at, 
                                  units = c("secs"))

# Inspect the structure of the columns 
str(all_trips$ride_length)
is.numeric(all_trips$ride_length) #shows false

# Convert "ride_length" from 'difftime' to numeric so we can run 
# calculations on the data.

all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length)) 

# recheck
is.numeric(all_trips$ride_length) # shows true

# 4). There are some rides where "tripduration" shows up as negative, including
# several hundred rides where Divvy took bikes out of circulation for Quality 
# Control reasons. We will want to delete these rides.

# Checking for NUlls, NAs and Negative values 
summary(all_trips) # "end_station_id" shows 1 NA
sum(all_trips$ride_length < 0) # shows 130 "ride_length" values less than 0
sum(all_trips$start_station_name == "HQ QR") # shows 3767 values entered under "HQ QR"

# This needs to removed from the dataframe and make a new version of the dataframe

all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | 
                            all_trips$ride_length < 0),]

#=====================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS 
#=====================================

# Descriptive analysis on ride_length (all figures in seconds)

mean(all_trips_v2$ride_length) # average (total ride length / rides)
median(all_trips_v2$ride_length) # midpoint number in ascending array of ride length
max(all_trips_v2$ride_length)# longest ride 
min(all_trips_v2$ride_length)# shortest ride

# you can condense the four line above to one line using summary() on the specific 
# attribute

summary(all_trips_v2$ride_length)

# Compare members and casual users riding length during the week 

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + 
            all_trips_v2$day_of_week, FUN = mean)

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + 
            all_trips_v2$day_of_week, FUN = median)

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + 
            all_trips_v2$day_of_week, FUN = max)

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual+ 
            all_trips_v2$day_of_week, FUN = min)

# The days of the week are not ordered properly 

all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, 
                            levels = c("Sunday", "Monday", "Tuesday",
                                        "Wednesday", "Thursday", "Friday",
                                        "Saturday"))

# Recheck aggregate
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + 
          all_trips_v2$day_of_week, FUN = mean) 
          # shows table ordered from Sunday through Saturday as the last 
          # day of the week

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean) 

all_trips_v2$month <- ordered(all_trips_v2$month, 
                              levels = c("January", "February", "March", "April", 
                                         "May","June", "July", "August", 
                                         "September", "October","November", 
                                         "December"))

# Analyzing bike riding behavior between Casual Riders and Members on a monthly basis
# Let's save this as a dataframe "monthly_ridership"
# This will make visualizing easier.

monthly_ridership <- all_trips_v2 %>% 
  group_by(member_casual, month, year) %>% 
  summarise(monthly_rides = n(), average_monthly_duration = mean(ride_length)) %>% 
  arrange(year)

ggplot(monthly_ridership, aes(x = month,y = average_monthly_duration, 
                          fill = member_casual)) + 
  geom_col(position = "dodge") + facet_grid(~ year, scales = "free", 
                                            space = "free") +
  labs(title = "Average Time Spent Month to Month", x = "Month", 
       y = "Ride Time (seconds)", fill = "Member Type",
       caption = "Ridership from Q2 2019 - Q1 2020") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), 
        axis.line = element_line(colour = 'black', size = 0.5, linetype = 'solid')) +
  scale_y_continuous(limits = c(0,10000), breaks = seq(0,10000,500))
  
ggsave("Average_time_spent_monthly.png")



ggplot(monthly_ridership, aes(x = month, y = monthly_rides, fill = member_casual)) +
  geom_col(position = "dodge") + 
  facet_grid(~year, scales = "free", space = "free") +
  labs(title = "Monthly Ridership", x = "Month", 
      y = "Number of Riders", fill = "Member Type",
      caption = "Ridership from Q2 2019 - Q1 2020") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), 
        axis.line = element_line(colour = 'black', size = 0.5, linetype = 'solid')) +
  scale_y_continuous(limits = c(0,410000), breaks = seq(0,410000,25000))

ggsave("Monthly_Ridership.png")

# Analyze the ridership data by type and weekday
# Let's save this as a dataframe under "ridership_data_by_type_and_weekday". 


ridership_data_by_type_and_weekday <- all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>% 
  summarise(number_of_ride = n(), average_duration =  mean(ride_length)) %>%  
  arrange(member_casual, weekday)

# Making the visualization of this dataframe 
# Bar Graph
ggplot(ridership_data_by_type_and_weekday, 
       mapping = aes(x = weekday, y = number_of_ride, fill = member_casual)) +
  geom_col(position = "dodge") + 
  labs(title = "Ridership During the week", x = "Day of Week", 
       y = "Number of Riders", fill = "Member Type", 
       caption = "Ridership from Q2 2019 to Q1 2020") +
  theme(plot.title = element_text(hjust = 0.5), axis.line = 
          element_line(colour = 'black', size = 0.5, linetype = 'solid')) +
  scale_y_continuous(limits = c(0,510000), breaks = seq(0,510000,25000)) 

ggsave("Ridership_During_the_Week.png")

# Let's create a visualization for average duration 

ggplot(ridership_data_by_type_and_weekday,
       aes(x = weekday, y = average_duration, fill = member_casual)) + 
  geom_col(position = "dodge") +
  labs(title = "Average Ride Time", x = "Day of Week", 
       y = "Ride Time (Seconds)", fill = "Member Type", 
       caption = "Ridership from Q2 2019 - Q1 2020") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.line = element_line(colour = 'black', 
                                 size = 0.5, linetype = 'solid')) +
  scale_y_continuous(limits = c(0,4000), breaks = seq(0,4000,250))
  
ggsave("Average_daily_duration.png")


#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS 
#=================================================

# Create A csv of "ridership_data_by_type_and_weekday", "monthly_ridership
# and "all_trips_v2"

write.csv(all_trips_v2, 
          file = '~/Desktop/001_Clyclistic/all_trips_v2.csv')

write.csv(monthly_ridership, 
          file = '~/Desktop/001_Clyclistic/monthly_ridership.csv')

write.csv(ridership_data_by_type_and_weekday, 
          file = '~/Desktop/001_Clyclistic/ridership_data_by_type_and_weekday.csv')
