
# Importing the required libraries and packages
library(tidyverse)
library(lubridate)

## Data collection

bikeshare_data_2023 <- list.files(path = 'C:/Users/13475/Documents/my docs/Google Data Analytics Course/course 8/divvy-tripdata/202207-202307-divvy-tripdata',
                                  pattern = "*.csv", full.names = TRUE) |>  
                        lapply(read_csv) |> 
                        bind_rows()

## Data Wrangling

# removing columns not required
bikeshare_data_2023 <- subset(bikeshare_data_2023, select = -c(start_lat, start_lng, end_lat, end_lng))

#Viewing the dataset
glimpse(bikeshare_data_2023)
summary(bikeshare_data_2023)
head(bikeshare_data_2023)

# Transforming datetime variables and calculating ride length

bikeshare_data_2023 <- bikeshare_data_2023 |> 
                        mutate(day_of_week = format(as.Date(started_at), "%A")) |> 
                        mutate(year = format(as.Date(started_at), "%Y")) |> 
                        mutate(month = format(as.Date(started_at), "%B")) |> 
                        mutate(day = format(as.Date(started_at), "%d")) |> 
                        mutate(ride_length = difftime(ended_at, started_at)/60)

bikeshare_data_2023 <- bikeshare_data_2023 |> 
                        mutate(ride_length_mins = as.numeric(ride_length))

glimpse(bikeshare_data_2023)     

bikeshare_data_2023 <- subset(bikeshare_data_2023, select = -ride_length)
                              
# removing negative and zero ride duration

bikeshare_data_2023 <- bikeshare_data_2023[!(bikeshare_data_2023$ride_length_mins <= 0), ]

# removing null values

bikeshare_data_2023 <- na.omit(bikeshare_data_2023)

# removing duplicates
bikeshare_data_2023 <- distinct(bikeshare_data_2023)

### Data Analysis

# Calculating mean, median, maximum and minimum ride length values


bikeshare_data_2023 |> 
  summarise(avg_ride_mins = mean(ride_length_mins), median_ride_mins = median(ride_length_mins), max_ride_mins =  max(ride_length_mins), min_ride_mins = min(ride_length_mins))
  

# Calculating mean, median, maximum and minimum ride length values based on rider type

bikeshare_data_2023 |> 
  group_by(member_casual) |> 
  summarise(avg_ride_mins = mean(ride_length_mins), median_ride_mins = median(ride_length_mins), max_ride_mins =  max(ride_length_mins), min_ride_mins = min(ride_length_mins))

# Visualizing number of riders of casual riders and members

ggplot(bikeshare_data_2023, aes(x = member_casual, fill = member_casual))+
  geom_bar() +
  labs(title = "Number of casual riders and members", x = "Casual vs Members", y = "Number of Rides")+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
      
  
# Sorting day of week in order

bikeshare_data_2023$day_of_week <- ordered(bikeshare_data_2023$day_of_week, levels= c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Summarizing number of rides and average ride length for both rider type

bikeshare_data_2023 |> 
  group_by(member_casual, day_of_week) |> 
  summarise(number_of_rides = n(), avg_ride_mins = mean(ride_length_mins, .groups = "drop")) |> 
  arrange(member_casual, day_of_week)

# Visualizing weekly number of rides by membership type

bikeshare_data_2023 |> 
  group_by(member_casual, day_of_week) |> 
  summarise(number_of_rides = n(), .groups = 'drop') |> 
  arrange(member_casual, day_of_week) |> 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Weekly Number of Rides by Membership Type ", y = 'Number of Rides', x = 'Day of Week') +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
  
# Visualizing average weekly ride duration by membership type

bikeshare_data_2023 |> 
  group_by(member_casual, day_of_week) |> 
  summarise(avg_ride_length_mins = mean(ride_length_mins), .groups = 'drop') |> 
  ggplot(aes(x = day_of_week, y = avg_ride_length_mins, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Average Weekly Ride Duration by Membership Type", y = 'Average Ride Length in Mins', x = 'Day of Week') 
  
# Sorting the month by order

bikeshare_data_2023$month <- ordered(bikeshare_data_2023$month, 
                                levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

# Summarizing number of rides and average ride duration by membership type and month

bikeshare_data_2023 %>% 
  group_by(member_casual, month) %>%  
  summarise(number_of_rides = n(), avg_ride_mins = mean(ride_length_mins), .groups="drop") %>% 
  arrange(member_casual, month)

# Visualizing monthly ride by membership

bikeshare_data_2023 %>%  
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n(),.groups="drop") %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  labs(title ="Monthly Rides by Membership", x = "Month", y= "Number Of Rides") +
  theme(axis.text.x = element_text(angle = 45)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

# Summarizing number of rides by rideable type

bikeshare_data_2023 %>%
  group_by(rideable_type) %>% 
  summarise(count = length(ride_id))

# Visualizing number of rides for members and casual riders based on rideable

ggplot(bikeshare_data_2023, aes(x=rideable_type, fill=member_casual)) +
  labs(x="Type of Rideable", title="Total Rides for Members and Casual Riders Based on Rideable") +
  geom_bar() +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
