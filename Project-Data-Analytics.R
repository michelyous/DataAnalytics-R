#1 DataCollection
# Set a new working directory
setwd("C:/Users/vitto/Desktop/R-projects/DataAnalytics/DataSets_uber-pickups-in-new-york-city")

uber_data_april = read.csv("uber-raw-data-apr14.csv")
uber_data_april
summary(uber_data_april)
str(uber_data_april)  # This will show your current working directory



uber_data_aug = read.csv("uber-raw-data-aug14.csv")
uber_data_aug
summary(uber_data_aug)
str(uber_data_aug)

uber_data_july = read.csv("uber-raw-data-jul14.csv")
uber_data_july
summary(uber_data_july)
str(uber_data_july)

uber_data_june = read.csv("uber-raw-data-jun14.csv")
uber_data_june
summary(uber_data_june)
str(uber_data_june)

uber_data_may = read.csv("uber-raw-data-may14.csv")
uber_data_may
summary(uber_data_may)
str(uber_data_may)

uber_data_sep = read.csv("uber-raw-data-sep14.csv")
uber_data_sep
summary(uber_data_sep)
str(uber_data_sep)

uber_data_janjune = read.csv("uber-raw-data-janjune-15.csv")
uber_data_janjune
summary(uber_data_janjune)
str(uber_data_janjune)

# Combine All the data together  
uber_data_2014 <- rbind(uber_data_april, uber_data_aug, uber_data_july,uber_data_june,uber_data_may,uber_data_sep)
uber_data_2014
# Make sure that all the date is combined
cat("The dimensions of all data are:", dim(uber_data_2014))

#2 CLEANING DATA: CHECK FOR UNIQUENESS AND NAN-VALUES

#----------------------------------------------------------------------------------
#NAN-values

# Check for missing values in each column
missing_values <- colSums(is.na(uber_data_2014))

# Display the count of missing values in each column
print("Missing values in each column:")
print(missing_values)

#Since there is no missing value we do not change anything

#-----------------------------------------------------------------------------------
#UNIQUENESS
# Identify duplicate rows
duplicated_rows <- duplicated(uber_data_2014)

# Count the number of duplicate rows
num_duplicates <- sum(duplicated_rows)


# Check if there are any duplicate rows
if (num_duplicates > 0) {
  # Display duplicate rows
  print("Number of duplicate rows:")
  print(num_duplicates)
  
  
} else {
  print("No duplicate rows.")
}

# Remove duplicated rows
uber_data_2014_cleaned <- unique(uber_data_2014)

# Identify duplicate rows
duplicated_rows <- duplicated(uber_data_2014_cleaned)

# Count the number of duplicate rows
num_duplicates <- sum(duplicated_rows)

# Check if the new df contains  any duplicate rows
if (num_duplicates > 0) {
  # Display duplicate rows
  print("Number of duplicate rows:")
  print(num_duplicates)
  
} else {
  print("No duplicate rows.")
}

#test


#3 data analysis

uber_data_2014$Date.Time <- as.POSIXct(uber_data_2014$Date.Time, format = "%Y-%m-%d %H:%M:%S")
#extract day and month from date and add it new column
uber_data_2014$day <- as.Date(uber_data_2014$Date.Time, format = "%Y-%m-%d")
uber_data_2014$month <- format(uber_data_2014$Date.Time, "%Y-%m")

install.packages("dplyr")
library(dplyr)

#Daily Rides
daily_rides <- uber_data_2014 %>%
  group_by(day) %>%
  summarise(number_of_rides = n())

#Monthly Rides
monthly_rides <- uber_data_2014 %>%
  group_by(month) %>%
  summarise(number_of_rides = n())

print(daily_rides)
print(monthly_rides)

#4 Data Visualization

#visualizing daily number of rides for month April (example)
barplot(daily_rides, main = "Daily Number of Rides", xlab = "Date", ylab = "Number of Rides", col = "skyblue")

#visualizing monthly number of rides 
barplot(monthly_rides, main = "Monthly Number of Rides", xlab = "Month", ylab = "Number of Rides", col = "lightcoral")

#5 information extraction 
head(uber_data_2014)
uber_data_2014$Date.Time <- as.POSIXct(uber_data_2014$Date.Time, format="%d/%m/%Y %H:%M:%S")

uber_data_2014$Day <- format(uber_data_2014$Date.Time, "%d")
uber_data_2014$Month <- format(uber_data_2014$Date.Time, "%m")
uber_data_2014$Year <- format(uber_data_2014$Date.Time, "%Y")
uber_data_2014$Hour <- format(uber_data_2014$Date.Time, "%H")

#Number of rides per hour
hourly_rides <- aggregate(Date.Time ~ Hour, data = uber_data_2014, FUN = length)

#Find the top 5 most frequent pickup locations
library(stats)
hotspots <- aggregate(cbind(Lat, Lon) ~ Lat + Lon, data = uber_data_2014, FUN = length)
hotspots <- hotspots[order(-hotspots$Lat), ][1:5, ]

#print results
print(hourly_rides)
print(hotspots)

#visualizing number of rides per hour
plot(hourly_rides$Hour, hourly_rides$Date.Time, type = 'b', 
     xlab = 'Hour of the Day', ylab = 'Number of Rides', main = 'Hourly Rides')


#6 Average Passenger Determination

# Convert Date.Time to Date
uber_data_2014$Date <- as.Date(uber_data_2014$Date.Time, format="%m/%d/%Y %H:%M:%S")

# Aggregate data to count rides per day
daily_ride_counts <- aggregate(Date.Time ~ Date, data = uber_data_2014, FUN = length)

# Rename the aggregated column for clarity
colnames(daily_ride_counts)[2] <- "RideCount"

# Calculate the average number of rides per day
average_rides_per_day <- mean(daily_ride_counts$RideCount)

# Print the average
cat("Average Number of Rides (Passengers) Per Day:", average_rides_per_day, "\n")

#7 Peak Hour Identification:

#peak hours 
uber_data_2014$hour <- as.numeric(format(uber_data_2014$Date.Time, "%H"))
rides_per_hour <- table(uber_data_2014$hour)
rides_per_hour

#visualizing peak hours 
plot(rides_per_hour, type = "o", col = "orange", xlab = "Hour of the Day", ylab = "Number of Rides", main = "Peak Hours Analysis")

####Calculate the integral under the curve of the peak analysis graph
start_hour <- 12
end_hour <- 24

# Function to calculate the trapezoidal area under the curve
trapezoidal_area <- function(y, h) {
  0.5 * sum(h * (y[-1] + y[-length(y)]))
}

# Calculate the indices for the specific interval
interval_indices <- start_hour: end_hour

# Extract the counts for the specific interval
specific_counts <- as.numeric(rides_per_hour[interval_indices])

# Extract the counts for the entire range
all_counts <- as.numeric(rides_per_hour)

# Calculate the trapezoidal area for the specific interval
specific_area <- trapezoidal_area(specific_counts, 1)

# Calculate the trapezoidal area for the total range
total_area <- trapezoidal_area(all_counts, 1)

# Calculate the percentage
percentage_specific_area <- (specific_area / total_area) * 100

# Print the results
cat("Specific Area:", specific_area, "\n")
cat("Total Area:", total_area, "\n")
cat("Percentage of Total Area:", percentage_specific_area, "%\n")


#8 Peak Day Analysis

#April
# Convert Date.Time to Date format and extract the day
uber_data_april$Date <- as.Date(uber_data_april$Date.Time, format="%m/%d/%Y")
uber_data_april$Day <- as.integer(format(uber_data_april$Date, "%d"))

# Aggregate data to count rides per day
ride_counts <- table(uber_data_april$Day)

# Identify the day with the highest number of rides
peak_day <- which.max(ride_counts)
peak_rides <- ride_counts[peak_day]

# Print the peak day and the number of rides
cat("April: \n","Peak Day:", peak_day, "\nNumber of Rides:", peak_rides, "\n")

# Convert ride_counts to a data frame
ride_data <- data.frame(Day = as.integer(names(ride_counts)), Rides = as.integer(ride_counts))

# Load ggplot2
library(ggplot2)

# Create the plot
ggplot(ride_data, aes(x = Day, y = Rides)) +
  geom_bar(stat = "identity", fill = ifelse(ride_data$Day == peak_day, "red", "blue")) +
  geom_text(data = subset(ride_data, Day == peak_day), aes(label = Rides, vjust = -0.5)) +
  theme_minimal() +
  labs(title = "Uber Rides per Day in April", x = "Day of the Month", y = "Number of Rides")



#August

library(ggplot2)

# Convert Date.Time to Date format and extract the day
uber_data_aug$Date <- as.Date(uber_data_aug$Date.Time, format="%m/%d/%Y")
uber_data_aug$Day <- as.integer(format(uber_data_aug$Date, "%d"))

# Aggregate data to count rides per day
ride_counts <- table(uber_data_aug$Day)

# Identify the day with the highest number of rides
peak_day <- which.max(ride_counts)
peak_rides <- ride_counts[peak_day]

# Print the peak day and the number of rides
cat("August: \n","Peak Day:", peak_day, "\nNumber of Rides:", peak_rides, "\n")

# Convert ride_counts to a data frame
ride_data <- data.frame(Day = as.integer(names(ride_counts)), Rides = as.integer(ride_counts))

# Create the plot
ggplot(ride_data, aes(x = Day, y = Rides)) +
  geom_bar(stat = "identity", fill = ifelse(ride_data$Day == peak_day, "red", "blue")) +
  geom_text(data = subset(ride_data, Day == peak_day), aes(label = Rides, vjust = -0.5)) +
  theme_minimal() +
  labs(title = "Uber Rides per Day in August", x = "Day of the Month", y = "Number of Rides")


#July
# Convert Date.Time to Date format and extract the day
uber_data_july$Date <- as.Date(uber_data_july$Date.Time, format="%m/%d/%Y")
uber_data_july$Day <- as.integer(format(uber_data_july$Date, "%d"))

# Aggregate data to count rides per day
ride_counts <- table(uber_data_july$Day)

# Identify the day with the highest number of rides
peak_day <- which.max(ride_counts)
peak_rides <- ride_counts[peak_day]

# Print the peak day and the number of rides
cat("July: \n","Peak Day:", peak_day, "\nNumber of Rides:", peak_rides, "\n")

# Convert ride_counts to a data frame
ride_data <- data.frame(Day = as.integer(names(ride_counts)), Rides = as.integer(ride_counts))

# Create the plot
ggplot(ride_data, aes(x = Day, y = Rides)) +
  geom_bar(stat = "identity", fill = ifelse(ride_data$Day == peak_day, "red", "blue")) +
  geom_text(data = subset(ride_data, Day == peak_day), aes(label = Rides, vjust = -0.5)) +
  theme_minimal() +
  labs(title = "Uber Rides per Day in July", x = "Day of the Month", y = "Number of Rides")


#June
# Convert Date.Time to Date format and extract the day
uber_data_june$Date <- as.Date(uber_data_june$Date.Time, format="%m/%d/%Y")
uber_data_june$Day <- as.integer(format(uber_data_june$Date, "%d"))

# Aggregate data to count rides per day
ride_counts <- table(uber_data_june$Day)

# Identify the day with the highest number of rides
peak_day <- which.max(ride_counts)
peak_rides <- ride_counts[peak_day]

# Print the peak day and the number of rides
cat("June: \n","Peak Day:", peak_day, "\nNumber of Rides:", peak_rides, "\n")

# Convert ride_counts to a data frame
ride_data <- data.frame(Day = as.integer(names(ride_counts)), Rides = as.integer(ride_counts))

# Create the plot
ggplot(ride_data, aes(x = Day, y = Rides)) +
  geom_bar(stat = "identity", fill = ifelse(ride_data$Day == peak_day, "red", "blue")) +
  geom_text(data = subset(ride_data, Day == peak_day), aes(label = Rides, vjust = -0.5)) +
  theme_minimal() +
  labs(title = "Uber Rides per Day in June", x = "Day of the Month", y = "Number of Rides")


#May 
# Convert Date.Time to Date format and extract the day
uber_data_may$Date <- as.Date(uber_data_may$Date.Time, format="%m/%d/%Y")
uber_data_may$Day <- as.integer(format(uber_data_may$Date, "%d"))

# Aggregate data to count rides per day
ride_counts <- table(uber_data_may$Day)

# Identify the day with the highest number of rides
peak_day <- which.max(ride_counts)
peak_rides <- ride_counts[peak_day]

# Print the peak day and the number of rides
cat("May: \n","Peak Day:", peak_day, "\nNumber of Rides:", peak_rides, "\n")

# Convert ride_counts to a data frame
ride_data <- data.frame(Day = as.integer(names(ride_counts)), Rides = as.integer(ride_counts))

# Create the plot
ggplot(ride_data, aes(x = Day, y = Rides)) +
  geom_bar(stat = "identity", fill = ifelse(ride_data$Day == peak_day, "red", "blue")) +
  geom_text(data = subset(ride_data, Day == peak_day), aes(label = Rides, vjust = -0.5)) +
  theme_minimal() +
  labs(title = "Uber Rides per Day in May", x = "Day of the Month", y = "Number of Rides")

#September
library(ggplot2)
# Convert Date.Time to Date format and extract the day
uber_data_sep$Date <- as.Date(uber_data_sep$Date.Time, format="%m/%d/%Y")
uber_data_sep$Day <- as.integer(format(uber_data_sep$Date, "%d"))

# Aggregate data to count rides per day
ride_counts <- table(uber_data_sep$Day)

# Identify the day with the highest number of rides
peak_day <- which.max(ride_counts)
peak_rides <- ride_counts[peak_day]

# Print the peak day and the number of rides
cat("September: \n","Peak Day:", peak_day, "\nNumber of Rides:", peak_rides, "\n")

# Convert ride_counts to a data frame
ride_data <- data.frame(Day = as.integer(names(ride_counts)), Rides = as.integer(ride_counts))

# Create the plot
ggplot(ride_data, aes(x = Day, y = Rides)) +
  geom_bar(stat = "identity", fill = ifelse(ride_data$Day == peak_day, "red", "blue")) +
  geom_text(data = subset(ride_data, Day == peak_day), aes(label = Rides, vjust = -0.5)) +
  theme_minimal() +
  labs(title = "Uber Rides per Day in September", x = "Day of the Month", y = "Number of Rides")


#9 Association rules using Apriori algorithm 
install.packages("arules")
library(arules)

uber_data_2014

transactions <- as(uber_data_2014[, c("month", "hour", "Lat", "Lon", "Base")], "transactions")

rules <- apriori(transactions, parameter = list(support = 0.001, confidence = 0.6))

inspect(rules)
rules


#10 Data dimensioning
# from the association rules : 

# *Certain hours like : 12h and 18h 00h are frequently associated with a certain locations such as
# Lon=[-73.97,-72.07]} => Lat=[40.8,42.1] 
# 
# *certain hours like : 12h, 18h and 00h are associated with certain bases such as : Base=B02512,  
# Base=B02682, Base=B02598.
# 
# *at 00h, the actif bases are : Base=B02764, Base=B02617, Base=B02512
# 
# *Peak horus for the following months : 04, 03, 02, 08, 05, 11, 10, 09 is 12h and 18h 
# while for the following months : 06, 02, 05 is 00h and 12h.


#scatter plot of the association rules 
quality(rules)$support
quality(rules)$confidence

plot(quality(rules)$support, quality(rules)$confidence, xlab = "Support", ylab = "Confidence", col = "blue", main = "Association Rules Scatterplot")

#frequent itemsets using Eclat method  
frequent_itemsets <- eclat(transactions, parameter = list(supp = 0.1))
inspect(frequent_itemsets)

frequent_df <- as(frequent_itemsets, "data.frame")

# Plotting support values
barplot(frequent_df$support, names.arg = frequent_df$items,
        main = "Support of Frequent Itemsets",
        xlab = "Frequent Itemsets", ylab = "Support",
        col = "skyblue", cex.names = 0.7)

#11 Prediction model :
# Load necessary libraries
library(lubridate)
library(dplyr)
library(caret)

# Convert Date.Time to POSIXct and create additional time features
uber_data_2014 <- uber_data_2014 %>%
  mutate(
    Date.Time = ymd_hms(Date.Time),
    Hour = hour(Date.Time),
    Weekday = wday(Date.Time),
    Month = month(Date.Time)
  )

# Aggregate data to get trip count per hour (or another suitable metric)
uber_data_aggregated <- uber_data_2014 %>%
  group_by(Hour, Weekday, Month) %>%
  summarize(TripCount = n())

# Split data into independent variables (X) and dependent variable (y)
X <- uber_data_aggregated %>% select(-TripCount)
y <- uber_data_aggregated$TripCount

# Split data into training and testing sets
set.seed(123) # For reproducibility
trainIndex <- createDataPartition(y, p = 0.8, list = FALSE)
train_X <- X[trainIndex, ]
train_y <- y[trainIndex]
test_X <- X[-trainIndex, ]
test_y <- y[-trainIndex]

# Fit a model, for example, a linear regression model
model <- train(train_X, train_y, method = "lm")


#12 Advanced Peak Hour analysis
head(uber_data_2014)
uber_data_2014$Date.Time <- as.POSIXct(uber_data_2014$Date.Time, format = "%Y-%m-%d %H:%M:%S")
uber_data_2014$Hour <- as.integer(format(uber_data_2014$Date.Time, "%H"))
uber_data_2014$Weekday <- weekdays(uber_data_2014$Date.Time)
uber_data_2014$Date <- as.Date(uber_data_2014$Date.Time)
library(ggplot2)

#Ride Counts by weekdays
weekday_counts <- table(uber_data_2014$Weekday)
weekday_df <- as.data.frame(weekday_counts)
names(weekday_df) <- c("Weekday", "Frequency")
ggplot(data = weekday_df, aes(x = Weekday, y = Frequency)) +
  geom_bar(stat = "identity") +
  labs(title = "Ride Counts by Weekday", x = "Day of the Week", y = "Number of Rides") +
  theme_minimal()

#Geospatial Analysis


'''
# Simple approach: count rides per rounded lat/lon
# Aggregate ride counts by rounded latitude and longitude
location_counts <- aggregate(cbind(Count = uber_data_2014$Lat) ~ round(Lat, 2) + round(Lon, 2), data = uber_data_2014, FUN = length)
# Correct the column names if necessary
names(location_counts) <- c("Latitude", "Longitude", "Count")
# Plot the high-demand locations
plot(location_counts$Latitude, location_counts$Longitude, main = "High-Demand Locations", xlab = "Latitude", ylab = "Longitude", pch = 19, col = "blue")


### SAME PLOT AS BEFORE BUT WITH A CLEARLY INDICATION ON WHIC IS MORE USED
# Install and load necessary packages (if not already installed)
if (!require("viridis")) install.packages("viridis", dependencies=TRUE)
library(viridis)

# Aggregate ride counts by rounded latitude and longitude
location_counts <- aggregate(cbind(Count = uber_data_2014$Lat) ~ round(Lat, 2) + round(Lon, 2), data = uber_data_2014, FUN = length)

# Correct the column names if necessary
names(location_counts) <- c("Latitude", "Longitude", "Count")

# Normalize count for better color representation
location_counts$Count <- log(location_counts$Count + 1)

# Create a scatter plot with color intensity representing ride counts
plot(location_counts$Latitude, location_counts$Longitude, 
     main = "High-Demand Locations", 
     xlab = "Latitude", ylab = "Longitude", 
     pch = 19, 
     col = viridis(100)[cut(location_counts$Count, breaks = 100)])

# Invert the x and y axes
axis(1, at = pretty(location_counts$Latitude), labels = pretty(location_counts$Latitude), cex.axis = 0.2)
axis(2, at = pretty(location_counts$Longitude), labels = pretty(location_counts$Longitude), cex.axis = 0.2)

# Add a color legend
legend("topright", legend = "Ride Count", fill = viridis(100), cex = 0.7, title = "Count", box.col = "white")
'''



# Install and load necessary packages (if not already installed)
if (!require("viridis")) install.packages("viridis", dependencies=TRUE)
if (!require("leaflet")) install.packages("leaflet", dependencies=TRUE)

library(viridis)
library(leaflet)

# Aggregate ride counts by rounded latitude and longitude
location_counts <- aggregate(cbind(Count = uber_data_2014$Lat) ~ round(Lat, 2) + round(Lon, 2), data = uber_data_2014, FUN = length)

# Correct the column names if necessary
names(location_counts) <- c("Latitude", "Longitude", "Count")

# Normalize count for better color representation
location_counts$Count <- log(location_counts$Count + 1)

# Sort by count in descending order
location_counts <- location_counts[order(-location_counts$Count), ]

# Calculate the cumulative percentage of rides
location_counts$CumulativePercent <- cumsum(location_counts$Count) / sum(location_counts$Count) * 100

# Find the number of locations needed to reach or exceed 75%
num_locations_75_percent <- which(location_counts$CumulativePercent >= 75)[1]

# Select the top locations that aggregate 75% of all rides
top_locations_75_percent <- location_counts[1:num_locations_75_percent, ]

# Calculate the percentage of total rides that the top locations make
percentage_top_locations <- sum(top_locations_75_percent$Count) / sum(location_counts$Count) * 100

# Categorize locations based on usage
usage_categories <- cut(top_locations_75_percent$Count, breaks = c(0, 300, 600, Inf), labels = c("Low Frequency", "Medium Frequency", "High Frequency"))

# Assign colors based on usage categories
colors <- viridis(length(levels(usage_categories)))

# Limit to the top 1000 locations
top_1000_locations <- top_locations_75_percent[1:1000, ]

# Calculate the percentage of total rides that the top 1000 locations make
percentage_top_1000 <- sum(top_1000_locations$Count) / sum(location_counts$Count) * 100

# Create breaks based on the usage categories
breaks <- c(0, quantile(top_1000_locations$Count, 0.3), quantile(top_1000_locations$Count, 0.6), Inf)

# Categorize locations based on usage
usage_categories_1000 <- cut(top_1000_locations$Count, breaks = breaks, labels = c("Low Frequency", "Medium Frequency", "High Frequency"))

# Create a leaflet map with colored markers
m <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    lng = top_1000_locations$Longitude,
    lat = top_1000_locations$Latitude,
    popup = paste("Count: ", top_1000_locations$Count, "<br>Category: ", usage_categories_1000),
    color = colors[as.numeric(usage_categories_1000)],
    fillOpacity = 0.8
  ) %>%
  addLegend(
    "topright",
    colors = colors,
    labels = c("Low Frequency", "Medium Frequency", "High Frequency"),
    title = paste("Frequently used Pick-Up points")
  )

# Print the map
print(m)
print(round(percentage_top_1000, 2))


#public events


#holidays


#weather conditions
file_path_weather_conditions <- "weather.csv"

# Read the CSV file into a data frame
weather_data <- read.csv(file_path_weather_conditions)
weather_data$time_stamp <- as.POSIXct(weather_data$time_stamp, origin = "1970-01-01", tz = "UTC")
weather_data$time_stamp <- as.Date(weather_data$time_stamp)
# Display the updated structure of the data frame
str(weather_data)

###################################################################


#Segmantation using clustering
install.packages("cluster")
library(cluster)
#The cluster library provides clustering algorithms. In this case k-means is used.
#clustering is performed on lat and long
# Example: Clustering based on geographical data
set.seed(123) # For reproducibility
kmeans_result <- kmeans(uber_data_2014[, c('Lat', 'Lon')], centers=5)
# Adding cluster information to the dataset
uber_data_2014$Cluster <- kmeans_result$cluster

#Analyzing Segments
library(ggplot2)
ggplot(uber_data_2014, aes(x=Hour, fill=factor(Cluster))) + 
  geom_histogram(binwidth=1, position="dodge") + 
  labs(fill="Cluster", x="Hour of the Day", y="Count") +
  ggtitle("Trip Patterns by Cluster")

#Tailoring Strategies Based on Segments
# Example: Segmenting based on time of day and frequency
uber_data_2014$User_Type <- ifelse(uber_data_2014$Hour >= 6 & uber_data_2014$Hour <= 18, 'Day User', 'Night User')
uber_data_2014$Frequency <- ifelse(uber_data_2014$Day <= 15, 'First Half of Month', 'Second Half of Month')
#This code segments the data by time of day ('Day User' vs. 'Night User')
library(dplyr)
filtered_data <- uber_data_2014 %>% 
  filter(!is.na(User_Type) & !is.na(Frequency))
ggplot(filtered_data, aes(x=User_Type, fill=Frequency)) + 
  geom_bar(position="dodge") + 
  labs(fill="Frequency", x="User Type", y="Count") +
  ggtitle("Day and Night Users by Frequency")


# Analyze segments
#Day and Night users
table(uber_data_2014$User_Type, uber_data_2014$Frequency)
#Each cluster (1, 2, 3, 4, 5) represents a group of trips that are
#geographically close to each other.
#The histogram shows how trip patterns vary by hour for each cluster
#A high bar in cluster suggests a high number of trips in that geographical area around the time we see in x-axis

#13: Resource Allocation Optimization
install.packages("lubridate")
library(lubridate)
library(ggplot2)
library(forecast)

# Convert Date.Time to POSIXct
uber_data_2014$Date.Time <- ymd_hms(uber_data_2014$Date.Time)

# Extracting date and time features
uber_data_2014$Hour <- hour(uber_data_2014$Date.Time)
uber_data_2014$Weekday <- wday(uber_data_2014$Date.Time, label=TRUE)
uber_data_2014$Month <- month(uber_data_2014$Date.Time, label=TRUE)

# Analysis: Count trips by hour or by location
trip_count_by_hour <- table(uber_data_2014$Hour)
trip_count_by_location <- table(uber_data_2014$Lat, uber_data_2014$Lon)

# Predictive Model: For simplicity, using a time series model for hourly data
ts_data <- ts(trip_count_by_hour, frequency=24)

# Fit a time series model (like ARIMA)
fit <- auto.arima(ts_data)

# Forecast the next 24 hours
forecast <- forecast(fit, h=24)

# Plot the forecast
plot(forecast)


#14: Customer Segmentation
#Data Preparation
library(lubridate)
uber_data_2014$Date.Time <- ymd_hms(uber_data_2014$Date.Time)
uber_data_2014$Hour <- hour(uber_data_2014$Date.Time)
uber_data_2014$Day <- day(uber_data_2014$Date.Time)
uber_data_2014$Weekday <- wday(uber_data_2014$Date.Time, label=TRUE)

#Segmantation using clustering
install.packages("cluster")
library(cluster)
# Example: Clustering based on geographical data
set.seed(123) # For reproducibility
kmeans_result <- kmeans(uber_data_2014[, c('Lat', 'Lon')], centers=5)
# Adding cluster information to the dataset
uber_data_2014$Cluster <- kmeans_result$cluster

#Analyzing Segments
library(ggplot2)
ggplot(uber_data_2014, aes(x=Hour, fill=factor(Cluster))) + 
  geom_histogram(binwidth=1, position="dodge") + 
  labs(fill="Cluster", x="Hour of the Day", y="Count") +
  ggtitle("Trip Patterns by Cluster")

#Tailoring Strategies Based on Segments
# Example: Segmenting based on time of day and frequency
uber_data_2014$User_Type <- ifelse(uber_data_2014$Hour >= 6 & uber_data_2014$Hour <= 18, 'Day User', 'Night User')
uber_data_2014$Frequency <- ifelse(uber_data_2014$Day <= 15, 'First Half of Month', 'Second Half of Month')

# Analyze segments
#Day and Night users
table(uber_data_2014$User_Type, uber_data_2014$Frequency)


#15: Predictive Model Evaluation

# Make predictions and evaluate the model
predictions <- predict(model, test_X)
rmse_val <- RMSE(predictions, test_y)
r2_val <- R2(predictions, test_y)

# Print evaluation results
print(paste("RMSE:", rmse_val))
print(paste("R-squared:", r2_val))

residuals <- test_y - predictions
plot(residuals, type = 'l', main = "Residuals Plot", xlab = "Index", ylab = "Residuals")
