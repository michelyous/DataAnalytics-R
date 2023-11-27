#STEP 1: DataCollection
uber_data_april = read.csv("uber-raw-data-apr14.csv")
uber_data_april
summary(uber_data_april)
str(uber_data_april)

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

#STEP 2: Data Cleaning

# Combine all data except janjune because janjune has different and additional column
uber_data_14 <- rbind(uber_data_april, uber_data_aug, uber_data_july,uber_data_june,uber_data_may,uber_data_sep)

# Make sure that all the date is combined
cat("The dimensions of all data are:", dim(uber_data_2014))

#CLEANING DATA: CHECK FOR UNIQUENESS AND NAN_VALUES

#NAN VALUES

# Check for missing values in each column
missing_values <- colSums(is.na(uber_data_14))

# Display the count of missing values in each column
print("Missing values in each column:")
print(missing_values)

#Since there is no missing value we do not change anything

#UNIQUENESS
#Identify duplicate rows
duplicated_rows <- duplicated(uber_data_14)

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
uber_data_14_cleaned <- unique(uber_data_14)

# Identify duplicate rows
duplicated_rows <- duplicated(uber_data_14_cleaned)

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

#Average Passenger Determination:

#Ensure that the 'date' column is of type Date
uber_data_14_cleaned$Date.Time <- as.Date(uber_data_14_cleaned$Date.Time)

# Calculate the daily count of Uber trips using the table function
# table: Count the occurrences of each unique date in the 'date' column
daily_trip_counts <- table(uber_data_14_cleaned$Date.Time)

# Create a new dataframe with the results
# names(daily_trip_counts): Extract the unique dates from the result of table
# as.numeric(): Convert the count values to numeric
# date: The unique dates
# trip_counts: The count of Uber trips for each date
result_data <- data.frame(date = as.Date(names(daily_trip_counts)), trip_counts = as.numeric(daily_trip_counts))

# Display the results
print(result_data)
summary(result_data)
str(result_data)

#STEP 3: Data Analysis

head(uber_data_april)
uber_data_april$Date.Time <- as.POSIXct(uber_data_april$Date.Time, format="%d/%m/%Y %H:%M:%S")

#daily number of rides : 
uber_data_april$day <- format(uber_data_april$Date.Time, "%Y-%m-%d")
daily_rides <- table(uber_data_april$day)
daily_rides

#monthly number of rides
uber_data_april$month <- format(uber_data_april$Date.Time, "%Y-%m")
monthly_rides <- table(uber_data_april$month)
monthly_rides

#peak hours  
uber_data_april$hour <- as.numeric(format(uber_data_april$Date.Time, "%H"))
rides_per_hour <- table(uber_data_april$hour)

#Step 4: Data Visualization

#visualizing daily number of rides 
barplot(daily_rides, main = "Daily Number of Rides", xlab = "Date", ylab = "Number of Rides", col = "skyblue")

#visualizing monthly number of rides 
barplot(monthly_rides, main = "Monthly Number of Rides", xlab = "Month", ylab = "Number of Rides", col = "lightcoral")

#visualizing peak hours 
plot(rides_per_hour, type = "o", col = "orange", xlab = "Hour of the Day", ylab = "Number of Rides", main = "Peak Hours Analysis")
