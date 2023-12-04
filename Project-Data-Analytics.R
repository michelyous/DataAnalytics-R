#DataCollection
uber_data_april = read.csv("C:/Users/dell/Downloads/UberDatasets/uber-raw-data-apr14.csv")
uber_data_april
summary(uber_data_april)
str(uber_data_april)

uber_data_aug = read.csv("C:/Users/dell/Downloads/UberDatasets/uber-raw-data-aug14.csv")
uber_data_aug
summary(uber_data_aug)
str(uber_data_aug)

uber_data_july = read.csv("C:/Users/dell/Downloads/UberDatasets/uber-raw-data-jul14.csv")
uber_data_july
summary(uber_data_july)
str(uber_data_july)

uber_data_june = read.csv("C:/Users/dell/Downloads/UberDatasets/uber-raw-data-jun14.csv")
uber_data_june
summary(uber_data_june)
str(uber_data_june)

uber_data_may = read.csv("C:/Users/dell/Downloads/UberDatasets/uber-raw-data-may14.csv")
uber_data_may
summary(uber_data_may)
str(uber_data_may)

uber_data_sep = read.csv("C:/Users/dell/Downloads/UberDatasets/uber-raw-data-sep14.csv")
uber_data_sep
summary(uber_data_sep)
str(uber_data_sep)

uber_data_janjune = read.csv("C:/Users/dell/Downloads/DataSets_uber-pickups-in-new-york-city/uber-raw-data-janjune-15.csv")
uber_data_janjune
summary(uber_data_janjune)
str(uber_data_janjune)

# Combine All the data together  
uber_data_2014 <- rbind(uber_data_april, uber_data_aug, uber_data_july,uber_data_june,uber_data_may,uber_data_sep)
uber_data_2014
# Make sure that all the date is combined
cat("The dimensions of all data are:", dim(uber_data_2014))

#CLEANING DATA: CHECK FOR UNIQUENESS AND NAN-VALUES

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
duplicated_rows <- duplicated(uber_data_214)

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

#3 data analysis

par(mfrow=c(1,1))

head(uber_data_2014)
uber_data_2014$Date.Time <- as.POSIXct(uber_data_2014$Date.Time, format="%d/%m/%Y %H:%M:%S")

#daily number of rides : 
uber_data_april$day <- format(uber_data_april$Date.Time, "%Y-%m-%d")
daily_rides <- table(uber_data_april$day)
length(daily_rides)

#monthly number of rides
uber_data_2014$month <- format(uber_data_2014$Date.Time, "%Y-%m")
monthly_rides <- table(uber_data_2014$month)
length(monthly_rides)

#peak hours 
uber_data_2014$hour <- as.numeric(format(uber_data_2014$Date.Time, "%H"))
rides_per_hour <- table(uber_data_2014$hour)
rides_per_hour


#visualizing daily number of rides for month April (example)
barplot(daily_rides, main = "Daily Number of Rides", xlab = "Date", ylab = "Number of Rides", col = "skyblue")

#visualizing monthly number of rides 
barplot(monthly_rides, main = "Monthly Number of Rides", xlab = "Month", ylab = "Number of Rides", col = "lightcoral")

#visualizing peak hours 
plot(rides_per_hour, type = "o", col = "orange", xlab = "Hour of the Day", ylab = "Number of Rides", main = "Peak Hours Analysis")

#information extraction 


#Association and Dimensioning using Apriori algorithm 
install.packages("arules")
library(arules)

uber_data_2014

transactions <- as(uber_data_2014[, c("month", "hour", "Lat", "Lon", "Base")], "transactions")

rules <- apriori(transactions, parameter = list(support = 0.001, confidence = 0.6))

inspect(rules)
rules

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

quality(rules)$support
quality(rules)$confidence

#ploting the association rules scatterplot
plot(quality(rules)$support, quality(rules)$confidence, xlab = "Support", ylab = "Confidence", col = "blue", main = "Association Rules Scatterplot")
#Interpreting the plot : most of the rules are concentrated in he upper left of the plot 

#Prediction model :
uber_data_2014




