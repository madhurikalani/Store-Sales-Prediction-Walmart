# Objective is to analyze historical data for 45 Walmart stores and various departments. 
# Predicting sales for future time and department wise sales
# We have weekly sales data with date, department.
# Other parameters included in the data are markdowns, temperature, date, CPI and holiday.

#Checking the directory
getwd()
#Changing the directory
setwd("C:/Users/akash/Documents/Madhuri stuff/R programming/Walmart dataset")

# Loading stores data
StoreData <- read.csv("stores.csv",sep=",")
StoreData #print to check correctness of data

# Loading historical sales data from 2010-02-05 to 2012-11-01
TrainData <- read.csv("train.csv",sep=",")
TrainData #print to check correctness of data

# Loading additional data related to the store, department, and regional activity for the given dates
# for markdown column, missing value is marked with an NA
Features <- read.csv("features.csv",sep=",")
Features #print to check correctness of data

# Displaying structure of StoreData
str(StoreData)
# Displaying structure of TrainData
str(TrainData)
# Displaying structure of FeaturesData
str(Features)

