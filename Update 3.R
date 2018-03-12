#The objective of this update is to clean, transform and prepare data to be technically consistent
getwd()
setwd("C:/Users/akash/Documents/Madhuri stuff/R programming/Walmart dataset")
load("FileLoad.RData") #fileload.RData 
#Checking the structure of Store data file
str(StoreData)

#Coerce the Store ID column as Factor
StoreData$Store <- as.factor(StoreData$Store)
#Coerce the Size column as Numeric
StoreData$Size <- as.numeric(StoreData$Size) 
str(StoreData) #verifying structure again for consistency

#Checking the structure of Features data file
str(Features) 

#Coerce the Store ID column as Factor
Features$Store <- as.factor(Features$Store)
#Coerce the date column as Date
Features$Date <- as.Date(Features$Date)
str(Features) #verifying structure again for consistency
Features$Date
#Checking the structure of Training data file
str(TrainData)
#Coerce the Store ID column as Factor
TrainData$Store <- as.factor(TrainData$Store)
#Coerce the Department ID column as Factor
TrainData$Dept <- as.factor(TrainData$Dept)
#Coerce the date column as Date
TrainData$Date <- as.Date(TrainData$Date)
str(TrainData) #verifying structure again for consistency

#Checking the structure of Test data file
str(TestData)
#Coerce the Store ID column as Factor
TestData$Store <- as.factor(TestData$Store)
#Coerce the Department ID column as Factor
TestData$Dept <- as.factor(TestData$Dept)
#Coerce the date column as Date
TestData$Date <- as.Date(TestData$Date)
str(TestData) #verifying structure again for consistency

# Checking Number of NAs in Markdown columns
sum(is.na(Features$MarkDown1))
sum(is.na(Features$MarkDown2))
sum(is.na(Features$MarkDown3))
sum(is.na(Features$MarkDown4))
sum(is.na(Features$MarkDown5))

#Fixing and assigning values 0 since column type is numeric
Features[is.na(Features$MarkDown1), 5] <- 0
Features[is.na(Features$MarkDown2), 6] <- 0
Features[is.na(Features$MarkDown3), 7] <- 0
Features[is.na(Features$MarkDown4), 8] <- 0
Features[is.na(Features$MarkDown5), 9] <- 0

str(Features)

#Checking for NA in CPI
sum(is.na(Features$CPI))

#Making CPI as a mean of Store's CPI
#Step 1 is to find unique stores
UniqueStore <- unique((Features[!is.na(Features$CPI),"Store"]))

#Step 2 CPI assignment
for (i in UniqueStore)
{
#Calculating mean CPI for each store
CPIMean <- mean(Features[Features$Store==i,"CPI"],na.rm=TRUE)
#Assigning CPI the value of CPIMean for Each Store where CPI is NA 
Features[Features$Store==i & is.na(Features$CPI),"CPI"] <- CPIMean

}

# Replacing NA for Unemployment with Unemployment mean for every store
# Step 1 calculating unique stores
UniqueStore1 <- unique((Features[!is.na(Features$Unemployment),"Store"]))

# Just checking to see if step 1 in both show same result
identical(UniqueStore1,UniqueStore)

#Step 2 Unemployment
for (i in UniqueStore1)
{
  
# Calculating mean Unemployment for each store
UnemploymentMean <- mean(Features[Features$Store==i,"Unemployment"],na.rm=TRUE)
# Assigning Unemployment the value of UnemploymentMean for Each Store where Unemployment is NA   
Features[Features$Store==i & is.na(Features$Unemployment),"Unemployment"] <- UnemploymentMean
  
}

?rbind #combines data by rows

# Since  this dataset include Holidays
# Preparing to mark holidays explicitly

#Super Bowl: 12-Feb-10, 11-Feb-11, 10-Feb-12, 8-Feb-13
#Labor Day: 10-Sep-10, 9-Sep-11, 7-Sep-12, 6-Sep-13
#Thanksgiving: 26-Nov-10, 25-Nov-11, 23-Nov-12, 29-Nov-13
#Christmas: 31-Dec-10, 30-Dec-11, 28-Dec-12, 27-Dec-13

# Adding Holidays in dataset

# Creating a vectore to add holidays
HOl <- c("superbowl","12-Feb-10")
HOl <- rbind(HOl,c("superbowl","11-Feb-11"))
HOl <- rbind(HOl,c("superbowl","10-Feb-12"))
HOl <- rbind(HOl,c("superbowl","8-Feb-13"))
HOl <- rbind(HOl,c("laborday","10-Sep-10"))
HOl <- rbind(HOl,c("laborday","9-Sep-11"))
HOl <- rbind(HOl,c("laborday","7-Sep-12"))
HOl <- rbind(HOl,c("laborday","6-Sep-13"))
HOl <- rbind(HOl,c("Thanksgiving","26-Nov-10"))
HOl <- rbind(HOl,c("Thanksgiving","25-Nov-11"))
HOl <- rbind(HOl,c("Thanksgiving","23-Nov-12"))
HOl <- rbind(HOl,c("Thanksgiving","29-Nov-13"))
HOl <- rbind(HOl,c("Christmas","31-Dec-10"))
HOl <- rbind(HOl,c("Christmas","30-Dec-11"))
HOl <- rbind(HOl,c("Christmas","28-Dec-12"))
HOl <- rbind(HOl,c("Christmas","27-Dec-13"))

#Giving column name and rowname to convert it into a dataframe
colnames(HOl) <- c("Holiday","Date")
rownames(HOl)<- NULL
HolidayData <- as.data.frame(HOl) #creating data.frame
HolidayData #printing to check

HolidayData$Date <- as.Date(HolidayData$Date,"%d-%b-%y") #Coerce date as date type
str(HolidayData) #checking the structure
plot(HolidayData$Date,HolidayData$Holiday,type = 'l')


print(ifelse(TRUE,HolidayData$Date[HolidayData$Date],HolidayData$Holiday[HolidayData$Holiday]))
HolidayData$Holiday
c(HolidayData$Date,HolidayData$Holiday[[1]])
#merging holidat data with features data
FeatureNew <- merge(Features,HolidayData,by="Date", all.x=TRUE)
str(FeatureNew) #checking structure to validate correctness
plot(FeatureNew$Date,FeatureNew$CPI,type = 'l')
abs((FeatureNew$Weekly_Sales))
plot(x=FeatureNew$Store==1,y=FeatureNew$Weekly_Sales,type = 'l')

print(ifelse(FeatureNew$Holiday=="laborday",FeatureNew$Holiday,FeatureNew$Date))
#Creating levels to add a new level as "Regularday"
levels(FeatureNew$Holiday) <- c("superbowl","laborday","Christmas","Thanksgiving","Regularday")

#Updating holiday column for regular dates
FeatureNew[is.na(FeatureNew$Holiday),"Holiday"] <- "Regularday"


class(FeatureNew$Holiday) #checking class to verify
str(FeatureNew) #checking structure to verify
FeatureNew$Holiday #printing

months.Date(FeatureNew$Date) #FeatureNew$Date
weekdays.Date(FeatureNew$Date)


?merge
# Merge Store data with Training and Test data
Traindf <- merge(x=TrainData, y=StoreData, by="Store",all.x=TRUE)
Testdf <- merge(x=TestData, y=StoreData, by="Store", all.x=TRUE)

# Merge features data with Training and Test data
Traindf <- merge(x=TrainData, y=FeatureNew,all.x=TRUE)
Testdf <- merge(x=TestData, y=FeatureNew, all.x=TRUE)

Traindf$Month <- months.Date(Traindf$Date) 
Traindf$Weekday <- weekdays.Date(Traindf$Date)

Testdf$Month <- months.Date(Testdf$Date) #FeatureNew$Date
Testdf$Weekday <- weekdays.Date(Testdf$Date)

Traindf$Month <- as.factor(Traindf$Month)
Traindf$Weekday <- as.factor(Traindf$Weekday)

Testdf$Month <- as.factor(Testdf$Month)
Testdf$Weekday <- as.factor(Testdf$Weekday)

#str(Traindf)
#str(Testdf)
# Data is now ready and technically consistent
save.image("FileLoad1.RData")

sum(is.na(Traindf$Weekly_Sales))
