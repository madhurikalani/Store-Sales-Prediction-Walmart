#In this Update, I have used different types of plots (ggplot, hist, barplot) to understand the data and factors that could affect the sales

getwd()
setwd("C:/Users/akash/Documents/Madhuri stuff/R programming/Walmart dataset")
load("FileLoad2.RData") #fileload.RData 

Traindf$Month <- as.factor(Traindf$Month)
Traindf$Weekday <- as.factor(Traindf$Weekday)

Testdf$Month <- as.factor(Testdf$Month)
Testdf$Weekday <- as.factor(Testdf$Weekday)

save.image("FileLoad2.RData")
quarters(Date)
str(Traindf)
?ggplot
#Frequency of Weekly Sales Amount
#

plot(x=Traindf$Month,y=Traindf$Weekly_Sales,type = 'p')

plot(Traindf$Weekly_Sales~Traindf$Date,type = 'p')

#Yearly Sales
barplot(tapply(Traindf$Weekly_Sales, format(Traindf$Date, "%Y"), FUN=sum))
#store wise Unemployment
barplot(tapply(Traindf$Unemployment,Traindf$Store, FUN=sum))
min(Traindf$Weekly_Sales)

library(ggplot2)
?ggplot
?plot
#Temperature at various stores
plot(Traindf$Weekly_Sales~Traindf$Temperature,xlab="Temperature",ylab="Weekly_Sales",type = 'l')
#Fuel prices at different stores
plot(Traindf$Fuel_Price,Traindf$Weekly_Sales,xlab="Fuel_price",ylab="Weekly_Sales",type = 'l')

plot(Traindf$Store,Traindf$Fuel_Price,xlab="Store",ylab="Fuel_Price",type = 'l')
plot(Traindf$CPI~Traindf$Store,xlab="Store",ylab="CPI",type = 'l')
plot(y=Traindf$Unemployment,x=Traindf$Store,xlab="Store",ylab="Unemployment",type = 'p')

#Store wide sales over the years

ggplot(Traindf, aes(x=Date,y=Weekly_Sales, fill = Store)) + 
  +   geom_bar(stat = "identity")
#Quarterly sales comparison for all stores
ggplot(Traindf, aes(x=Store,y=Weekly_Sales, fill = quarters(Date))) + 
  geom_bar(stat = "identity") 

ggplot(Traindf, aes(x=Temperature,y=Weekly_Sales, fill = quarters(Date))) + 
  geom_bar(stat = "identity") 
#Weekly sales over the quarters
ggplot(Traindf,aes(x=Date,y=Weekly_Sales,color=Holiday)) +geom_point()
ggplot(Traindf,aes(x=Date,y=Weekly_Sales,color=Store)) +geom_point(aes(shape = Temperature))
ggplot(Traindf, aes(x=Date,y=Weekly_Sales)) + geom_line(aes(color = Store))
ggplot(Traindf, aes(x=Store,y=Weekly_Sales)) + geom_line(aes(color = quarters(Date)))

#ggplot(Traindf, aes(x=Date,y=Weekly_Sales)) + geom_point() + facet_grid(. ~ Store) 
ggplot(Traindf, aes(x=Store,y=Weekly_Sales[!is.na(Weekly_Sales)])) + geom_point() + facet_grid(. ~ quarters(Date)) 

ggplot(data = Traindf, aes(x = Holiday, y = Weekly_Sales)) + 
  geom_line(aes(color = Holiday))

print(ifelse(Traindf$Holiday=="laborday",Traindf$Weekday,Traindf$Date))

hist(Traindf$Weekly_Sales, freq = FALSE)

str(Traindf)
?plot()
sum(is.na(Traindf$Month))

plot(Traindf$Month,log(Traindf$Weekly_Sales),ylim='227')
