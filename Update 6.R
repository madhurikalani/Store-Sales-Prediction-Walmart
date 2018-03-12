getwd()
setwd("C:/Users/akash/Documents/Madhuri stuff/R programming/Walmart dataset")

load("FileLoad2.RData")  # Loading .Rdata file


# Applying Regression model on Training data

#Converting Weekly Sales values to log  because of it high values
Traindf$Weekly_Sales<-log(4990+Traindf$Weekly_Sales) 

#Linear Model


  fitting <-lm(Weekly_Sales ~ Date, Traindf)
fitting #Printing to check correctness

fitting$residuals
Residual_Sample<- sample(fitting$residuals,4000) #Creating a sample of residuals

#Plotting a histogram
hist(Residual_Sample)
# Data might/might not normally distributed visually

#Plotting histogram using ggplot
library(ggplot2)
ggplot(fitting, aes(x = fitting$residuals)) + geom_histogram() + 
  ggtitle("Residuals")

#
#QQ Plot
#
# There's also a visual called the QQ plot that will put
# a distribution on each axis. If the points line up on
# a straight line, then it is the same distribution.
# This is commonly done to visually check if samples
# appear normally distributed. 
# However, it is hard to get the tails to match:
qqnorm(Residual_Sample)   # plots the sample distribution against a normal one
qqline(Residual_Sample) # adds that line

#
#Monte Carlo Simulations
#
#having uncertainty about the inputs/models
# To Check the sum of samples from any distribution will be normally distributed
N<-500 # # How many times I'll repeat the process
xsum <- replicate(N, sum(rpois(Residual_Sample,2))) #the sum of the Poisson samples, 2 forPoisson Dist

hist(xsum) #Plotting a histogram of these sums
# From the histogram, the sum of the Poisson samples appears to be
# normally distributed, confirming what the CLT says. 


#Checking mean value of residual sample
round(mean(Residual_Sample)) #Close to 0

#
#Shapiro-Wilk test for normality
#
shapiro.test(Residual_Sample) #Checking p-value for sample set of values
#Here, the p-value is 2.2e-16, which is < 0.05, 
#thus it proves the data is not normally distributed

#
#t-test with the null hypothesis being that the true mean is zero
#
t.test(Residual_Sample) #Checking p-value for sample set of values
# Once you have the p-value, you can make your conclusion. 
# Here, the p-value is 0.7763, which is > 0.05. 
#So we fail to reject the null hypothesis that says the 
# true mean is zero. 
#And that mean lies between -0.02186670  0.02927951
#Therefore -> we conclude the mean is zero. 

#
#Chi Squared tests: 
#
# It is a family of tests that compare discrete
#populations. For example, you can test if two variables are 
# independent or test the goodness-of-fit test for testing proportions.

# 1. Null hypothesis: the two variables are independent
# 2. Alternative hypothesis: the two variables are dependent

chisq.test(Traindf$Store,Traindf$Weekly_Sales, correct=FALSE) 
# Since the p-value > 0.05, we reject the null hypothesis and 
# conclude that the variables are dependent, i.e. the Store 
# seems to make a difference!

table(Traindf$Month,Traindf$Weekly_Sales) 
chisq.test(Traindf$Month, Traindf$Weekly_Sales, correct=FALSE) 
# Since the p-value < 0.05, we reject the null hypothesis and 
# conclude that the variables are dependent, i.e. the Month 
# seems to make a difference!

