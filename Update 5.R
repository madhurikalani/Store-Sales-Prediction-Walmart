getwd()
setwd("C:/Users/akash/Documents/Madhuri stuff/R programming/Walmart dataset")

load("FileLoad2.RData")  # Loading .Rdata file

#Applying correlation: Weekly Sales with Markdown1
with(Traindf, cor(Weekly_Sales, MarkDown1))
with(Traindf, plot(MarkDown1,Weekly_Sales))

#Calculating R Square
r_sq <- cor(Traindf$Weekly_Sales, Traindf$MarkDown1)
r_sq #printing: value not that good
cor(Traindf[,5:10]) #Correlation all should be numeric


# Applying Regression model on Training data

#Converting Weekly Sales values to log  because of it high values
Traindf$Weekly_Sales<-log(4990+Traindf$Weekly_Sales) 

#Linear Model
fitting <-lm(Weekly_Sales ~ Date, Traindf)
fitting #Printing to check correctness

Inter<- fitting$coefficients[1] #Intercept
Coeff<- fitting$coefficients[2] #Coefficient
round(Inter)
round(Coeff)
# Confidence intervals: these encapsulate the 95% confidence interval
# of the predicted *mean* response
#95% confidence on Test data
confidence <- predict(fitting, Testdf, interval="confidence")
head(confidence) #printing

max(Traindf$Weekly_Sales) #checking max of Weekly Sales

# Prediction intervals: these encapsulate the 95% prediction interval
# of the predicted response;
#prediction on Test data
Prediction <- predict(fitting, Testdf, interval="prediction")
head(Prediction) #printing to check correctness

x<- Testdf$Date #assigning Date of Test data to x

predict(fitting,Testdf) #predicting weekly Sales

#creating a plot to0 show variation of eruptions with waiting time
plot(Traindf$Date,Traindf$Weekly_Sales,ylim=c(7.9,10),yaxt="n",col="gray")
axis(side = 2, at = seq(7.9,10, 0.0.05),las=2)
abline(fitting, col="red",lty=1) #Regression line in Red

#Confidence values almost same as fitting
lines(x,confidence[,2],lty=2,col="blue") #Lower confidence dashed line in blue
lines(x,confidence[,3],lty=2,col="blue") #Upper confidence dashed line in blue

#Prediction lines
lines(x,Prediction[,2],lty=3) #Lower prediction dotted line in default black
lines(x,Prediction[,3],lty=3) #Upper prediction dotted line in default black

#Predicted value when model is fitted on Test data
lines(predict(fitting, Testdf),col="green")

#Not working
text(2011,12.5,paste("E =", round(Inter), "+ Date * (", Coeff, ")"),col="red") #Adding equation to graph
legend("topright",c("Regression","Confidence","Prediction", "Predicted value Test"),lty=1:4,col=c("red","blue","black","green")) #Adding legends showing what each line is


#Predicting Sales values on Test daat

s=1 #creating a variable s with default value 1
d=1 #creating a variable d with default value 1

# Predicting Weekly Sales of Test data based on lm applied on Tarining data
Testdf$Weekly_Sales<-1 #Assigning default value of 1 to Testdf$weekly_Sales

#Creating a for loop to fit the model for every store and department in Test data
for (s in c(1:5)) #Predicting for Store 1 to 5
{
  print(s)
    #Fitting the model for store 1-5 and Dept=1
    fitting1 = lm(Weekly_Sales~  Date , data=Traindf[which(Traindf$Store == s & Traindf$Dept==1 ), ])
    #fitting1 #printing
    
    #predicting for store 1-5 and Dept=1
    predict = predict(fitting,Testdf[which(Testdf$Store == s & Testdf$Dept==1 ),])
    #predict #printing
    
    #Assigning predicted Weekly_sales for store 1-5 and Dept=1 and storing it on Test data
    Testdf[which(Testdf$Store == s & Testdf$Dept==1 ),17] = exp(predict) - 4990
  
}

#Finally writing predicted values to a file
write.table(x=Testdf,  file='Predicted_sales.csv',sep=',', row.names=FALSE, quote=FALSE) 


