
rm(list=ls())

# Load relevant packages
#installed.packages()
#install.packages("rpart") # CART standard package
#install.packages("rpart.plot") # Enhanced tree plots
#install.packages("rattle") # Fancy tree plot
#install.packages("RColorBrewer") # colors needed for rattle

# Import all the loaded packages
library(rpart)
library(rpart.plot)  	
library(rattle)           
library(RColorBrewer)     
library(caret)

# Load training and test data
air_training<- read.csv('C:/Users/amitr/Downloads/airplane_train.csv') # Load dataset
air_test<- read.csv("C:/Users/amitr/Downloads/airplane_test.csv") # Load dataset

# Replace any blanks with median
air_training$Arrival.Delay.in.Minutes[is.na(air_training$Arrival.Delay.in.Minutes)]<-median(air_training$Arrival.Delay.in.Minutes, na.rm = TRUE)
air_test$Arrival.Delay.in.Minutes[is.na(air_test$Arrival.Delay.in.Minutes)]<-median(air_test$Arrival.Delay.in.Minutes, na.rm = TRUE)

# Remove irrelevant columns (ex, IDs, row #, low correlated attributes)
air_training<-air_training[,-c(1,2,3,10,12,23,24)]
air_test<-air_test[,-c(1,2,3,10,12,23,24)]

# Factor Data
air_training$satisfaction <- factor(air_training$satisfaction) # assign labels
air_test$satisfaction <- factor(air_test$satisfaction) # assign labels

View(air_training) # check to see labels applied


# Grow tree via CART algorithm
set.seed(111)
CART_class<-rpart(satisfaction~.,data=air_training) # run CART algorithm on training data
CART_class

# Try plotting Cancer CART training data - - - - - - -
dev.off() # reset graphics device
par(mfrow=c(2,2)) # set graphical parameter arguments
par(mar=c(1,1,1,1))
rpart.plot(CART_class)
fancyRpartPlot(CART_class)

# Scoring cancer test data type
CART_predict<-predict(CART_class,air_test, type="class") # predicting values, have to use class, not vector b/c its factor structure
table(Actual=air_test[,"satisfaction"],CART=CART_predict) # confusion matrix
str(CART_predict) # check data structure
confusionMatrix(table(actual=air_test$satisfaction,prediction=CART_predict)) # return f1 statistics

# Accuracy results - double check
CART_wrong<-sum(air_test[,18]!=CART_predict)  # count number of wrongly predicted
CART_error_rate<-CART_wrong/length(air_test[,18]) # calculate the error rate 
1-CART_error_rate

