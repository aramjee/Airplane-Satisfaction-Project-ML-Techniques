# Airplane knn

# Load both training and test data
rm(list=ls())
air_training <- read.csv("C:/Users/amitr/Downloads/airplane_train.csv") # Load dataset
air_test<- read.csv("C:/Users/amitr/Downloads/airplane_test.csv") # Load dataset

# Cleaning Data - Replace data with median
air_training$Arrival.Delay.in.Minutes[is.na(air_training$Arrival.Delay.in.Minutes)]<-median(air_training$Arrival.Delay.in.Minutes, na.rm = TRUE)
air_test$Arrival.Delay.in.Minutes[is.na(air_test$Arrival.Delay.in.Minutes)]<-median(air_test$Arrival.Delay.in.Minutes, na.rm = TRUE)


#  Please also Eliminate the following agreed upon columns in both training and test data, check views
# air_training<-air_training[,-c(1,2,3,10,12,23,24)] # eliminates columns
air_training$Gender<-ifelse(air_training$Gender=="Male", 1, 0)
air_training$Customer.Type<-ifelse(air_training$Customer.Type=="Loyal Customer", 1, 0)
air_training$Type.of.Travel<-ifelse(air_training$Type.of.Travel=="Business travel", 1, 0)
air_training$Business<-ifelse(air_training$Class=="Business", 1, 0)
air_training$EcoPlus<-ifelse(air_training$Class=="Eco Plus", 1, 0)
air_training<-air_training[,-4] # Eliminates class column since added Business & EcoPlus columns

# Update to categorical columns to numeric for training
air_training<-air_training[,-c(1,2,3,10,12,23,24)]
air_training$Gender<-ifelse(air_training$Gender=="Male", 1, 0)
air_training$Customer.Type<-ifelse(air_training$Customer.Type=="Loyal Customer", 1, 0)
air_training$Type.of.Travel<-ifelse(air_training$Type.of.Travel=="Business travel", 1, 0)
air_training$Business<-ifelse(air_training$Class=="Business", 1, 0)
air_training$EcoPlus<-ifelse(air_training$Class=="Eco Plus", 1, 0)
air_training<-air_training[,-4] # Eliminates class column
View(air_training)

# Update to categorical columns to numeric for test
air_test<-air_test[,-c(1,2,3,10,12,23,24)]
air_test$Gender<-ifelse(air_test$Gender=="Male", 1, 0)
air_test$Customer.Type<-ifelse(air_test$Customer.Type=="Loyal Customer", 1, 0)
air_test$Type.of.Travel<-ifelse(air_test$Type.of.Travel=="Business travel", 1, 0)
air_test$Business<-ifelse(air_test$Class=="Business", 1, 0)
air_test$EcoPlus<-ifelse(air_test$Class=="Eco Plus", 1, 0)
air_test<-air_test[,-4] # Eliminates class column

# Factor data
air_training$satisfaction <- factor(air_training$satisfaction) # assign labels
air_test$satisfaction <- factor(air_test$satisfaction) # assign labels

View(air_training) # check to see labels applied


# Code to run the KNN model for multiple K values:
k_value<-c(1,3,4,5,6,7,9,12,15,20,30,50,65,80,100) # K value to test
error_Rate<-vector(length = length(k_value))  # store error rate for each model

library(kknn) # use library to run kknn formula
for(i in 1:length(k_value)){
  knn_model<-kknn(satisfaction~., air_training, air_test,k = k_value[i],kernel = "rectangular")
  predicted<-fitted(knn_model)
  wrong<-sum(predicted!=air_test$satisfaction) # count number of wrong where test data doesn't prediction
  error_Rate[i]<-wrong/length(predicted)
}

print(1-error_Rate) # Print Accuracy
plot(k_value, 1-error_Rate, type = "b", ylab = "Accuracy") # Plot the accuracy
