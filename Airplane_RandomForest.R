# Load both training and test data
rm(list=ls())
air_training <- read.csv("C:/Users/amitr/Downloads/airplane_train.csv") # Load dataset
air_test<- read.csv("C:/Users/amitr/Downloads/airplane_test.csv") # Load dataset


# Pre-processing & Cleaning replace blanks with Medians
air_training$Arrival.Delay.in.Minutes[is.na(air_training$Arrival.Delay.in.Minutes)]<-median(air_training$Arrival.Delay.in.Minutes, na.rm = TRUE)
air_test$Arrival.Delay.in.Minutes[is.na(air_test$Arrival.Delay.in.Minutes)]<-median(air_test$Arrival.Delay.in.Minutes, na.rm = TRUE)

# Update to categorical columns to numeric for training
air_training<-air_training[,-c(1,2,3,10,12,23,24)] # eliminates columns
air_training$Customer.Type<-ifelse(air_training$Customer.Type=="Loyal Customer", 1, 0)
air_training$Type.of.Travel<-ifelse(air_training$Type.of.Travel=="Business travel", 1, 0)
air_training$Business<-ifelse(air_training$Class=="Business", 1, 0)
air_training$EcoPlus<-ifelse(air_training$Class=="Eco Plus", 1, 0)
air_training<-air_training[,-4] # Eliminates class column since added Business & EcoPlus columns

# Update to categorical columns to numeric for test
air_test<-air_test[,-c(1,2,3,10,12,23,24)]
air_test$Customer.Type<-ifelse(air_test$Customer.Type=="Loyal Customer", 1, 0)
air_test$Type.of.Travel<-ifelse(air_test$Type.of.Travel=="Business travel", 1, 0)
air_test$Business<-ifelse(air_test$Class=="Business", 1, 0)
air_test$EcoPlus<-ifelse(air_test$Class=="Eco Plus", 1, 0)
air_test<-air_test[,-4] # Eliminates class column

# Factor data
air_training$satisfaction <- factor(air_training$satisfaction) # assign labels
air_test$satisfaction <- factor(air_test$satisfaction) # assign labels

set.seed(111)
library(randomForest)
library(caret)

fit <- randomForest(satisfaction~., data=air_training, importance=TRUE, ntree=200)
importance(fit)
varImpPlot(fit)
Prediction <- predict(fit, air_test)
table(actual=air_test[,17],Prediction)
confusionMatrix(table(actual=air_test[,17],Prediction)) #Accuracy results

wrong<- (air_test[,17]!=Prediction)
error_rate<-sum(wrong)/length(air_test[,17])
error_rate
