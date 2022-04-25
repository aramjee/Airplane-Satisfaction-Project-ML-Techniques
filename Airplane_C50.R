# Airplane C5.0

rm(list=ls())

air_training<- read.csv('C:/Users/amitr/Downloads/airplane_train.csv') # Load dataset
air_test<- read.csv("C:/Users/amitr/Downloads/airplane_test.csv") # Load dataset

air_training$Arrival.Delay.in.Minutes[is.na(air_training$Arrival.Delay.in.Minutes)]<-median(air_training$Arrival.Delay.in.Minutes, na.rm = TRUE)
air_test$Arrival.Delay.in.Minutes[is.na(air_test$Arrival.Delay.in.Minutes)]<-median(air_test$Arrival.Delay.in.Minutes, na.rm = TRUE)

view(air_test)



# Update to categorical columns to numeric for training
air_training<-air_training[,-c(1,2,3,10,12,23,24)]
air_training$Customer.Type<-ifelse(air_training$Customer.Type=="Loyal Customer", 1, 0)
air_training$Type.of.Travel<-ifelse(air_training$Type.of.Travel=="Business travel", 1, 0)
air_training$Business<-ifelse(air_training$Class=="Business", 1, 0)
air_training$EcoPlus<-ifelse(air_training$Class=="Eco Plus", 1, 0)
air_training<-air_training[,-4] # Eliminates class column

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

#Install C50 package and Grow tree
install.packages("C50")
library('C50')
C50_class<- C5.0(satisfaction~.,data=air_training)

# summarize & plot
summary(C50_class) # this only uses training not test, don't rely on
dev.off()
plot(C50_class)

# Score the data
C50_predict<-predict(C50_class, air_test, type="class")
table(actual=air_test[,17],C50=C50_predict)
confusionMatrix(table(actual=air_test[,17],C50=C50_predict))

# Accuracy results (Error Rate)
wrong<- (air_test[,22]!=C50_predict)
error_rate<-sum(wrong)/length(air_test[,22])
error_rate
accuracy<-1-error_rate
accuracy
