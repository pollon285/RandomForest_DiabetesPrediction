#---
#  Title: Machine Learning: Predicting Diabetes using the Pima Indians Dataset
#  Date: 31/05/2020
#---

# install.packages("neuralnet")
# install.packages("caret")
# install.packages("tidyverse")
# install.packages("mlbench")
# install.packages("e1071")

#load relevant libraries
library(neuralnet)
library(caret)
library(tidyverse)
library(mlbench)
library(e1071)

#set WD
setwd("/Users/aurafrizzati/Dropbox/MachineLearning_PROJECTS/RandomForest_DiabetesPrediction/")
getwd()

#1) DATA LOADING
#load the PIMA dataset
df = read.csv("pima-indians-diabetes_MOD.csv")
str(df)

#I convert the data type of the columns as the tutorial
df$diabetes<-as.factor(df$diabetes) #2=1=positive, 1=0=negative
levels(df$diabetes) <- c("neg","pos")
df[, c(1:5,8)] <- sapply(df[, c(1:5,8)], as.numeric)
str(df)

# 2) EXPLORATORY ANALYSIS
ggplot(df, aes(diabetes, fill = factor(diabetes)))+
  geom_bar()

df$binary <- ifelse(df$diabetes == 'neg', 0, 1)
str(df)

# 3) DATA PARTITION
# Creating the train (70%) and test (30%) sets

rows <- createDataPartition(df$binary, times = 1, p = 0.7, list = FALSE) 
# this contains 538 elements (70% of original df) and gives the row number of the original df which will
# be part of the train dataset

#subset the df
train <-df[rows,] 
test <-df[-rows,]
dim(train)
dim(test)


# 4) CREATING MODELS

#clean up the train and test datasets
str(train)
names(train) #diabetes is the 9 variable in train and test dataframes
#train<-train[,-9]
#test<-test[,-9]
str(train) 
str(test) #now the diabetes var has been removed (it is the same as binary and it would 
# compromise the correlation step)

#set up the random forest model with caret
model<-train(as.factor(binary) ~ ., #'.' indicates we use as predictors all the other vars in train df
             data = train,
             method = "ranger", #algorithm used
             trControl = trainControl(method = "repeatedcv", number = 2, repeats = 2)) #Train control object

model

# 5) PREDICT USING A TEST SET

#predict outcome in both train and test sets
predict_train <- predict(model, train)
predict_test <- predict(model, test)

#create a confusion matrix
confusionMatrix(predict_train, as.factor(train$binary))
confusionMatrix(predict_test, as.factor(test$binary))



