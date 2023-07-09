while (!is.null(dev.list())) dev.off()
rm(list = ls())
cat('\014')

library(dplyr)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(e1071)
library(caret)
library(randomForest)
library(adabag)



df <- read.csv(file.choose())
head (df)
str (df)

df$satisfaction <- ifelse(df$satisfaction == 'satisfied', 1, 0)
df$Gender <- ifelse(df$Gender == 'Male', 1, 0)
df$Customer.Type <- ifelse(df$Customer.Type == 'Loyal Customer', 1, 0)
df$Type.of.Travel <- ifelse(df$Type.of.Travel == 'Personal Travel', 1, 0)

df <- na.omit(df)
str(df)
df<- df[,-c(6)]

train_ind<- sample(c(1:dim(df)[1]),.6*dim(df)[1])
df_train<-df[train_ind,]
df_test<-df[-train_ind,]

simple.tr<-rpart(satisfaction~.,data = df_train,method="class")
rpart.plot(simple.tr,type=1,extra=1,under=TRUE)
pred<-predict(simple.tr,df_test,type="class")
confusionMatrix(as.factor(df_test$satisfaction),pred)

simple.tr_min_split<-rpart(satisfaction~.,data = df_train,method="class", minsplit = 7, cp = 0)
rpart.plot(simple.tr_min_split,type=1,extra=1,under=TRUE)

pred<-predict(simple.tr_min_split,df_test,type="class")
confusionMatrix(as.factor(df_test$satisfaction),pred)
#ACCURARY HAS IMPROVED TO 94 WHEN MIN SPLIT IS 10 or even 7 but the Table does not make sense

#FOR RANDOM FOREST AND BOOSTED RUN THIS LINE FIRST 
df$satisfaction <- as.factor(df$satisfaction)

#ntree is 100 breaks the program
#going to try 20 next which works but takes time but confusion matrix does not work 
rf<- randomForest(satisfaction~., df_train, ntree = 20, mtry = 3, nodesize =2, importance = 5)
rf$terms

varImpPlot(rf, type =1)

rf.pred <- predict(rf, df_test)

confusionMatrix(rf.pred, df_test$satisfaction)
#94% effective make sure to factor satisfaction before running

