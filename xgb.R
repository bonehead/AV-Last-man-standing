rm(list=ls())
setwd("G:\\work\\LEARNING\\R Projects\\last_man")

##Load Libraries
library(readr)
library(caTools)
library(xgboost)
library(tm)
library(caret)
library(magrittr)
library(data.table)
library(rpart)


train=read_csv("train.csv")
test=read_csv("test.csv")
samp=read_csv("sample.csv")

summary(train)
names(train)


feature.names <- names(train)[3:ncol(train)-1]

cat("assuming text variables are categorical & replacing them with numeric ids\n")
for (f in feature.names) {
  if (class(train[[f]])=="character") {
    levels <- unique(c(train[[f]], test[[f]]))
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
  }
}

cat("replacing missing values with -1\n")
train[is.na(train)] <- -1
test[is.na(test)]   <- -1

cat("training a XGBoost classifier\n")
clf <- xgboost(data        = data.matrix(train[,feature.names]),
               label       = train$Crop_Damage,
               nrounds     = 20,
               objective   = "multi:softmax",
               eval_metric = "merror",
               num_class=3)


submission <- data.frame(ID=test$ID)
submission$Crop_Damage <- predict(clf, data.matrix(test[,feature.names]))

cat("saving the submission file\n")
write_csv(submission, "xgboost_submission.csv")


importance_matrix <- xgb.importance(feature.names, model = clf)
xgb.plot.importance(importance_matrix)


