rm(list=ls())
setwd("G:\\work\\LEARNING\\R Projects\\last_man")

##Load Libraries
library(readr)
library(caTools)
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


plot(fit, uniform=TRUE, main="Classification Tree")
text(fit, use.n=TRUE, all=TRUE, cex=.8)


fit = rpart(Crop_Damage~Estimated_Insects_Count+Crop_Type+Soil_Type+Pesticide_Use_Category+Number_Doses_Week+Number_Weeks_Used+Number_Weeks_Quit+Season, method="class", data=train)
printcp(fit)
plotcp(fit)
summary(fit)

p<-predict(fit,newdata=test,type="class",na.action=na.fail)

submission <- data.frame(ID=test$ID)
submission$Crop_Damage <- p

cat("saving the submission file\n")
write_csv(submission, "cart.csv")

