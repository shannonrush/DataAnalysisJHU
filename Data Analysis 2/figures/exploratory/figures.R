# Best result from random forest

load("../../data/processed/training.rda")
load("../../data/processed/test.rda")
training$activity<-as.factor(training$activity)
test$activity<-as.factor(test$activity)

library(randomForest)
set.seed(335)
activity.rf <- randomForest(training$activity~.,data=training,xtest=test[,-563],ytest=test[,563],do.trace=T,importance=T,replace=T,ntree=750,sampsize=nrow(training))
colors <- c("black","red","orange1","yellow2","green2","blue","purple1")
plot(activity.rf,lwd=2,ann=F,col=colors)
title(main="Mean Squared Error By Tree",xlab="Number of Trees",ylab="Mean Squared Error")
legend("topright",c("Out Of Bag Error",levels(training$activity)),fill=colors)