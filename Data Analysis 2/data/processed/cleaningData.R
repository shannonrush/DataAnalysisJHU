load("../raw/samsungData.rda")

# this renames the duplicated variable names, appending .1 and .2 to the dups
data <- data.frame(samsungData)

save(data,file="data.rda")

# split data into training and test 
testIndices <- which(data$subject %in% c(27,28,29,30))
train <- data[-testIndices,]
test <- data[testIndices,]

save(train,file="train.rda")
save(test,file="test.rda")

# now split train into training and validating

library(caret)
set.seed(335)
trainIndex <- createDataPartition(train$activity, list=FALSE, p = 0.7)
training <- train[trainIndex,]
validating <- train[-trainIndex,]
save(training,file="training.rda")
save(validating,file="validating.rda")