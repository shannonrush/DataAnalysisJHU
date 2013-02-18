randomForest1 <- function() {
  load("../../data/processed/loansData_Clean.rda") # data 2498 x 17
  library(randomForest)
  data$State <- NULL
  rf <- randomForest(Rate ~.,data=data,do.trace=T,importance=T)
  importance(rf)
}

