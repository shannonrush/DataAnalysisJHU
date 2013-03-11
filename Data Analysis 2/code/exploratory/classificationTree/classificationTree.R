load("../../data/processed/training.rda")
load("../../data/processed/validating.rda")
library(tree)
training$activity <- as.factor(training$activity)
validating$activity <- as.factor(validating$activity)
activity.tree <- tree(training$activity~.,data=training)
summary(activity.tree)
# Classification tree:
#   tree(formula = training$activity ~ ., data = training)
# Variables actually used in tree construction:
#   [1] "fBodyAccJerk.bandsEnergy...1.16" "tGravityAcc.min...X"             "angle.Y.gravityMean."           
# [4] "fBodyAccMag.mad.."               "tGravityAcc.arCoeff...Y.1"       "tGravityAcc.energy...Y"         
# [7] "fBodyGyro.maxInds.X"             "tGravityAcc.min...Y"            
# Number of terminal nodes:  9 
# Residual mean deviance:  0.5733 = 2351 / 4100 
# Misclassification error rate: 0.09029 = 371 / 4109 
p<-predict(activity.tree,newdata=validating,type="class")
getAccuracy(p,validating$activity)
# [1] "1576/1758"
# [1] "89.65%"
#           prediction
# actual     laying sitting standing walk walkdown walkup
# laying      334       0        0    0        0      0
# sitting       0     282       24    0        0      0
# standing      0      38      289    0        0      0
# walk          0       0        0  270        5     24
# walkdown      0       0        0   30      176     29
# walkup        0       0        1   18       13    225
# plot(cv.tree(activity.tree,method="misclass"))

