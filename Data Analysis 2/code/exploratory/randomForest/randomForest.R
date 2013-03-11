load("../../../data/processed/training.rda")
load("../../../data/processed/validating.rda")
training$activity <- as.factor(training$activity)
validating$activity <- as.factor(validating$activity)
library(randomForest)
set.seed(335)

activity.forest <- randomForest(training$activity~.,data=training,do.trace=T,importance=T,replace=T,ntree=500,sampsize=nrow(training))
getAccuracy(activity.forest$predicted,training$activity)
# [1] "4015/4109"
# [1] "97.71%"
# prediction
# actual     laying sitting standing walk walkdown walkup
# laying      780       0        0    0        0      0
# sitting       0     689       26    0        0      1
# standing      0      38      726    0        0      0
# walk          0       0        0  689        5      4
# walkdown      0       0        0    5      539      7
# walkup        0       0        0    1        7    592
p<-predict(activity.forest,newdata=validating)
getAccuracy(p,validating$activity)
# [1] "1720/1758"
# [1] "97.84%"
# prediction
# actual     laying sitting standing walk walkdown walkup
# laying      334       0        0    0        0      0
# sitting       0     296       10    0        0      0
# standing      0      15      312    0        0      0
# walk          0       0        0  291        2      6
# walkdown      0       0        0    3      232      0
# walkup        0       0        0    0        2    255
plot(activity.forest)

# INCREASED NTREE 
# ntree = 750
activity.rf <- randomForest(training$activity~.,data=training,do.trace=T,importance=T,replace=T,ntree=750,sampsize=nrow(training))
getAccuracy(activity.rf$predicted,training$activity)
# [1] "4017/4109"
# [1] "97.76%"
# prediction
# actual     laying sitting standing walk walkdown walkup
# laying      780       0        0    0        0      0
# sitting       0     689       26    0        0      1
# standing      0      35      729    0        0      0
# walk          0       0        0  686        8      4
# walkdown      0       0        0    4      541      6
# walkup        0       0        0    1        7    592
p<-predict(activity.rf,validating)
getAccuracy(p,validating$activity)
# [1] "1725/1758"
# [1] "98.12%"
# prediction
# actual     laying sitting standing walk walkdown walkup
# laying      334       0        0    0        0      0
# sitting       0     297        9    0        0      0
# standing      0      12      315    0        0      0
# walk          0       0        0  293        2      4
# walkdown      0       0        0    3      232      0
# walkup        0       0        0    0        3    254

# ntree = 800
activity.rf <- randomForest(training$activity~.,data=training,do.trace=T,importance=T,replace=T,ntree=800,sampsize=nrow(training))
getAccuracy(activity.rf$predicted,training$activity)
# [1] "4012/4109"
# [1] "97.64%"
# prediction
# actual     laying sitting standing walk walkdown walkup
# laying      780       0        0    0        0      0
# sitting       0     690       25    0        0      1
# standing      0      38      726    0        0      0
# walk          0       0        0  685        8      5
# walkdown      0       0        0    6      538      7
# walkup        0       0        0    0        7    593
p<-predict(activity.rf,validating)
getAccuracy(p,validating$activity)
# [1] "1719/1758"
# [1] "97.78%"
# prediction
# actual     laying sitting standing walk walkdown walkup
# laying      334       0        0    0        0      0
# sitting       0     295       11    0        0      0
# standing      0      16      311    0        0      0
# walk          0       0        0  292        2      5
# walkdown      0       0        0    3      232      0
# walkup        0       0        0    0        2    255


# ntree = 1000
activity.rf <- randomForest(training$activity~.,data=training,do.trace=T,importance=T,replace=T,ntree=1000,sampsize=nrow(training))
getAccuracy(activity.rf$predicted,training$activity)
# [1] "4018/4109"
# [1] "97.79%"
# prediction
# actual     laying sitting standing walk walkdown walkup
# laying      780       0        0    0        0      0
# sitting       0     690       25    0        0      1
# standing      0      35      729    0        0      0
# walk          0       0        0  686        8      4
# walkdown      0       0        0    4      540      7
# walkup        0       0        0    0        7    593
p<-predict(activity.rf,validating)
getAccuracy(p,validating$activity)
# [1] "1723/1758"
# [1] "98.01%"
# prediction
# actual     laying sitting standing walk walkdown walkup
# laying      334       0        0    0        0      0
# sitting       0     296       10    0        0      0
# standing      0      15      312    0        0      0
# walk          0       0        0  292        2      5
# walkdown      0       0        0    1      234      0
# walkup        0       0        0    0        2    255

# ntree = 1250
activity.rf <- randomForest(training$activity~.,data=training,do.trace=T,importance=T,replace=T,ntree=1250,sampsize=nrow(training))
getAccuracy(activity.rf$predicted,training$activity)
# [1] "4014/4109"
# [1] "97.69%"
# prediction
# actual     laying sitting standing walk walkdown walkup
# laying      780       0        0    0        0      0
# sitting       0     690       25    0        0      1
# standing      0      39      725    0        0      0
# walk          0       0        0  686        8      4
# walkdown      0       0        0    5      541      5
# walkup        0       0        0    1        7    592
p<-predict(activity.rf,validating)
getAccuracy(p,validating$activity)
# [1] "1721/1758"
# [1] "97.9%"
# prediction
# actual     laying sitting standing walk walkdown walkup
# laying      334       0        0    0        0      0
# sitting       0     295       11    0        0      0
# standing      0      15      312    0        0      0
# walk          0       0        0  292        2      5
# walkdown      0       0        0    1      234      0
# walkup        0       0        0    0        3    254

# Conclusion - ntree = 750 optimal
activity.rf <- randomForest(training$activity~.,data=training,do.trace=T,importance=T,replace=T,ntree=750,sampsize=nrow(training))
plot(activity.rf)
activity.rf$importance

# FEATURE SELECTION 
gini <- activity.rf$importance[,8]
attach(training)

# find features whose meanDecreaseGini > 10
toInclude <- gini[gini>10]
predictors<-paste0(names(toInclude),collapse="+")
formula <- as.formula(paste("activity~",predictors))
features.rf <- randomForest(formula,data=training,do.trace=T,importance=T,replace=T,ntree=750,sampsize=nrow(training))
getAccuracy(features.rf$predicted,activity)
# [1] "4019/4109"
# [1] "97.81%"
# prediction
# actual     laying sitting standing walk walkdown walkup
# laying      780       0        0    0        0      0
# sitting       0     698       18    0        0      0
# standing      0      36      728    0        0      0
# walk          0       0        0  685        6      7
# walkdown      0       0        0    8      537      6
# walkup        0       0        0    2        7    591
p<-predict(features.rf,newdata=validating)
getAccuracy(p,validating$activity)
# [1] "1717/1758"
# [1] "97.67%"
# prediction
# actual     laying sitting standing walk walkdown walkup
# laying      334       0        0    0        0      0
# sitting       0     297        9    0        0      0
# standing      0      11      316    0        0      0
# walk          0       0        0  291        1      7
# walkdown      0       0        0    4      228      3
# walkup        0       0        0    1        5    251

# meanDecreaseGini > 20
toInclude <- gini[gini>20]
predictors<-paste0(names(toInclude),collapse="+")
formula <- as.formula(paste("activity~",predictors))
features.rf <- randomForest(formula,data=training,do.trace=T,importance=T,replace=T,ntree=750,sampsize=nrow(training))
getAccuracy(features.rf$predicted,activity)
# [1] "4015/4109"
# [1] "97.71%"
# prediction
# actual     laying sitting standing walk walkdown walkup
# laying      780       0        0    0        0      0
# sitting       0     699       17    0        0      0
# standing      0      29      735    0        0      0
# walk          0       0        0  682        8      8
# walkdown      0       0        0    9      536      6
# walkup        0       0        0    9        8    583
p<-predict(features.rf,newdata=validating)
getAccuracy(p,validating$activity)
# [1] "1710/1758"
# [1] "97.27%"
# prediction
# actual     laying sitting standing walk walkdown walkup
# laying      334       0        0    0        0      0
# sitting       0     300        6    0        0      0
# standing      0      10      317    0        0      0
# walk          0       0        0  288        3      8
# walkdown      0       0        0   10      222      3
# walkup        0       0        0    1        7    249

# gini > 30
toInclude <- gini[gini>30]
predictors<-paste0(names(toInclude),collapse="+")
formula <- as.formula(paste("activity~",predictors))
features.rf <- randomForest(formula,data=training,do.trace=T,importance=T,replace=T,ntree=750,sampsize=nrow(training))
getAccuracy(features.rf$predicted,activity)
p<-predict(features.rf,newdata=validating)
getAccuracy(p,validating$activity)

# Conclusion - eliminating variables by meanDecreasedGini decreases accuracy

# predict with test set
load("../../../data/processed/test.rda")
test$activity <- as.factor(test$activity)
activity.rf <- randomForest(training$activity~.,data=training,do.trace=T,importance=T,replace=T,ntree=750,sampsize=nrow(training))
p<-predict(activity.rf,newdata=test)
getAccuracy(p,test$activity)
# [1] "1413/1485"
# [1] "95.15%"
# prediction
# actual     laying sitting standing walk walkdown walkup
# laying      293       0        0    0        0      0
# sitting       0     227       37    0        0      0
# standing      0      27      256    0        0      0
# walk          0       0        0  229        0      0
# walkdown      0       0        0    3      193      4
# walkup        0       0        0    1        0    215

# creating a larger training set

newTraining <- rbind(training,validating)
activity.rf <- randomForest(newTraining$activity~.,data=newTraining,do.trace=T,importance=T,replace=T,ntree=750,sampsize=nrow(training))
p<-predict(activity.rf,newdata=test)
getAccuracy(p,test$activity)
# [1] "1411/1485"
# [1] "95.02%"
# prediction
# actual     laying sitting standing walk walkdown walkup
# laying      293       0        0    0        0      0
# sitting       0     226       38    0        0      0
# standing      0      28      255    0        0      0
# walk          0       0        0  228        0      1
# walkdown      0       0        0    2      194      4
# walkup        0       0        0    1        0    215

# RFCV
load("../../../data/processed/train.rda")
train$activity <- as.factor(train$activity)
cv <- rfcv(train[,-563],train[,563])
save(cv,file="rfcv.rda")
# indicates 140 is the optimum number of variables to use in random forest
# take top 140 by importance and re run:
i <- importance(activity.rf)
gini <- i[,8]
sorted <- sort(gini,decreasing=T)
selected <- gini[1:140]
predictors <- paste(names(gini),collapse="+")
form <- as.formula(paste("activity~",predictors))
# now run this new formula and find the optimum ntree
features.rf <- randomForest(form,data=training,do.trace=T,importance=T,replace=T,ntree=750,sampsize=nrow(training))
p<-predict(features.rf,newdata=validating)
getAccuracy(p,validating$activity)
[1] "97.72%"
features.rf <- randomForest(form,data=training,do.trace=T,importance=T,replace=T,ntree=1000,sampsize=nrow(training))
p<-predict(features.rf,newdata=validating)
getAccuracy(p,validating$activity)
[1] "97.84%"
features.rf <- randomForest(form,data=training,do.trace=T,importance=T,replace=T,ntree=1100,sampsize=nrow(training))
p<-predict(features.rf,newdata=validating)
getAccuracy(p,validating$activity)
[1] "97.95%"
features.rf <- randomForest(form,data=training,do.trace=T,importance=T,replace=T,ntree=1200,sampsize=nrow(training))
p<-predict(features.rf,newdata=validating)
getAccuracy(p,validating$activity)
[1] "97.9%"

#confounding
c<-training[c("activity",names(selected))]
c$activity<-as.numeric(c$activity)
cor<-cor(c)
a<-cor[,1]
s<-sort(round(a,2),decreasing=T)
nondups <- names(s[-which(duplicated(s))])
dups <- names(s[which(duplicated(s))])
uniqdups <- c("tBodyAccJerk.entropy...Y", "tBodyAcc.mad...Y","tBodyAcc.iqr...Y","tBodyAcc.max...Y","tBodyAcc.iqr...Z","tBodyAcc.iqr...X","tBodyAccJerk.iqr...Y","tBodyAcc.entropy...Y","tBodyGyro.iqr...X","tBodyGyro.max...Z","tBodyAccJerk.max...Y","tBodyAcc.entropy...X","tBodyAcc.entropy...Z","tBodyGyro.energy...X","tGravityAcc.min...X","tBodyAccJerk.energy...Y","tBodyGyro.energy...Z","tBodyAccJerk.energy...Z","tGravityAcc.correlation...X.Y","tGravityAcc.correlation...Y.Z","tGravityAcc.std...Z","tBodyAcc.arCoeff...Y.4","tBodyAccJerk.mean...Y","tGravityAcc.mad...X","tBodyGyro.mean...Y","tBodyGyro.mean...Z","tBodyAccJerk.arCoeff...Z.3","tBodyAcc.arCoeff...Y.1","tGravityAcc.max...Z","tGravityAcc.min...Y","tBodyGyro.min...Z")
refined <- c(nondups,uniqdups)
refined<-refined[-1]
form <- as.formula(paste("activity~",paste(refined,collapse="+")))
refined.rf <-randomForest(form,data=training,do.trace=T,replace=T,ntree=750)
p<-predict(refined.rf,newdata=validating)
getAccuracy(p,validating$activity)
[1] "97.84%"
refined.rf <-randomForest(form,data=training,do.trace=T,replace=T,ntree=1000)
p<-predict(refined.rf,newdata=validating)
getAccuracy(p,validating$activity)
[1] "97.9%"
