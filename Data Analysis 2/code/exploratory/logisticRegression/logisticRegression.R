load("../../../data/processed/training.rda")
load("../../../data/processed/validating.rda")
training$activity <- as.factor(training$activity)
validating$activity <- as.factor(validating$activity)
library(nnet)
set.seed(335)

activity.lr <- multinom(activity ~ .,data=training, MaxNWts=3400, maxit=100)
p<-predict(activity.lr, newdata=validating)
# [1] "1669/1758"
# [1] "94.94%"
# prediction
# actual     laying sitting standing walk walkdown walkup
# laying      323       6        2    0        2      1
# sitting       4     281       18    2        1      0
# standing      0      18      305    0        0      4
# walk          0       6        1  291        0      1
# walkdown      2       2        4    3      223      1
# walkup        1       5        4    1        0    246

#increased iterations
activity.lr <- multinom(activity ~ .,data=training, MaxNWts=3400, maxit=200)
p<-predict(activity.lr, newdata=validating)
getAccuracy(p,validating$activity)
# [1] "1649/1758"
# [1] "93.8%"
# prediction
# actual     laying sitting standing walk walkdown walkup
# laying      320       6        2    2        2      2
# sitting       3     282       16    4        1      0
# standing      0      17      303    3        0      4
# walk          1       6        3  288        0      1
# walkdown      4       5        5    3      216      2
# walkup        2       5        7    2        1    240

activity.lr$AIC
# [1] 4810
activity.lr$deviance
# [1] 0.0001597426

# increased size for training set
newTraining <- rbind(training,validating)
load("../../../data/processed/test.rda")
test$activity <- as.factor(test$activity)
activity.lr <- multinom(activity ~ .,data=newTraining, MaxNWts=3400, maxit=200)
p<-predict(activity.lr,newdata=test)
getAccuracy(p,test$activity)
# [1] "1320/1485"
# [1] "88.89%"
# prediction
# actual     laying sitting standing walk walkdown walkup
# laying      263       1       28    0        0      1
# sitting       0     224       38    2        0      0
# standing      0       9      271    0        2      1
# walk          1      37        1  190        0      0
# walkdown      0       6        1    0      192      1
# walkup        5      27        4    0        0    180

# Feature Selection
minimal.lr <- multinom(activity~subject,data=training)
activity.lr <- multinom(activity ~ .,data=training, MaxNWts=3400, maxit=200)
library(MASS)
step <- step(minimal.lr, scope=list(lower=minimal.lr, upper=activity.lr), direction="forward")
save(step,file="step.rda")
step$anova
# Step Df   Deviance Resid. Df Resid. Dev        AIC
# 1                                       NA         NA      4099 14640.5531 14660.5531
# 2                   + +tBodyAcc.max...X -5 7034.82898      4094  7605.7241  7635.7241
# 3                + +tGravityAcc.min...X -5 3531.84017      4089  4073.8839  4113.8839
# 4               + +tGravityAcc.mean...Y -5 1352.84342      4084  2721.0405  2771.0405
# 5          + +tGravityAcc.arCoeff...X.1 -5  832.86620      4079  1888.1743  1948.1743
# 6         + +tBodyAcc.correlation...X.Y -5  286.57784      4074  1601.5965  1671.5965
# 7          + +tGravityAcc.arCoeff...Y.2 -5  274.68481      4069  1326.9116  1406.9116
# 8             + +fBodyGyro.meanFreq...X -5  148.83203      4064  1178.0796  1268.0796
# 9               + +fBodyAccMag.energy.. -5  155.11719      4059  1022.9624  1122.9624
# 10            + +tGravityAcc.energy...Y -5   90.38836      4054   932.5741  1042.5741
# 11                 + +fBodyGyro.max...Z -5   72.57491      4049   859.9992   979.9992
# 12              + +tBodyAccJerk.std...Z -5   70.95207      4044   789.0471   919.0471
# 13             + +tBodyGyroJerk.mad...X -5   81.68773      4039   707.3594   847.3594
# 14    + +tBodyAccJerk.correlation...X.Z -5   61.04381      4034   646.3155   796.3155
# 15                  + +fBodyAcc.std...Z -5   59.87829      4029   586.4373   746.4373
# 16   + +tBodyGyroJerk.correlation...Y.Z -5   55.81161      4024   530.6256   700.6256
# 17                 + +fBodyGyro.max...Y -5   38.93975      4019   491.6859   671.6859
# 18       + +tBodyGyroJerk.arCoeff...X.3 -5   26.09147      4014   465.5944   655.5944
# 19                + +tBodyGyroMag.iqr.. -5   22.81793      4009   442.7765   642.7765
# 20           + +tBodyGyro.arCoeff...Y.1 -5   26.14863      4004   416.6279   626.6279
# 21           + +tBodyGyro.arCoeff...Y.3 -5   18.06696      3999   398.5609   618.5609
# 22 + +fBodyAccJerk.bandsEnergy...1.24.1 -5   29.11709      3994   369.4438   599.4438
# 23       + +tBodyGyroJerk.arCoeff...X.2 -5   31.41891      3989   338.0249   578.0249
# 24           + +tBodyGyro.arCoeff...X.1 -5   26.69839      3984   311.3265   561.3265
# 25            + +tBodyAcc.arCoeff...Z.4 -5   28.50100      3979   282.8255   542.8255
# 26          + +tBodyGyroJerk.energy...Y -5   14.85695      3974   267.9686   537.9686
# 27   + +tBodyGyroJerk.correlation...X.Y -5   15.10627      3969   252.8623   532.8623
# 28       + +tBodyGyro.correlation...X.Z -5   16.90746      3964   235.9548   525.9548
# 29        + +tBodyAcc.correlation...X.Z -5   13.30500      3959   222.6498   522.6498
aov <- step$anova
summary(step)

features.lr <- multinom(formula = activity ~ subject + tBodyAcc.max...X + tGravityAcc.min...X + 
                          tGravityAcc.mean...Y + tGravityAcc.arCoeff...X.1 + tBodyAcc.correlation...X.Y + 
                          tGravityAcc.arCoeff...Y.2 + fBodyGyro.meanFreq...X + fBodyAccMag.energy.. + 
                          tGravityAcc.energy...Y + fBodyGyro.max...Z + tBodyAccJerk.std...Z + 
                          tBodyGyroJerk.mad...X + tBodyAccJerk.correlation...X.Z + 
                          fBodyAcc.std...Z + tBodyGyroJerk.correlation...Y.Z + fBodyGyro.max...Y + 
                          tBodyGyroJerk.arCoeff...X.3 + tBodyGyroMag.iqr.. + tBodyGyro.arCoeff...Y.1 + 
                          tBodyGyro.arCoeff...Y.3 + fBodyAccJerk.bandsEnergy...1.24.1 + 
                          tBodyGyroJerk.arCoeff...X.2 + tBodyGyro.arCoeff...X.1 + tBodyAcc.arCoeff...Z.4 + 
                          tBodyGyroJerk.energy...Y + tBodyGyroJerk.correlation...X.Y + 
                          tBodyGyro.correlation...X.Z + tBodyAcc.correlation...X.Z, 
                        data = training,maxit=2000)
p<-predict(features.lr, newdata=validating)
getAccuracy(p,validating$activity)
# [1] "1711/1758"
# [1] "97.33%"
# prediction
# actual     laying sitting standing walk walkdown walkup
# laying      331       0        0    0        0      3
# sitting       2     288       16    0        0      0
# standing      0      19      308    0        0      0
# walk          0       0        0  297        1      1
# walkdown      0       0        0    3      232      0
# walkup        0       0        0    1        1    255

# removing subject
features.lr <- multinom(formula = activity ~ tBodyAcc.max...X + tGravityAcc.min...X + 
                          tGravityAcc.mean...Y + tGravityAcc.arCoeff...X.1 + tBodyAcc.correlation...X.Y + 
                          tGravityAcc.arCoeff...Y.2 + fBodyGyro.meanFreq...X + fBodyAccMag.energy.. + 
                          tGravityAcc.energy...Y + fBodyGyro.max...Z + tBodyAccJerk.std...Z + 
                          tBodyGyroJerk.mad...X + tBodyAccJerk.correlation...X.Z + 
                          fBodyAcc.std...Z + tBodyGyroJerk.correlation...Y.Z + fBodyGyro.max...Y + 
                          tBodyGyroJerk.arCoeff...X.3 + tBodyGyroMag.iqr.. + tBodyGyro.arCoeff...Y.1 + 
                          tBodyGyro.arCoeff...Y.3 + fBodyAccJerk.bandsEnergy...1.24.1 + 
                          tBodyGyroJerk.arCoeff...X.2 + tBodyGyro.arCoeff...X.1 + tBodyAcc.arCoeff...Z.4 + 
                          tBodyGyroJerk.energy...Y + tBodyGyroJerk.correlation...X.Y + 
                          tBodyGyro.correlation...X.Z + tBodyAcc.correlation...X.Z, 
                        data = training,maxit=2000)
p<-predict(features.lr, newdata=validating)
getAccuracy(p,validating$activity)
# [1] "1717/1758"
# [1] "97.67%"
# prediction
# actual     laying sitting standing walk walkdown walkup
# laying      334       0        0    0        0      0
# sitting       0     289       16    1        0      0
# standing      0      20      307    0        0      0
# walk          0       0        0  298        0      1
# walkdown      0       0        0    1      233      1
# walkup        0       0        0    1        0    256

# test accuracy:
# [1] "1424/1485"
# [1] "95.89%"
# prediction
# actual     laying sitting standing walk walkdown walkup
# laying      293       0        0    0        0      0
# sitting       0     232       32    0        0      0
# standing      0      21      261    1        0      0
# walk          0       0        0  229        0      0
# walkdown      0       0        0    0      199      1
# walkup        0       0        0    4        2    210

# trying stepwise forward again with tBodyAcc.max...X as the minimum variable
minimal.lr <- multinom(activity~tBodyAcc.max...X,data=training)
activity.lr <- multinom(activity ~ .,data=training, MaxNWts=3400, maxit=200)
step2 <- step(minimal.lr, scope=list(lower=minimal.lr, upper=activity.lr), direction="forward")
save(step2,file="step2.rda")
step2$anova
features.lr <- multinom(formula = activity ~ tBodyAcc.max...X + tGravityAcc.min...X + 
                          tGravityAcc.mean...Y + tGravityAcc.arCoeff...X.1 + tBodyAcc.correlation...X.Y + 
                          tGravityAcc.arCoeff...Y.2 + fBodyGyro.meanFreq...X + fBodyAccMag.energy.. + 
                          tBodyGyroJerk.correlation...X.Z + fBodyGyro.kurtosis...Z + 
                          tBodyGyro.correlation...Y.Z + fBodyAcc.bandsEnergy...9.16.1 + 
                          fBodyBodyAccJerkMag.std.. + tBodyGyroJerk.correlation...X.Y + 
                          fBodyGyro.entropy...X + tBodyGyro.mad...Z + fBodyAccJerk.meanFreq...Z + 
                          tBodyAccJerk.entropy...X + tBodyGyroMag.entropy.. + tGravityAcc.sma.. + 
                          tBodyGyroJerk.entropy...X + tBodyGyro.mean...X + tBodyAcc.min...X + 
                          tBodyAccJerk.correlation...X.Z + tBodyGyro.arCoeff...Y.1 + 
                          tBodyGyro.arCoeff...Y.3 + fBodyAccJerk.entropy...Z + tBodyGyroJerk.arCoeff...X.2 + 
                          tBodyGyro.arCoeff...X.1 + fBodyAcc.maxInds.Y + tBodyGyroJerk.arCoeff...X.3 + 
                          tBodyAcc.arCoeff...Z.4, data = training, maxit=2000)
p<-predict(features.lr, newdata=validating)
getAccuracy(p,validating$activity)
# [1] "1702/1758"
# [1] "96.81%"
# prediction
# actual     laying sitting standing walk walkdown walkup
# laying      332       0        0    2        0      0
# sitting       1     287       14    3        1      0
# standing      0      14      311    2        0      0
# walk          0       0        0  292        5      2
# walkdown      2       0        1    1      231      0
# walkup        0       0        1    4        3    249

load("../../../data/processed/test.rda")
test$activity <- as.factor(test$activity)
p<-predict(features.lr, newdata=test)
getAccuracy(p,test$activity)
# [1] "1378/1485"
# [1] "92.79%"
# prediction
# actual     laying sitting standing walk walkdown walkup
# laying      292       0        0    1        0      0
# sitting       0     231       33    0        0      0
# standing      0      18      265    0        0      0
# walk          0       0        0  222        7      0
# walkdown      0       0        0    0      199      1
# walkup        3       0        4    0       40    169

# CORRELATION
cor(training[,-563], as.numeric(training[,563]))
# Highest correlation with 0.842357256 tBodyGyroJerk.entropy...Z
minimal.lr <- multinom(activity~tBodyGyroJerk.entropy...Z,data=training)
activity.lr <- multinom(activity ~ .,data=training, MaxNWts=3400, maxit=200)
step <- step(minimal.lr, scope=list(lower=minimal.lr, upper=activity.lr), direction="forward")
features.lr <- multinom(formula = activity ~ tBodyGyroJerk.entropy...Z + tGravityAcc.min...X + 
           fBodyAccMag.mad.. + tGravityAcc.mean...Y + tGravityAcc.arCoeff...Y.2 + 
           tBodyAcc.correlation...X.Y + tBodyGyro.arCoeff...X.1 + tGravityAcc.arCoeff...X.1 + 
           tBodyGyroJerk.entropy...X + tBodyAccJerk.entropy...Z + tBodyAccJerkMag.iqr.. + 
           tBodyAcc.std...X + tBodyGyroJerk.correlation...X.Z + tBodyGyroJerk.arCoeff...X.2 + 
           fBodyAccJerk.meanFreq...Z + tBodyGyro.arCoeff...Y.1 + fBodyGyro.max...Z + 
           tBodyGyroJerk.iqr...Z + tBodyGyro.arCoeff...Y.2 + tBodyGyro.mean...X + 
           tBodyGyroJerk.correlation...X.Y + tBodyGyroJerk.arCoeff...X.3 + 
           tGravityAcc.arCoeff...Y.4 + tBodyGyro.correlation...Y.Z + 
           tBodyGyroJerk.mean...X + tGravityAcc.arCoeff...Z.4 + tBodyGyroJerk.entropy...Y + 
           tBodyGyro.max...X, data = training, maxit=200)
# AIC: 486.322
p<-predict(features.lr,newdata=validating)
getAccuracy(p,validating$activity)
# [1] "1703/1758"
# [1] "96.87%"
# prediction
# actual     laying sitting standing walk walkdown walkup
# laying      333       0        0    0        0      1
# sitting       1     287       16    1        1      0
# standing      0      14      312    1        0      0
# walk          1       0        0  287        8      3
# walkdown      0       0        0    3      231      1
# walkup        0       0        1    1        2    253

# test
p<-predict(features.lr,newdata=test)
getAccuracy(p,test$activity)
# [1] "1432/1485"
# [1] "96.43%"
# prediction
# actual     laying sitting standing walk walkdown walkup
# laying      293       0        0    0        0      0
# sitting       0     235       29    0        0      0
# standing      0       9      274    0        0      0
# walk          0       0        0  222        0      7
# walkdown      0       0        0    0      197      3
# walkup        0       0        0    1        4    211


# confounders amongst 28 selected variables and outcome variable
t<-training[c('activity','tBodyGyroJerk.entropy...Z','tGravityAcc.min...X','fBodyAccMag.mad..','tGravityAcc.mean...Y','tGravityAcc.arCoeff...Y.2','tBodyAcc.correlation...X.Y','tBodyGyro.arCoeff...X.1','tGravityAcc.arCoeff...X.1','tBodyGyroJerk.entropy...X','tBodyAccJerk.entropy...Z','tBodyAccJerkMag.iqr..','tBodyAcc.std...X','tBodyGyroJerk.correlation...X.Z','tBodyGyroJerk.arCoeff...X.2','fBodyAccJerk.meanFreq...Z','tBodyGyro.arCoeff...Y.1','fBodyGyro.max...Z','tBodyGyroJerk.iqr...Z','tBodyGyro.arCoeff...Y.2','tBodyGyro.mean...X','tBodyGyroJerk.correlation...X.Y','tBodyGyroJerk.arCoeff...X.3','tGravityAcc.arCoeff...Y.4','tBodyGyro.correlation...Y.Z','tBodyGyroJerk.mean...X','tGravityAcc.arCoeff...Z.4','tBodyGyroJerk.entropy...Y','tBodyGyro.max...X')]
t$activity<-as.numeric(t$activity)
library(corrgram)
corrgram(t,lower.panel=panel.shade,upper.panel=panel.pie, text.panel=panel.txt)
at <- t[c(1,2,3,4,5,8,10,11,12,13,16,18,19,28,29)]
corrgram(at,lower.panel=panel.shade,upper.panel=panel.pie, text.panel=panel.txt)
# collinear: 7&8,12&13,14&15
st <- at[c(1,2,3,4,5,6,7,9,10,11,12,13,14)]
corrgram(st,lower.panel=NULL,upper.panel=panel.pie, text.panel=panel.txt)
remaining <- at[-1]
form <- as.formula(paste("activity~",paste(names(remaining),collapse="+")))
remaining.lr <- multinom(form,data=training,maxit=300)
p<-predict(remaining.lr,newdata=validating)
getAccuracy(p,validating$activity)
[1] "93.91%"