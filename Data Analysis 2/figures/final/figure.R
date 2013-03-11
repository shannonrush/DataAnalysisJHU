load("../../data/processed/test.rda")
test$activity <- as.factor(test$activity)
table(test$activity)

laying  sitting standing     walk walkdown   walkup 
293      264      283      229      200      216 

# MLR

[1] "1432/1485"
[1] "96.43%"
prediction
actual     laying sitting standing walk walkdown walkup
laying      293       0        0    0        0      0
sitting       0     235       29    0        0      0
standing      0       9      274    0        0      0
walk          0       0        0  222        0      7
walkdown      0       0        0    0      197      3
walkup        0       0        0    1        4    211

mlr <- round(c(0,29/264,9/283,7/229,3/200,5/216) * 100,2)

# Random Forest 

[1] "1413/1485"
[1] "95.15%"
prediction
actual     laying sitting standing walk walkdown walkup
laying      293       0        0    0        0      0
sitting       0     227       37    0        0      0
standing      0      27      256    0        0      0
walk          0       0        0  229        0      0
walkdown      0       0        0    3      193      4
walkup        0       0        0    1        0    215

rf <- round(c(0,37/264,27/283,0,7/200,1/216) * 100,2)

df <- data.frame(models = factor(c(rep("Random Forest",6),rep("Multinomial Logistic Regression",6))),
                 activities = factor(rep(c("Laying Down","Sitting","Standing","Walking","Walking Down","Walking Up"),2)),
                 levels = c("Laying Down","Sitting","Standing","Walking","Walking Down","Walking Up"),
                 percents = c(rf,mlr)
                 )

ggplot(data=df, aes(x=activities, y=percents, fill=models)) + geom_bar(stat="identity", position=position_dodge(), colour="black") + scale_fill_manual(values=c("#999999", "#E69F00"),name="")+labs(title="Percentage Misclassified By Activity Performed", x="Activity Performed", y="Percentage Misclassified")+theme(legend.text = element_text(size=15),plot.title=element_text(size=16, vjust=1),axis.text.x=element_text(size=14),axis.text.y=element_text(size=14),axis.title.x=element_text(size=15, vjust=-.3),axis.title.y=element_text(size=15))