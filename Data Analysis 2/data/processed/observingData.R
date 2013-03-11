load("../raw/samsungData.rda")

# investigating duplicate variable names
dups <- sapply(origNames,function(x)length(origNames[origNames==x]))
# dups
# 1   3 
# 437 126 
which(duplicated(origNames))
name <- origNames[317]
names(samsungData[which(origNames==name)])
plot(samsungData[,307])
points(samsungData[,317],col="red")
points(samsungData[,331],col="green")

data <- data.frame(samsungData)
attach(data)

# Missing Values
any(showMissing(data))
[1] FALSE

# Outliers
mins <- apply(data[-c(562,563)],2,function(x)min(as.numeric(x)))
any(mins< -1)
[1] FALSE

maxs <- apply(data[-c(562,563)],2,function(x)max(as.numeric(x)))
any(maxs > 1)
[1] FALSE

save(data,file="data.rda")