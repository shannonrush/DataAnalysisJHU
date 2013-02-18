cleanData <- function() {
  load("../raw/loansData.rda") # loansData 2500 x 14
  
  # there are so few NAs probably ok to just remove them
  data <- na.omit(loansData)
  
  # Add FICO, the lower score of FICO.Range
  data$FICO <- sapply(data$FICO.Range, function(x) as.numeric(sub("-.*","",x)))
  data$FICO.Range <- NULL
  
  # Add Rate, the numeric value of Interest.Rate
  data$Rate <- sapply(data$Interest.Rate, function(x) as.numeric(sub("%","",x)))
  data$Interest.Rate <- NULL
  
  # Convert Debt to Income Ratio to numeric
  data$Debt.Income.Ratio <- sapply(data$Debt.To.Income.Ratio, function(x)as.numeric(sub("%","",x)))
  data$Debt.To.Income.Ratio <- NULL
  
  # Add Income.Level, factor of Monthly.Income
  data$Adj.Income <- sapply(data$Monthly.Income, function(x) ifelse (x>10000,10000,x)) 
  })
  data$Income.Level <- factor(cut(data$Adj.Income,5,labels=F))
  data$Income.Levels <- factor(cut(data$Adj.Income,5))
  
  save(data,file="loansData_Clean.rda")
}