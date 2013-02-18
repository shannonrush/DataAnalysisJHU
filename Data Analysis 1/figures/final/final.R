figureFinal <- function() {
  palette(rainbow(5))
  par(mar=c(5,4,4,7) + 0.1,xpd=TRUE)
  plot(data$Amount.Requested,data$Rate,pch=19,cex=0.5,col=data$FICO.Level,
       main="Increased Interest Rate Associated With Increased Amount Requested",
       xlab="Loan Amount Requested (US$)",
       ylab="Loan Interest Rate (%)")
  legendLabels <- c("641-678","679-716","717-754","755-792","793-830","All")
  legend(list(x=37000,y=25),legendLabels,fill=c(levels(data$FICO.Level),"black"),title="FICO Score")
  lm1<-lm(data$Rate~data$Amount.Requested)
  lines(data$Amount.Requested,lm1$fitted,col="black",lwd=3)
  for (i in 1:5) {
    lm1 <- lm(data$Rate[data$FICO.Level==i]~data$Amount.Requested[data$FICO.Level==i])
    lines(data$Amount.Requested[data$FICO.Level==i],lm1$fitted,col=palette()[i],lwd=3)
    print(i)
    print(lm1$coefficients)
    print(confint(lm1))
  }
}


