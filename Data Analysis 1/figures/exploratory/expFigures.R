load("../../data/processed/loansData_Clean.rda") # data 2498 x 17

FICOvsRate <- function() {
  plot(data$FICO,data$Rate,pch=19,cex=0.5)
  lm1<-lm(data$Rate~data$FICO)
  lines(data$FICO,lm1$fitted,col="red",lwd=3)
}

FICOvsRatewInquiriesLM <- function() {
  palette(rainbow(10))
  inquiries<-as.factor(data$Inquiries.in.the.Last.6.Months)
  cols<-as.numeric(levels(inquiries))+1
  plot(data$FICO,data$Rate,pch=19,cex=0.5,col=cols)
  legend("topright",levels(inquiries),fill=cols)
  for (i in 0:9) {
    lm1 <- lm(data$Rate[data$Inquiries.in.the.Last.6.Months==i]~data$FICO[data$Inquiries.in.the.Last.6.Months==i])
    print(summary(lm1))
    lines(data$FICO[data$Inquiries.in.the.Last.6.Months==i],lm1$fitted,col=palette()[i+1],lwd=3)
  }
}

RatevsInquiries <- function() {
  inquiries<-as.factor(data$Inquiries.in.the.Last.6.Months)
  plot(data$Rate~data$Inquiries.in.the.Last.6.Months,col=data$FICO.Level,pch=19,cex=0.5)
  legend("topright",levels(data$FICO.Levels),fill=levels(data$FICO.Level),title="FICO")
}

FICOvsRatewIncome <- function() {
  plot(data$FICO,data$Rate,pch=19,cex=0.5,col=data$Income.Level,main="Rate By FICO and Income")
  legend("topright",levels(data$Income.Levels),fill=levels(data$Income.Level))
}

AmountRequestedvsRate <- function() {
  palette(rainbow(5))
  plot(data$Amount.Requested,data$Rate,pch=19,cex=0.5,col=data$FICO.Level)
  par(xpd=TRUE)
  legend(-20,30,levels(data$FICO.Levels),fill=levels(data$FICO.Level),title="FICO")
  lm1<-lm(data$Rate~data$Amount.Requested)
  lines(data$Amount.Requested,lm1$fitted,col="black",lwd=3)
  for (i in 1:5) {
    lm1 <- lm(data$Rate[data$FICO.Level==i]~data$Amount.Requested[data$FICO.Level==i])
    print(summary(lm1))
    lines(data$Amount.Requested[data$FICO.Level==i],lm1$fitted,col=palette()[i],lwd=3)
  }
}

InquiriesVsRate <- function() {
  palette(rainbow(5))
  plot(data$Inquiries.in.the.Last.6.Months,data$Rate,pch=19,cex=0.5,col=data$FICO.Level)
  par(xpd=TRUE)
  legend(-20,30,levels(data$FICO.Levels),fill=levels(data$FICO.Level),title="FICO")
  lm1<-lm(data$Rate~data$Inquiries.in.the.Last.6.Months)
  lines(data$Inquiries.in.the.Last.6.Months,lm1$fitted,col="black",lwd=3)
  for (i in 1:5) {
    lm1 <- lm(data$Rate[data$FICO.Level==i]~data$Inquiries.in.the.Last.6.Months[data$FICO.Level==i])
    print(summary(lm1))
    lines(data$Inquiries.in.the.Last.6.Months[data$FICO.Level==i],lm1$fitted,col=palette()[i],lwd=3)
  }
}

InquiriesvsRate <- function() {
  plot(data$Inquiries.in.the.Last.6.Months,data$Rate,pch=19,cex=0.5,col=data$FICO.Level)
  par(xpd=TRUE)
  legend(-20,30,levels(data$FICO.Levels),fill=levels(data$FICO.Level),title="FICO")
  lm1<-lm(data$Rate~data$Inquiries.in.the.Last.6.Months)
  lines(data$Inquiries.in.the.Last.6.Months,lm1$fitted,col="black",lwd=3)
}

FICOvRatewLength <- function() {
  # Rate is consistently higher for 60 month loans vs 36 month loans
  plot(data$FICO,data$Rate,pch=19,cex=0.5,col=as.numeric(factor(data$Loan.Length)))
  legend("topright",levels(factor(data$Loan.Length)),fill=levels(factor(as.numeric(factor(data$Loan.Length)))),title="Loan Length")
}

addFICOLevels <- function() {
  data$FICO.Level <- factor(cut(data$FICO,5,labels=F))
  data$FICO.Levels <- cut(data$FICO,5)
}

RequestedvsFunded <- function() {
  plot(data$Amount.Requested,data$Amount.Funded.By.Investors,pch=19,cex=0.5,col=data$FICO.Level)
  legend("topleft",levels(data$FICO.Levels),fill=levels(data$FICO.Level),title="FICO")
}

FICOvRatewPurpose <- function() {
  data$Purpose
  plot(data$FICO,data$Rate,pch=19,cex=0.5,col=as.numeric(data$Loan.Purpose))
  legend("topright",levels(data$Loan.Purpose),fill=levels(factor(as.numeric(data$Loan.Purpose))),title="Loan Purpose")
}

OpenCreditLinesvsRatewFICO <- function() {
  plot(data$Open.CREDIT.Lines,data$Rate,pch=19,cex=0.5,col=data$FICO.Level)
  par(xpd=TRUE)
  legend(-20,30,levels(data$FICO.Levels),fill=levels(data$FICO.Level),title="FICO")
  lm1<-lm(data$Rate~data$Open.CREDIT.Lines)
  lines(data$Open.CREDIT.Lines,lm1$fitted,col="black",lwd=3)
}

