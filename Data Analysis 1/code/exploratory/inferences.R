load("../../data/processed/loansData_Clean.rda") # data 2498 x 17

# Candidates:
# Amount.Requested
# Open.CREDIT.Lines
# Inquiries.in.the.Last.6.Months

# > quantile(data$Amount.Requested)
# 0%   25%   50%   75%  100% 
# 1000  6000 10000 17000 35000 
# > quantile(data$Open.CREDIT.Lines)
# 0%  25%  50%  75% 100% 
# 2    7    9   13   38 
# > quantile(data$Inquiries.in.the.Last.6.Months)
# 0%  25%  50%  75% 100% 
# 0    0    0    1    9 
# > table(data$Inquiries.in.the.Last.6.Months)
# 
# 0    1    2    3    4    5    6    7    8    9 
# 1250  657  336  169   50   14    8    7    2    5 

## Confidence Intervals

amount.requested.lm <- lm(data$Rate~data$Amount.Requested)
amount.requested.lm$coefficients
# (Intercept) data$Amount.Requested 
# 1.086439e+01          1.777148e-04 
confint(amount.requested.lm)
# 2.5 %       97.5 %
#   (Intercept)           1.057369e+01 1.115509e+01
# data$Amount.Requested 1.578854e-04 1.975443e-04

# A $1 increase in amount requested is associated with a .0017% interest rate increase (95% CI: .0016% - .002%)

open.credit.lines.lm <- lm(data$Rate~data$Open.CREDIT.Lines)
open.credit.lines.lm$coefficients
# (Intercept) data$Open.CREDIT.Lines 
# 12.22719288             0.08368438 
confint(open.credit.lines.lm)
# 2.5 %     97.5 %
#   (Intercept)            11.82736409 12.6270217
# data$Open.CREDIT.Lines  0.04746163  0.1199071

# 1 additional open credit line is associated with a .0837% interest rate increase (95% CI: .0475%-0.1199%)

inquiries.lm <- lm(data$Rate~data$Inquiries.in.the.Last.6.Months)
inquiries.lm$coefficients
# (Intercept) data$Inquiries.in.the.Last.6.Months 
# 12.5639205                           0.5587927 
confint(inquiries.lm)
# 2.5 %     97.5 %
#   (Intercept)                         12.3630906 12.7647504
# data$Inquiries.in.the.Last.6.Months  0.4274013  0.6901841

# 1 additional inquiry is associated with a .5588% interest rate increase (95% CI: .4274%-.6902%)

## P-Values
summary(amount.requested.lm)
#p-value: < 2.2e-16 
summary(open.credit.lines.lm)
#p-value: 6.169e-06
summary(inquiries.lm)
#p-value: < 2.2e-16 

