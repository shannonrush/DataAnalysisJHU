## Statistical Analyses for loan Interest Rate based on Amount Requested,
## taking into account FICO credit score
load("../../data/processed/loansData_Clean.rda")
lm1 <- lm(data$Rate~data$Amount.Requested)
summary(lm1)
# Call:
#   lm(formula = data$Rate ~ data$Amount.Requested)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -10.3756  -3.0095   0.0255   2.7173  11.9079 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           1.086e+01  1.482e-01   73.28   <2e-16 ***
#   data$Amount.Requested 1.777e-04  1.011e-05   17.57   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
# 
# Residual standard error: 3.942 on 2496 degrees of freedom
# Multiple R-squared: 0.1101,  Adjusted R-squared: 0.1098 
# F-statistic: 308.8 on 1 and 2496 DF,  p-value: < 2.2e-16 
anova(lm1)
# Analysis of Variance Table
# 
# Response: data$Rate
# Df Sum Sq Mean Sq F value    Pr(>F)    
# data$Amount.Requested    1   4799  4799.4  308.85 < 2.2e-16 ***
#   Residuals             2496  38788    15.5                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
confint(lm1)
#2.5 %       97.5 %
#   (Intercept)           1.057369e+01 1.115509e+01
# data$Amount.Requested 1.578854e-04 1.975443e-04

lm1$coefficients
# (Intercept) data$Amount.Requested 
# 1.086439e+01          1.777148e-04 
confint(lm1)
# 2.5 %       97.5 %
#   (Intercept)           1.057369e+01 1.115509e+01
# data$Amount.Requested 1.578854e-04 1.975443e-04

# A $1 increase in amount requested is associated with a .0017% interest rate increase (95% CI: .0016% - .002%)
  
  