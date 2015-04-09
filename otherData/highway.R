# Some preliminary analysis for potential data for 333 prject 
setwd('/Users/Qihong/Code/github/STAT333_RegressionAnalysis/333projectCode')
rm(list = ls())
mydata = read.csv('databases/Highway1.csv')

head(mydata)
plot(mydata)

lm.fit = lm(mydata$rate ~ mydata$trks * mydata$sigs1 * mydata$acpt)
summary(lm.fit)
anova(lm.fit)

