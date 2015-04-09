# Some preliminary analysis for potential data for 333 prject 
setwd('/Users/Qihong/Code/github/STAT333_RegressionAnalysis/333projectCode')
rm(list = ls())
library('GGally')
mydata = read.csv('databases/Crime.csv')

# verify the dimensionality of the input space
dim(mydata)
mydata = mydata[1:300,5:14]
pairs(mydata)
head(mydata)

par(mfrow = c(1,2))
plot(mydata$crmrte ~ mydata$density, pch = 16)
plot(mydata$crmrte ~ mydata$taxpc, pch = 16)

lm.fit = lm(mydata$crmrte ~ mydata$density * mydata$taxpc)
summary(lm.fit)
