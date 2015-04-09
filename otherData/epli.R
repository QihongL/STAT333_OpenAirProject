# Some preliminary analysis for potential data for 333 prject 
setwd('/Users/Qihong/Code/github/STAT333_RegressionAnalysis/333projectCode')
rm(list = ls())
library('ggplot2')
mydata = read.csv('databases/epil.csv')
mydata = mydata[,2:10]
# verify the dimensionality of the input space
dim(mydata)
pairs(mydata)


lm.fit = lm(mydata$y ~ mydata$base * mydata$age * mydata$subject * mydata$lbase * mydata$lage)
summary(lm.fit)
