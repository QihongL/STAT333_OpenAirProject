# Some preliminary analysis for potential data for 333 prject 
setwd('/Users/Qihong/Code/github/STAT333_Project')
rm(list = ls())
library(GGally)
library(car)                                                                                                                                  
library(plyr)
library(leaps)
source('All_reg.R')

################
#PRE-PROCESSING
################
# load the data
mydata = read.csv('data/OpenAir_example_data_long.csv')
dim(mydata)

# eliminate rows with missing values (NA)
mydata = mydata[complete.cases(mydata), ]
# switch the response variables to the end of the dataframe
mydata[length(mydata) + 1] = mydata[7]
mydata[7] = NULL
colnames(mydata)[10] = 'pm10'


####################################
# glance at the data
####################################
# temp: trim the dimentionality of the input space (to plot data)
numObsSelect = 1000
mydataTrim = data.frame(mydata[1:numObsSelect,2:10])
mydata = mydata[1:numObsSelect,]

# some summaries
head(mydata)
summary(mydata)

# scatter matrix and correlation matrix 
# corr with Y & multicollinearity between X detected 
# ggpairs(mydataTrim)
# plot(mydataTrim, pch = 20)

# plot data aginst time (for 1st 300 obs)
# we see clear autocorrelation when plotting 300 obs,
# it is not obvious in the long run... might need formal test
# par(mfrow=c(3,3)) 
# for (i in 1:length(mydataTrim)){
#     plot(1:dim(mydataTrim[i])[1], mydataTrim[,i], pch = 20)  
#     title(colnames(mydataTrim)[i])
# }
# mtext("Plot all predictors against time", side = 3, line = -1.5, outer = TRUE)

# box plot, check outliers
# par(mfrow=c(3,3)) 
# for (i in 1:length(mydataTrim)){
#     boxplot(mydataTrim[,i])
#     title(colnames(mydataTrim)[i])
# }
# mtext("Box Plot for all predictors", side = 3, line = -1.5, outer = TRUE)



####################################
# Analysis, fitting models 
####################################

# fitting naive models
lm.fit_full = lm(mydata$pm10 ~ mydata$ws+ mydata$wd + mydata$nox +mydata$no2 +mydata$o3 +mydata$so2 +mydata$co)
lm.fit_all = lm(mydata$pm10 ~ mydata$ws* mydata$wd* mydata$nox *mydata$no2 *mydata$o3 *mydata$so2 *mydata$co)
lm.fit_null = lm(mydata$pm10 ~ 1, data = mydata)

step(lm.fit_null, scope=list(lowr=lm.fit_null, upper=lm.fit_all), direction="both")

# all regression
out = All_reg(pm10 ~ ws +wd +nox +no2 +o3 +so2 +co, data = mydata, nbest=4, nvmax=6)

fit = lm (y ~ x, data=dat,weights=(1/dat$x^2))






##############################
# plot variable selection criteria
##############################
par(mfrow=c(2,2)) 
plot(out$RSQ ~ out$P, pch = 20, main = 'R against P', 
     ylab = 'R', xlab = 'number of parameters')
plot(out$RSQ_A ~ out$P, pch = 20, main = 'Adjusted R sq against P', 
     ylab = 'Adjusted R sq', xlab = 'number of parameters')
plot(out$Cp ~ out$P, pch = 20, main = 'Cp against P', 
     ylab = 'Cp', xlab = 'number of parameters')
abline(0,1)
plot(out$BIC ~ out$P, pch = 20, main = 'BIC against P', 
     ylab = 'BIC', xlab = 'number of parameters')







####################
# Check the model
####################
# plot(lm.fit_all)
# QQ plot indicating abnormality

# 
# # residuals 
# par(mfrow=c(3,3)) 
# lm.fit_temp = lm.fit_full
# plot(lm.fit_temp$residuals ~ lm.fit_temp$fitted.values, pch = 20, 
#      main = 'Residuals against fitted values', xlab = 'fitted values', ylab = 'residuals')
# abline(0,0)
# 
# plot(lm.fit_temp$residuals, pch = 20, 
#      main = 'Residuals against time', xlab = 'time', ylab = 'residuals')
# abline(0,0)
# 
# # non-constancy variance! 
# plot(lm.fit_temp$residuals ~ mydata$ws, pch = 20, 
#      main = 'Residuals against wind speed', xlab = 'wind speed', ylab = 'residuals')
# abline(0,0)
# 
# plot(lm.fit_temp$residuals ~ mydata$wd, pch = 20, 
#      main = 'Residuals against wind direction', xlab = 'wind direction', ylab = 'residuals')
# abline(0,0)
# 
# plot(lm.fit_temp$residuals ~ mydata$nox, pch = 20, 
#      main = 'Residuals against nox', xlab = 'nox', ylab = 'residuals')
# abline(0,0)
# 
# plot(lm.fit_temp$residuals ~ mydata$no2, pch = 20, 
#      main = 'Residuals against no2', xlab = 'no2', ylab = 'residuals')
# abline(0,0)
# 
# # non-constancy variance
# plot(lm.fit_temp$residuals ~ mydata$o3, pch = 20, 
#      main = 'Residuals against o3', xlab = 'o3', ylab = 'residuals')
# abline(0,0)
# 
# plot(lm.fit_temp$residuals ~ mydata$so2, pch = 20, 
#      main = 'Residuals against so2', xlab = 'so2', ylab = 'residuals')
# abline(0,0)
# 
# # non-contancy variance
# plot(lm.fit_temp$residuals ~ mydata$co, pch = 20, 
#      main = 'Residuals against co', xlab = 'co', ylab = 'residuals')
# abline(0,0)


####### TODO ##############
## check multicolinearity
## variable selections
## residual analysis
## standardization
## regularization 
###########################