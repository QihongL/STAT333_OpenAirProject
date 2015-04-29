# Some preliminary analysis for potential data for 333 prject 

setwd('/Users/Qihong/Code/github/STAT333_OpenAirProject')

# setwd('/Users/Qihong/Code/github/STAT333_Project')

rm(list = ls())
library(GGally)
library(car)                                                                                                                                  
library(plyr)
library(leaps)
library(perturb)
library(MASS)
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

##################
# Modifying the data 
##################
# temp: trim the dimentionality of the input space (to plot data)
numObsSelect = 500
mydataTrim = data.frame(mydata[1:numObsSelect,2:10])
mydata = mydata[1:numObsSelect,]

mydataUNS = as.data.frame(scale(mydata[,2:10]))
#Fitting data by year
mydata1998 = mydata[1:3311, ] #1998
mydata1999 = mydata[3312:9440, ] #1999
mydata2000 = mydata[9441:16433, ] #2000
mydata2001 = mydata[16434:22295, ] #2001
mydata2002 = mydata[22296:29852, ] #2002
mydata2003 = mydata[29853:37117, ] #2003
mydata2004 = mydata[37118:42893, ] #2004

####################################
# glance at the data
####################################
# some summaries
head(mydata)
summary(mydata)

# scatter matrix and correlation matrix 
# corr with Y & multicollinearity between X detected 
ggpairs(mydataTrim)
# plot(mydataTrim, pch = 20)

########################
# plot data aginst time (for 1st 300 obs)
########################
# we see clear autocorrelation when plotting 300 obs,
# it is not obvious in the long run... might need formal test
par(mfrow=c(3,3)) 
for (i in 1:length(mydataTrim)){
    plot(1:dim(mydataTrim[i])[1], mydataTrim[,i], pch = 20)  
    title(colnames(mydataTrim)[i])
}
mtext("Plot all predictors against time", side = 3, line = -1.5, outer = TRUE)
# 
# # box plot, check outliers
par(mfrow=c(3,3)) 
for (i in 1:length(mydataTrim)){
    boxplot(mydataTrim[,i])
    title(colnames(mydataTrim)[i])
}
mtext("Box Plot for all predictors", side = 3, line = -1.5, outer = TRUE)


####################################
# Analysis, fitting models 
####################################

# fitting naive models
lm.fit_full = lm(pm10 ~ ws+ wd + nox +no2 +o3 +so2 +co, data = mydataUNS)
lm.fit_all = lm(pm10 ~ ws* wd* nox *no2 *o3 *so2 *co, data = mydataUNS)
lm.fit_null = lm(pm10 ~ 1, data = mydataUNS)

# step(lm.fit_null, scope=list(lowr=lm.fit_null, upper=lm.fit_all), direction="both")

step(lm.fit_null, scope=list(lowr=lm.fit_null, upper=lm.fit_full), direction="both")

# all regression
out = All_reg(pm10 ~ ws +wd +no2 +nox +o3 +so2 +co, data = mydataUNS, nbest=4, nvmax=6)


# WEIGETED LEAST SQUARE
lm.fit_wls = lm (pm10 ~  wd+ nox+ no2+ o3+ so2+ co, data = mydataUNS, weights = mydataUNS$nox)

# Fitting RIDGE regression
lm.fit_ridge = lm.ridge(pm10 ~  wd+ nox+ no2+ o3+ so2+ co, data = mydataUNS)


par(mfrow = c(2,2))

lm.fit_temp = lm.fit_full
plot(lm.fit_temp)
vif(lm.fit_temp)




##############################
# plot variable selection criteria
##############################
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

lm.fit_best = lm(pm10 ~ no2 + so2 + o3 + wd + ws + co , data = mydataUNS )

# # residuals 
par(mfrow=c(3,3)) 
lm.fit_temp = lm.fit_best
plot(lm.fit_temp$residuals ~ lm.fit_temp$fitted.values, pch = 20, 
     main = 'Residuals against fitted values', xlab = 'fitted values', ylab = 'residuals')
abline(0,0)

plot(lm.fit_temp$residuals, pch = 20, 
     main = 'Residuals against time', xlab = 'time', ylab = 'residuals')
abline(0,0)

# non-constancy variance! 
plot(lm.fit_temp$residuals ~ mydata$ws, pch = 20, 
     main = 'Residuals against wind speed', xlab = 'wind speed', ylab = 'residuals')
abline(0,0)

plot(lm.fit_temp$residuals ~ mydata$wd, pch = 20, 
     main = 'Residuals against wind direction', xlab = 'wind direction', ylab = 'residuals')
abline(0,0)

plot(lm.fit_temp$residuals ~ mydata$nox, pch = 20, 
     main = 'Residuals against nox', xlab = 'nox', ylab = 'residuals')
abline(0,0)

plot(lm.fit_temp$residuals ~ mydata$no2, pch = 20, 
     main = 'Residuals against no2', xlab = 'no2', ylab = 'residuals')
abline(0,0)

# non-constancy variance
plot(lm.fit_temp$residuals ~ mydata$o3, pch = 20, 
     main = 'Residuals against o3', xlab = 'o3', ylab = 'residuals')
abline(0,0)

plot(lm.fit_temp$residuals ~ mydata$so2, pch = 20, 
     main = 'Residuals against so2', xlab = 'so2', ylab = 'residuals')
abline(0,0)

# non-contancy variance
plot(lm.fit_temp$residuals ~ mydata$co, pch = 20, 
     main = 'Residuals against co', xlab = 'co', ylab = 'residuals')
abline(0,0)


####### TODO ##############
## check multicolinearity
## variable selections
## residual analysis
## standardization
## regularization 
###########################