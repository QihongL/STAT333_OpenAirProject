# code for 333 prject 
rm(list = ls())
setwd('/Users/Qihong/Code/github/STAT333_OpenAirProject')
library(GGally); library(car); library(plyr); library(leaps); 
library(perturb); library(MASS); source('All_reg.R')
# load the data
rawdata = read.csv('data/OpenAir_example_data_long.csv')

################
#PRE-PROCESSING
################
# eliminate rows with missing values (NA)
mydata = rawdata[complete.cases(rawdata), ]
# switch the response variables to the end of the dataframe
mydata[length(mydata)] = mydata[7]
mydata[7] = NULL
colnames(mydata)[9] = 'pm10'
rm(rawdata) # comment this line if raw data is needed 


##################
# Modifying the data 
##################
# temp: trim the dimentionality of the input space (to plot data)
numObsSelect = 500
mydataTrim = data.frame(mydata[1:numObsSelect,2:length(mydata)])
# mydata = mydata[1:numObsSelect,]

# unit normal scaling 
mydataUNS = as.data.frame(scale(mydata[,2:length(mydata)]))

yrInd = c(1, 3312, 9441, 16434, 22296, 39853, 37118)
# mydata1998 = mydata[yrInd[1]:dim(mydata)[1], ] #1998 after 
# mydata1999 = mydata[yrInd[2]:dim(mydata)[1], ] #1999 after 
# mydata2000 = mydata[yrInd[3]:dim(mydata)[1], ] #2000 after 
# mydata2001 = mydata[yrInd[4]:dim(mydata)[1], ] #2001 after 
# mydata2002 = mydata[yrInd[5]:dim(mydata)[1], ] #2002 after 
mydata2003 = mydata[yrInd[6]:dim(mydata)[1], ] #2003 after 
# mydata2004 = mydata[yrInd[7]:dim(mydata)[1], ] #2004 after 

##########################
# Exploratory visualizations
##########################
# scatter matrix and correlation matrix 
# corr with Y & multicollinearity between X detected 
# ggpairs(mydataTrim)
# plot(mydataTrim, pch = 20)

########################
# plot data aginst time (for 1st 300 obs)
########################
# we see clear autocorrelation when plotting 300 obs,
# it is not obvious in the long run... might need formal test

# par(mfrow=c(3,3)) 
# for (i in 1:length(mydataTrim)){
#     plot(1:dim(mydataTrim[i])[1], mydataTrim[,i], pch = 20)  
#     title(colnames(mydataTrim)[i])
# }
# mtext("Plot all predictors against time", side = 3, line = -1.5, outer = TRUE)
# # 
# # # box plot, check outliers
# par(mfrow=c(3,3)) 
# for (i in 1:length(mydataTrim)){
#     boxplot(mydataTrim[,i])
#     title(colnames(mydataTrim)[i])
# }
# mtext("Box Plot for all predictors", side = 3, line = -1.5, outer = TRUE)




#################
# Fitting models 
#################

# fitting naive models
lm.fit_full = lm(pm10 ~ ws+ wd + nox +no2 +o3 +so2 +co, data = mydata2003)
# lm.fit_all = lm(pm10 ~ ws* wd* nox *no2 *o3 *so2 *co, data = mydataUNS)
lm.fit_null = lm(pm10 ~ 1, data = mydata2003)

# stepwise procedure
step(lm.fit_null, scope=list(lowr=lm.fit_null, upper=lm.fit_full), direction="both")

# all regression
out = All_reg(pm10 ~ ws +wd +no2 +nox +o3 +so2 +co, data = mydataUNS, nbest=4, nvmax=6)


# "best"
lm.fit_best = lm(formula = pm10 ~ no2 + so2 + wd + o3 + ws + co, data = mydata2003)

# remove outlier from 2003after data 
# (every execution the this line removes one obervation w/ the biggest residual)
# Namely, you need to run this line TWICE to remove 2 outlier points! 
# And of course, you need to refit the model after outlier removal
mydata2003 = mydata2003[- which(lm.fit_best$residuals ==  max(lm.fit_best$residuals)), ]



############################
# visualize the model fit 
############################
par(mfrow = c(2,2))
lm.fit_temp = lm.fit_best
plot(lm.fit_temp)
vif(lm.fit_temp)
colldiag(lm.fit_temp)



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

# residuals 
lm.fit_temp = lm.fit_best
dat = mydata2003


par(mfrow=c(3,3)) 
plot(lm.fit_temp$residuals ~ lm.fit_temp$fitted.values, pch = 20, 
     main = 'Residuals against fitted values', xlab = 'fitted values', ylab = 'residuals')
abline(0,0)

plot(lm.fit_temp$residuals, pch = 20, 
     main = 'Residuals against time', xlab = 'time', ylab = 'residuals')
abline(0,0)

# non-constancy variance! 
plot(lm.fit_temp$residuals ~ dat$ws, pch = 20, 
     main = 'Residuals against wind speed', xlab = 'wind speed', ylab = 'residuals')
abline(0,0)

plot(lm.fit_temp$residuals ~ dat$wd, pch = 20, 
     main = 'Residuals against wind direction', xlab = 'wind direction', ylab = 'residuals')
abline(0,0)

plot(lm.fit_temp$residuals ~ dat$nox, pch = 20, 
     main = 'Residuals against nox', xlab = 'nox', ylab = 'residuals')
abline(0,0)

plot(lm.fit_temp$residuals ~ dat$no2, pch = 20, 
     main = 'Residuals against no2', xlab = 'no2', ylab = 'residuals')
abline(0,0)

# non-constancy variance
plot(lm.fit_temp$residuals ~ dat$o3, pch = 20, 
     main = 'Residuals against o3', xlab = 'o3', ylab = 'residuals')
abline(0,0)

plot(lm.fit_temp$residuals ~ dat$so2, pch = 20, 
     main = 'Residuals against so2', xlab = 'so2', ylab = 'residuals')
abline(0,0)

# non-contancy variance
plot(lm.fit_temp$residuals ~ dat$co, pch = 20, 
     main = 'Residuals against co', xlab = 'co', ylab = 'residuals')
abline(0,0)


####### TODO ##############
## check multicolinearity
## variable selections
## residual analysis
## standardization
## regularization 
########################### 