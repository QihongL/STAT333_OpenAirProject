# Some preliminary analysis for potential data for 333 prject 
setwd('/Users/Qihong/Code/github/STAT333_Project')
rm(list = ls())
library(GGally)
library(car)
library(plyr)
mydata = read.csv('data/OpenAir_example_data_long.csv')
dim(mydata)

# eliminate rows with missing values (NA)
mydata = mydata[complete.cases(mydata), ]
# switch the response variables to the end of the dataframe
mydata[length(mydata) + 1] = mydata[7]
mydata[7] = NULL
colnames(mydata)[10] = 'pm10'

# temp: trim the dimentionality of the input space (to plot data)
mydataTrim = data.frame(mydata[1:300,2:10])

# glance at the data
head(mydata)
summary(mydata)
# ggpairs(mydataTrim)
plot(mydataTrim, pch = 16)

# fit all air quality measures (with all interaction)
lm.fit_full = lm (mydata$pm10 ~ mydata$nox + mydata$no2 + mydata$o3 + mydata$so2 + mydata$co)
summary(lm.fit_full)
anova(lm.fit_full)
Anova(lm.fit_full, type = 'III')
plot(lm.fit_full)


####### TODO ##############
## check auto-correlations
## check multicolinearity
## variable selections
## residual analysis
## standardization
## regularization 
###########################