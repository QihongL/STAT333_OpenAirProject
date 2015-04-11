# Some preliminary analysis for potential data for 333 prject 
setwd('/Users/Qihong/Code/github/STAT333_Project')
rm(list = ls())
library(GGally)
library(car)
library(plyr)
library(leaps)
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

# fitting a nai
lm.fit_all = lm(mydata$pm10 ~ mydata$nox +mydata$no2 +mydata$o3 +mydata$so2 +mydata$co)

# plot(lm.fit_all)
# QQ plot indicating abnormality
# outliers removal needed

par(mfrow=c(3,3)) 
for (i in 1:length(mydataTrim)){
    plot(1:dim(mydataTrim[i])[1], mydataTrim[,i])  
    title(colnames(mydataTrim)[i])
}
mtext("Visualizeing everything against time", side = 3, line = -1.5, outer = TRUE)
# we see clear autocorrelation... 

# stepwise regression procedure
# lm.null = lm(mydata$pm10 ~ 1, data = mydataTrim)
# lm.full = lm(mydata$pm10 ~ ., data = mydataTrim)
# step(lm.null, scope = list(lowr = lm.null, upper = lm.full), direction = 'both')


####### TODO ##############
## check auto-correlations
## check multicolinearity
## variable selections
## residual analysis
## standardization
## regularization 
###########################