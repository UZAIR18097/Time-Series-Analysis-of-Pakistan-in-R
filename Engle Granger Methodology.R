library(TSstudio)
library(dynlm)
library(zoo)
library(forecast)
library(strucchange)
library(tidyverse)
library(lubridate)
library(MLmetrics)
library(xts)
library(DataCombine)
library(egcm)
library(urca)
library(tseries)
library(mFilter)
library(ARDL)
library(PerformanceAnalytics)
library(tsDyn)
cat("\014")


set.seed(123)

#clear global environment
rm(list = ls())

#below function reads the data from csv file
#Input : String of file
#Output: dataframe
read_data <- function(data){
        data <- read.csv(data)
        data$M2 <- as.numeric(data$M2)
        return(data)
}

df<-read_data("Data.csv")

#convert all columns to monthly time series
cpi <- ts(df$CPI,frequency=12, start=c(1991,1))
ipilsm <- ts(df$IPILSM,frequency=12, start=c(1991,1))
tbr <- ts(df$TBR,frequency=12, start=c(1991,1))
m2 <- ts(df$M2,frequency=12, start=c(1991,1))
exports <- ts(df$Exports,frequency=12, start=c(1991,1))
imports <- ts(df$Imports,frequency=12, start=c(1991,1))


#we want to perform Engle Granger test of Conintegration on inflation and M2 as they seems
#to have same long run relationship

#first plot two series
#plotting inflation
ts_plot(cpi)
ts_plot(m2)
#looking at plots we see that intercept is 0 and trend is present so must include trend


#Engle Granger test of Conintegration
#Step1: Ensure that series are all non stationary and of same order
ndiffs(cpi)
ndiffs(m2)

#confirm this by Dickey Fuller  test 
#H0 : Series is not stationary
#H1: Series is stationary

test_cpi<- urca::ur.df(cpi, type = "trend", selectlags = c("AIC"))
test_cpi@teststat
test_cpi@cval
#The value of the test statistic is 6.108 > -2.87 (Critical value of the DF Test, model 2 at 5% significance). 
#Thus, we do not reject the null hypothesis and conclude that cpi series is non-stationary.

test_m2<- urca::ur.df(m2, type = "trend", selectlags = c("AIC"))
urca::summary(test_m2)
#The value of the test statistic is 3.3372 > -3.419 (Critical value of the ADF Test, model 2 at 5% significance). 
#Thus, we do not reject the null hypothesis and conclude that this m2 is non-stationary.


#Step2: Estimate long-run Integration regression on levels and store residuals

#generate trend variable
trend <- seq_along(cpi)

reg_longrun <- lm(cpi~m2+trend) #includes the intercept and trend
summary(reg_longrun)
#we see that trend is significant in our model

#store residuals of model
resid_reg_longrun <- reg_longrun$residuals

#Step3: Apply unit root test on residuals 

#H0 : the series are not cointegrated >> residuals are nonstationary
#H0 : the series are cointegrated >> residuals are stationary
y = ur.df(resid_reg_longrun,type = "none",selectlags = "AIC")
y@teststat
y@cval
#The value of tau1 statistic is -1.611 > -1.95 so we do not reject the null hypothesis
#at 5% significance level and conclude that series are not conintegrated.

#By conintegration Analysis we see that Consumer Price index and Money Supply are 
#not cointegrated which is against the theory because increasing the money supply should 
#have long term relation with cpi.


#We now detrend the series and apply the over all analysis again to see if they are cointegrated
#-----------------------------------------------------------------------------------------------#

#Detrending Time Series Processes

# detrending Using HP-Filter
lcpi <- log(df$CPI)
lm2 <- log(df$M2)

lcpi.hp <- hpfilter(lcpi, freq=14400, drift=FALSE)
lm2.hp <- hpfilter(lm2, freq=14400, drift=FALSE)

df$lcpi.cycle <- lcpi.hp$cycle
df$lm2.cycle <- lm2.hp$cycle

#take logs and store in data frame
df$logcpi<-log(df$CPI)
df$logm2 <-log(df$M2)

temp <- data.frame(lcpi.cycle=lcpi.hp$cycle,lm2.cycle=lm2.hp$cycle)
chart.Correlation(temp, histogram=TRUE, pch=19)
                   
#convert all columns to monthly time series
lcpi <- ts(df$logcpi,frequency=12, start=c(1991,1))
lm2 <- ts(df$logm2,frequency=12, start=c(1991,1))


#Engle Granger test of Conintegration
#Step1: Ensure that series are all non stationary and of same order
ndiffs(lcpi)
ndiffs(lm2)

#confirm this by Dickey Fuller  test 
#H0 : Series is not stationary
#H1: Series is stationary

test_cpi<- urca::ur.df(lcpi, type = "trend", selectlags = c("AIC"))
test_cpi@teststat
test_cpi@cval
#The value of the test statistic is -1.261 > -3.42 (Critical value of the DF Test, model 2 at 5% significance). 
#Thus, we do not reject the null hypothesis and conclude that cpi series is non-stationary.

test_m2<- urca::ur.df(lm2, type = "trend", selectlags = c("AIC"))
test_m2@teststat
test_m2@cval
#The value of the test statistic is -2.95 > -3.42 (Critical value of the ADF Test, model 2 at 5% significance). 
#Thus, we do not reject the null hypothesis and conclude that this m2 is non-stationary.


#Step2: Estimate long-run Integration regression on levels and store residuals

#generate trend variable
trend <- seq_along(cpi)

reg_longrun <- lm(lcpi~lm2+trend) #includes the intercept and trend
summary(reg_longrun)
#we see that trend is not significant in our model but it was significant 
#before taking log

#store residuals of model
resid_reg_longrun <- reg_longrun$residuals

#Step3: Apply unit root test on residuals 

#H0 : the series are not cointegrated >> residuals are nonstationary
#H0 : the series are cointegrated >> residuals are stationary
y = ur.df(resid_reg_longrun,type = "none",selectlags = "AIC")
y@teststat
y@cval
#The value of tau1 statistic is -1.54944 > -1.62 so we do not reject the null hypothesis
#at 5% significance level and conclude that log of cpi and log of lm2 are not conintegrated.
#This means that there is no long term relation btw log cpi and log lm2
#---------------------------------------------------------------------------#
#continue the analysis for exports and industrial Production
ts_plot(exports) #there seems to be present of stochastic trend in the series
ts_plot(ipilsm)
ts_decompose(ipilsm)
#looking at plots we see that there is seasonal trend present and with intercept


#Engle Granger test of Conintegration
#Step1: Ensure that series are all non stationary and of same order
ndiffs(exports)
ndiffs(ipilsm)

#we see that ipilsm and exports are I(1) process

#confirm this by Dickey Fuller  test 
#H0 : Series is not stationary
#H1: Series is stationary

test_exports<- urca::ur.df(exports, type = "trend", selectlags = c("AIC"))
test_cpi@teststat
test_cpi@cval
#The value of the test statistic is -3.919 > -2.98 (Critical value of the DF Test, model 2 at 5% significance). 
#Thus,do reject the null hypothesis and conclude that exports are not stationary

test_ipilsm<- urca::ur.df(ipilsm, type = "trend", selectlags = c("AIC"))
urca::summary(test_ipilsm)
#The value of the test statistic is -7.21< -3.419 (Critical value of the ADF Test, model 2 at 5% significance). 
#Thus, we reject the null hypothesis and conclude that this m2 is stationary.


#Step2: Estimate long-run Integration regression on levels and store residuals

#generate trend variable
trend <- seq_along(ipilsm)

reg_longrun <- lm(exports~ipilsm+trend) #includes the intercept and trend
summary(reg_longrun)
#we see that trend is significant in our model with ipilsm

#store residuals of model
resid_reg_longrun <- reg_longrun$residuals
plot(resid_reg_longrun)
#the plot indicates stationary so the residuals must be stationary

#Step3: Apply unit root test on residuals 

#H0 : the series are not cointegrated >> residuals are nonstationary
#H0 : the series are cointegrated >> residuals are stationary
y = ur.df(resid_reg_longrun,type = "trend",selectlags = "AIC")
y@teststat
y@cval
#The value of tau1 statistic is -4.60 < -3.98 so we reject the null hypothesis
#at 5% significance level and conclude that series are  conintegrated.


#By conintegration Analysis we see that exports and Industrial Production are 
#cointegrated which is favours theory and practice because increasing the industrial production should 
#have long term relation with exports.
#-------------------------------------------------------------------------------------------------------#


