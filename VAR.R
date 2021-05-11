#In this file we will apply VAR to find relationships in Pakistan Industrial Production
#,inflation and M2.All Var models will include 12 lags of each variable in the system


library(urca)
library(vars)
library(mFilter)
library(tseries)
library(forecast)
library(tidyverse)
library(tsDyn)
#for wide margins
par(mar=c(1,1,1,1))

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
logm2<- log(m2)
exports <- ts(df$Exports,frequency=12, start=c(1991,1))
imports <- ts(df$Imports,frequency=12, start=c(1991,1))

#simple multivariate graphs

ggplot(data = df)+
        geom_point(mapping = aes(x=CPI,y = IPILSM ))
#We see a positively correlated plot,Generally we see as CPI rise thus Industrial Production rises.


#plot as series
autoplot(cbind(ipilsm,cpi,logm2))

#Sepec 1 : First, we estimate reduced form VAR models and does not impose structure and let the data speak itself! 
#based on difference stationary and of IPI, CPI, and LM2.


#determine the persistence for CPI 
#Moving Average Component
acf(cpi,main = "ACF for Consumer Price Index")
#there is lot of persistence as all lags are relatively significant
#AR component
pacf(cpi,main = "PACF for Consumer Price Index")
#Shows that most of the lags are within the bound and insignificant

#determine the persistence for Industrial Production 
#Moving Average Component
acf(ipilsm,main = "ACF for Industrial Production")
#there is lot of persistence as all lags are relatively significant
#AR component
pacf(ipilsm,main = "PACF for Industrial Production")
#Shows that most of the lags are within the bound and insignificant

#determine the persistence for Money Supply 
#Moving Average Component
acf(m2,main = "ACF for M2")
#there is lot of persistence as all lags are relatively significant
#AR component
pacf(m2,main = "PACF for M2")
#Shows that most of the lags are within the bound and insignificant

#We could use ADF tests to find lags but we have done in prev assignment
#so we finding optimal lags using  VARselection



#Finding Optimal lags
#bind the needed data. Note that we use log of money supply so that results are interpretable
spec_1_data <- cbind(cpi,logm2,ipilsm)

lagselect<-VARselect(spec_1_data,lag.max = 12,type ="both") #we are using both trend and intercept
lagselect$selection
#We will use 12 as optimal lags so our model will be called as VAR(12)

#----------------------------------------------------------------------------------------------------------#

#Building VAR
Spec_1<- VAR(spec_1_data,p = 12,type="both")
summary(Spec_1)
#We have couple of lags significant lags at 0.001 in Industrial Production and significant 
#lags in CPI at 0.10 significance level.

#-----------------------------------------------------------------------------------#
#DIAGONISTICS
#We first check the residuals of each variable
plot(residuals(Spec_1)[,1],main="Residual of CPI")
plot(residuals(Spec_1)[,2],main="Residual of LogM2")
plot(residuals(Spec_1)[,3],main="Residual of Industrial Production")

#we move forward to check ACF of the residuals of variables
acf(residuals(Spec_1)[,1],main = "ACF for residuals of CPI")
acf(residuals(Spec_1)[,2],main = "ACF for residuals of LogM2")
acf(residuals(Spec_1)[,3],main = "ACF for residuals of Industrial Production")
#It looks good for a residual ACF. 
#(The big spike at the beginning is the unimportant lag 0 correlation.)

#We may also examine these plots in the cross-correlation matrix provided 
acf(residuals(Spec_1))
#The plots along the diagonal are the individual ACFs for each model's residuals that we plotted above. 
#In addition, we now see the cross-correlation plots of each set of residuals. Ideally, these would also resemble white noise,which is the case.

#Diagnostic 1 : Serial Correlation
#H0: No AutoCorrelation is present in residuals
Serial1<- serial.test(x = Spec_1,lags.pt = 12,lags.bg = 12)
Serial1$serial
#We  reject the null hypothesis and conclude that there is presence of serial auto correlation in this specification.
#Since Auto correlation is not a desirable trait so we may want to switch from VAR(12) to other models.

#Diagnoistic 2 : Hetrosckedacity : ARCH (autoregressive conditionally heteroscedastic) test
#To test for volatality
#H0 : There are no ARCH effects
Arch1<-arch.test(x = Spec_1,multivariate.only = TRUE,lags.multi = 12)
Arch1
#Because the p-value is < 0.05, we reject the null hypothesis and conclude the presence of ARCH(12) effects.

#Diagnoistic 3: Normal Distribution of the residuals
#H0: Errors are normally distributed
Norm1<- normality.test(Spec_1,multivariate.only = TRUE)
Norm1
#We reject H0 by JB-test and conclude that residual errors are not normally distributed.

#Diagnonistic4: Testing for Structural Breaks in residuals(Model Stability)
Stab1<- stability(Spec_1,type = "OLS-CUSUM")
plot(Stab1)
#There are no points along the graph that voilates Upper and Lower Confidence Interval thus Model is Stable.

#Granger Causality

Grangercpi <- causality(Spec_1,cause="cpi")
Grangercpi
# Reject H0 and conclude CPI granger causes M2 and Industrial Production

Grangerlogm2 <- causality(Spec_1,cause="logm2")
Grangerlogm2
# Reject H0 and conclude Money granger causes CPI and Industrial Production

Grangeripilsm <- causality(Spec_1,cause="ipilsm")
Grangeripilsm
# Reject H0 and conclude Industrial Production granger causes CPI and money Supply


#Forecasting 
forecast<- predict(Spec_1,n.ahead = 12,ci = 0.95)
#Build the fan chart 
fanchart(forecast,names = "cpi")
fanchart(forecast,names = "logm2")
fanchart(forecast,names = "ipilsm")