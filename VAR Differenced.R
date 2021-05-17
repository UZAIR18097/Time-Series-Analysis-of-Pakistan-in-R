#########################################################################################
#AUTHOR : MUHAMMAD UZAIR ASLAM(18097)
#TITLE  : VAR ANALYSIS ON PAKISTAN CPI , M2 and Industrial Production
#GROUP MEMBERS : MUSTAFA SHAMIM, SHAHAD MOORANI
#########################################################################################

#In this file we plan to explore short run effect by differenced CPI,M2 and Industrial Production

library(urca)
library(vars)
library(mFilter)
library(tseries)
library(forecast)
library(tidyverse)
library(tsDyn)
library(zoo)


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
m2 <- ts(df$M2,frequency=12, start=c(1991,1))

#plot as series
autoplot(cbind(log(ipilsm),log(cpi),log(m2)))

#From our unit root tests we have seen that Industrial Production is I(1) process, CPI is I(2) and Money Supply is I(2)
#We first difference the series to remove unit roots and then apply VAR framework

#we make a function which would take difference of the series two times
diff_2<- function(series)
{
        return(diff(diff(series)))
}

cpi_diff_2 <- diff_2(cpi)
m2_diff_2 <- diff_2(m2)
ipilsm_diff_1 <- diff(ipilsm)

#plot as series
autoplot(cpi_diff_2)
autoplot(m2_diff_2)
autoplot(ipilsm_diff_1)

#determine the persistence for CPI 
#Moving Average Component
acf(cpi_diff_2,main = "ACF for Consumer Price Index")
#AR component
pacf(cpi_diff_2,main = "PACF for Consumer Price Index")
#Shows that most of the lags are within the bound and insignificant

#determine the persistence for Industrial Production 
#Moving Average Component
acf(ipilsm_diff_1,main = "ACF for Industrial Production")
#AR component
pacf(ipilsm_diff_1,main = "PACF for Industrial Production")
#Shows that most of the lags are within the bound and insignificant

#determine the persistence for Money Supply 
#Moving Average Component
acf(m2_diff_2,main = "ACF for M2")

pacf(m2_diff_2,main = "PACF for M2")
#Shows that most of the lags are within the bound and insignificant

#first remove NA's
cpi_up<- cpi_diff_2[!is.na(cpi_diff_2)]
#cpi_2 <- ts(cpi_up,frequency=12, start=c(1991,3))

m2_up<- m2_diff_2[!is.na(m2_diff_2)]
#m2_2 <- ts(m2_up,frequency=12, start=c(1991,3))

ipilsm_up<- ipilsm_diff_1[!is.na(ipilsm_diff_1)]
#ipilsm_1 <- ts(ipilsm_up,frequency=12, start=c(1991,3))

#bind the needed data.
spec_1_data <- cbind(cpi_up,m2_up,ipilsm_up)

#we find optimal lags using  VARselection
#Finding Optimal lags
lagselect<-VARselect(spec_1_data,lag.max = 12,type ="both") #we are using both trend and intercept
lagselect$selection
#We will use 12 as optimal lags so our model will be called as VAR(12)

#----------------------------------------------------------------------------------------------------------#

#Sepec 1 : First, we estimate reduced form VAR models and does not impose structure and let the data speak itself! 
#based on difference stationary of IPI, CPI, and LM2.

#Building VAR
Spec_1<- VAR(spec_1_data,p = 12)
#Show summary
summary(Spec_1)

#Coefficient matrices of the lagged endogenous variables
Acoef(Spec_1)

#Coefficient matrix of an estimated VAR(12)
Bcoef(Spec_1)

#Coefficient method for objects of class varest
coef(Spec_1)


#-----------------------------------------------------------------------------------#
#DIAGONISTICS
#We first check the residuals of each variable
plot(residuals(Spec_1)[,1],main="Residual of CPI")
plot(residuals(Spec_1)[,2],main="Residual of M2")
plot(residuals(Spec_1)[,3],main="Residual of Industrial Production")

#we move forward to check ACF of the residuals of variables
acf(residuals(Spec_1)[,1],main = "ACF for residuals of CPI")
acf(residuals(Spec_1)[,2],main = "ACF for residuals of M2")
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

################################################################################################################

#Granger Causality
Grangercpi <- causality(Spec_1,cause="cpi_up")
Grangercpi
# Reject H0 and conclude CPI granger causes M2 and Industrial Production

Grangerm2 <- causality(Spec_1,cause="m2_up")
Grangerm2
# Reject H0 and conclude Money granger causes CPI and Industrial Production

Grangeripilsm <- causality(Spec_1,cause="ipilsm_up")
Grangeripilsm
# Reject H0 and conclude Industrial Production granger causes CPI and money Supply

#Variance Decomposition
FEVD1 <- fevd(Spec_1, n.ahead = 24)
plot(FEVD1)

###################################################################################

#Forecasting 
forecast<- predict(Spec_1,n.ahead = 24,ci = 0.95)
#Build the fan chart 
fanchart(forecast,names = "cpi_up",colors = "red")
fanchart(forecast,names = "m2_up",colors = "blue")
fanchart(forecast,names = "ipilsm_up",colors = "orange")
####################################################################################
#FIN