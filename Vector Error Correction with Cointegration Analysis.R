#In this file we will apply VECM to find relationships in Pakistan Industrial Production
#inflation and M2


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

#Before Building VAR we need to check whether  series have cointegration if yes
#then we will go to VECM else we will difference the series and build VAR

#Lets apply Johansen's method(TRACE) for testing whether or not cointegration exists.
#H0:  one cointegration relation present
ctest1t <- ca.jo(spec_1_data, type = "trace", ecdet = "const", K = 11)
summary(ctest1t)
#Interpretation: We see that at r=0 for 5% null for no cintegration is rejected since
#70.69 > 34.91 which falls in rejection region.So we move to r=1 here we sees
#that we again reject null.Now this means we have atleast 1 cointegration relationship so we move to r=2 and here we see
#test value of 7.22 which is < 9.24 so we do not reject null  hypothesis at 5%. We conclude that atmost 1 conintegration relation exists


#Lets check now with Maximum Eigen Approach of Johansen
#Johansen Testing (MaxEigen)
ctest1t <- ca.jo(spec_1_data, type = "eigen", ecdet = "const", K = 11)
summary(ctest1t)
#We get same results from MaxEigen test also so we confirm that there are atleast 1
#cointegration relationship

#----------------------------------------------------------------------------------------------#

#After seeing the cointegration analysis we should now move to Vector Error Correction Model

#Build the VECM Model

Model1 <- VECM(spec_1_data, 11, r = 1, estim =("2OLS"))
summary(Model1)
#Interpretation for CPI: We see that there is delayed relationship b/w CPI today and that were previous.The ECT is not in the given range.It must be btw -1 and 0.Otherwise the error correction term is explosive.This may indicate CPI will not converge and will keep on increasing  
#Interpretation for logm2: We see that M2 is not very significant with its lags except 6 months before lag.ECT gives expected sign!
#Interpretation for ipilsm: We see that Error Correction term is negative which tells us that there is convergence
#from short run to long and we have a causal relationship btw IPILSM with logM2 and CPI
#4.89% is the speed it will take to adjust from short run to long run or Previous years errors will be corrected in following year at an adjustment rate of 4.89%
#Furthermore,Industrial Production has long term relationship with CPI and M2.
#--------------------------------------------------------------------------------#
#Diagnostic Tests

#Need to Transform VECM to VAR

Model1VAR <- vec2var(ctest1t, r = 1)


#Diagonistic 1: Serial Correlation
#H0: No AutoCorrelation is present in residuals
Serialtest <- serial.test(Model1VAR, lags.pt = 12, type = "PT.asymptotic")
Serialtest
#We  reject the null hypothesis and conclude that there is presence of serial auto correlation in this specification.
#Since Auto correlation is not a desirable trait so we may want to switch from VAR(12) to other models.

#Diagnoistic 2 : Hetrosckedacity : ARCH (autoregressive conditionally heteroscedastic) test
#To test for volatality
#H0 : There are no ARCH effects
Archtest<-arch.test(x = Model1VAR,multivariate.only = TRUE,lags.multi = 12)
Archtest
#Because the p-value is < 0.05, we reject the null hypothesis and conclude the presence of ARCH(12) effects.

#Diagnoistic 3: Normal Distribution of the residuals
#H0: Errors are normally distributed
Normtest<- normality.test(Model1VAR,multivariate.only = TRUE)
Normtest
#We reject H0 by JB-test and conclude that residual errors are not normally distributed.

#Impulse Response Functions
cpi_irf<-irf(x = Model1VAR,impulse = "cpi",n.ahead = 48,boot = TRUE)
plot(cpi_irf)
#We see that when giving shock to Industrial Production from CPI the industrial Production tends to go down in long run that is 4 years.How ever, the production tends to increase at last four months of every year.
#We also see that giving shock to itself CPI tends to increase in lon run
logm2_irf<-irf(x = Model1VAR,impulse = "logm2",n.ahead = 48,boot = TRUE)
plot(logm2_irf)
#we see that response on CPI is increasing and that is expected. Furthermore, giving shock in Industrial Production would increase production in short run but in long run when Error correction of ipilsm adjusts so it will decrease.Furthermore at every 12 month we do not have local minimum point.
ipilsm_irf<-irf(x = Model1VAR,impulse = "ipilsm",n.ahead = 48,boot = TRUE)
plot(ipilsm_irf)
#We see that increasing Industrial Production does cause Inflation in Long run.But we have cyclical graph of shock to itself which indicates presence of Seasonality in long run.

#Variance Decomposition
FEVD1 <- fevd(Model1VAR, n.ahead = 48)
plot(FEVD1)
#Both CPI and Money are influenced by their own shocks but cpi is increasing to be part of Industrial Production in Long run.

#-----------------------------------------------------------------------------------------------------------------------------------------#
#Forecasting 
forecast<- predict(Model1VAR,n.ahead = 12,ci = 0.95)
#Build the fan chart 
fanchart(forecast,names = "cpi")
fanchart(forecast,names = "logm2")
fanchart(forecast,names = "ipilsm")