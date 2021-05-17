library(dynlm)
library(zoo)
library(forecast)
library(strucchange)
library(tidyverse)
library(lubridate)
library(MLmetrics)
library(xts)
library(DataCombine)
library(TSstudio)
library(tseries)
cat("\014")


set.seed(123)

#clear global environment
rm(list = ls())

#below function reads the data from csv file
#Input : String of file
#Output: dataframe
read_data <- function(data){
  data <- read.csv(data)
  #make first column as date
  data[,1] <-as.Date(data[,1],format ="%m-%d-%y")
  data$M2 <- as.numeric(data$M2)
  return(data)
}

df<-read_data("Data.csv")

#convert cpi to inflation and make new dataframe storing in df
df <- change(data = df,Var = "CPI",NewVar = "Inflation",type = "percent")

#convert all columns to monthly time series
inflation <- ts(df$Inflation,frequency=12, start=c(1991,1))
ipilsm <- ts(df$IPILSM,frequency=12, start=c(1991,1))
tbr <- ts(df$TBR,frequency=12, start=c(1991,1))
m2 <- ts(df$M2,frequency=12, start=c(1991,1))
exports <- ts(df$Exports,frequency=12, start=c(1991,1))
imports <- ts(df$Imports,frequency=12, start=c(1991,1))

#split data for train and test and set the last 12 months as a testing partition 
split_inflation <- ts_split(ts.obj = inflation, sample.out = 12)
split_ipilsm <- ts_split(ts.obj = ipilsm, sample.out = 12)
split_tbr <- ts_split(ts.obj = tbr, sample.out = 12)
split_m2 <- ts_split(ts.obj = m2, sample.out = 12)
split_exports <- ts_split(ts.obj = exports, sample.out = 12)
split_imports <- ts_split(ts.obj = imports, sample.out = 12)


#fit auto.arima on each series training data
inflation_fit <- auto.arima(split_inflation[["train"]])
summary(inflation_fit)
#AR(2) p =2 with 12 seasons

ipilsm_fit <- auto.arima(split_ipilsm[["train"]])
summary(inflation_fit)
#ARMA(1,2) with drift and 12 seasons

tbr_fit <- auto.arima(split_tbr[["train"]])
summary(tbr_fit)
#ARIMA(0,1,0)

m2_fit <- auto.arima(split_m2[["train"]])
summary(m2_fit)
#ARIMA(2,1,2) with 12 seasons

exports_fit <- auto.arima(split_exports[["train"]])
summary(exports_fit)
#ARIMA(2,1,0) with 12 seasons

imports_fit <- auto.arima(split_imports[["train"]])
summary(imports_fit)
#ARIMA(0,1,1) with 12 seasons

#JB test for normality
jarque.bera.test(inflation_fit$residuals)
jarque.bera.test(ipilsm_fit$residuals)
jarque.bera.test(tbr_fit$residuals)
jarque.bera.test(m2_fit$residuals)
jarque.bera.test(exports_fit$residuals)
jarque.bera.test(imports_fit$residuals)
#Normality for all the residuals is rejected in models


#check residuals of all fitted models and conclude by Ljung-Box test
#H0 :	The model does not exhibit lack of fit.
#Ha:	The model exhibits lack of fit.
checkresiduals(inflation_fit) #The Box-Ljung test does not rejects the null hypothesis at 5%
checkresiduals(ipilsm_fit) #The Box-Ljung test rejects the null hypothesis
checkresiduals(tbr_fit) #The Box-Ljung test does not rejects the null hypothesis
checkresiduals(m2_fit) #The Box-Ljung test rejects the null hypothesis 
checkresiduals(exports_fit) #The Box-Ljung  test does not rejects the null hypothesis
checkresiduals(imports_fit) #The Box-Ljung test rejects the null hypothesis
#Conclusion: Inflation,TBR and Exports does not Exhibit lack of fit.

#generate predictions for next 12 months
inflation_predictions <- forecast(inflation_fit,h=24,fan = TRUE)
ipilsm_predictions <- forecast(ipilsm_fit,h= 24,fan = TRUE)
tbr_predictions <- forecast(tbr_fit,h=24,fan = TRUE)
m2_predictions <- forecast(m2_fit ,h=24,fan = TRUE)
exports_predictions <- forecast(exports_fit,h=24,fan = TRUE)
imports_predictions <- forecast(imports_fit,h=24,fan = TRUE)

#plot all the forecast with actual data with their fanplots
plot_forecast(inflation_predictions,color = "red")
plot_forecast(ipilsm_predictions,color = "orange")
plot_forecast(tbr_predictions,color = "green")
plot_forecast(m2_predictions,color = "cyan")
plot_forecast(exports_predictions,color = "purple")
plot_forecast(imports_predictions,color = "brown")

#plot model prediction with actual test data
inflation_fit %>%
  forecast(h=12) %>%
  autoplot() + autolayer(split_inflation[["test"]])

ipilsm_fit %>%
  forecast(h=12) %>%
  autoplot() + autolayer(split_ipilsm[["test"]])

tbr_fit %>%
  forecast(h=12) %>%
  autoplot() + autolayer(split_tbr[["test"]])

m2_fit %>%
  forecast(h=12) %>%
  autoplot() + autolayer(split_m2[["test"]])

exports_fit %>%
  forecast(h=12) %>%
  autoplot() + autolayer(split_exports[["test"]])

imports_fit %>%
  forecast(h=12) %>%
  autoplot() + autolayer(split_imports[["test"]])

