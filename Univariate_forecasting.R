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
ipilsm_fit <- auto.arima(split_ipilsm[["train"]])
tbr_fit <- auto.arima(split_tbr[["train"]])
m2_fit <- auto.arima(split_m2[["train"]])
exports_fit <- auto.arima(split_exports[["train"]])
imports_fit <- auto.arima(split_imports[["train"]])


#generate predictions for next 12 months
inflation_predictions <- forecast(inflation_fit,h=12,fan = TRUE)
ipilsm_predictions <- forecast(ipilsm_fit,h= 12,fan = TRUE)
tbr_predictions <- forecast(tbr_fit,h=12,fan = TRUE)
m2_predictions <- forecast(m2_fit ,h=12,fan = TRUE)
exports_predictions <- forecast(exports_fit,h=12,fan = TRUE)
imports_predictions <- forecast(imports_fit,h=12,fan = TRUE)

#plot all the forecast with actual data with their fanplots
plot_forecast(inflation_predictions)
plot_forecast(ipilsm_predictions)
plot_forecast(tbr_predictions)
plot_forecast(m2_predictions)
plot_forecast(exports_predictions)
plot_forecast(imports_predictions)

#check residuals of all fitted models and conclude by Ljung-Box test
checkresiduals(inflation_fit)
#The first and the last graph shows us that the residuals appear to be white noise (variables are independent and identically distributed with a mean of zero) and the second graph confirms it.
#There is a lag which goes above the 10% threshold, however, the other ones die out.
checkresiduals(ipilsm_fit)
#It seems that the model left some information in the residuals. The first and the last graph shows us that the residuals do not appear to be white noise but to have some correlation amongst them and the second graph confirms it. 
#Few of these lags go above the 10% threshold and they do not seem to die out with time.
checkresiduals(tbr_fit)
#It seems that the model left some information in the residuals. The first and the last graph shows us that the residuals do not appear to be white noise but to have some correlation amongst them and the second graph confirms it. 
#Few of these lags go above the 10% threshold and they do not seem to die out with time.
checkresiduals(m2_fit)
#The initial values of the first and second graph shows us that the errors are independent of each other with a mean of zero. However, there seem to be some correlation between the error terms after the 12th lag
#The initial lags fall within the threshold, however, after 12 years many of these lags go above the 10% threshold and they do not seem to die out with time.
checkresiduals(exports_fit)
#The initial values of the first and second graph shows us that the errors are independent of each other with a mean of zero. However, there seem to be some correlation between the error terms after the 20th lag
#The initial lags fall within the threshold, however, after 20 years many of these lags go above the 10% threshold and they do not seem to die out with time.
checkresiduals(imports_fit)
#The first and the last graph shows us that the residuals appear to be white noise (variables are independent and identically distributed with a mean of zero) and the second graph confirms it.
#There are few lags which goes above the 10% threshold, however, the other ones die out with time.

#plot training with test data
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

