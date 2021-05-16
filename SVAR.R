#########################################################################################
#AUTHOR : MUHAMMAD UZAIR ASLAM(18097)
#TITLE  : Structural VAR ANALYSIS ON PAKISTAN CPI , M2 and Industrial Production
#GROUP MEMBERS : MUSTAFA SHAMIM, SHAHAD MOORANI
#########################################################################################

#In this file we plan to explore short run effects by detrended CPI,M2 and Industrial Production
#and try to replicate Mahmood(Volume 4, Number 1, November, 2008).


library(urca)
library(vars)
library(mFilter)
library(tseries)
library(forecast)
library(tidyverse)
library(tsDyn)
library(zoo)
library(tsbox)
library(gridExtra)
library(grid)


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
m2 <- ts(df$M2,frequency=12, start=c(1991,1))
ipilsm <- ts(df$IPILSM,frequency=12, start=c(1991,1))
cpi <- ts(df$CPI,frequency=12, start=c(1991,1))

#SVAR Restriction
amat <- diag(3)
amat
amat[2,1] <- NA
amat[3,1] <- NA
amat[3,2] <- NA
amat    
#Lower triangle is to be estimated
#m2 can effect industrial production and cpi
#industrial production can effect cpi but not M2
#cpi can only effect itself.
#For this strucutre we want to see role of M2 and completely shut off CPI

#Group variables
sv <- cbind(m2,ipilsm,cpi)
colnames(sv)<- cbind("M2","Industrial Production","CPI")

#Lag order Selection
lagselect<-VARselect(sv,lag.max = 12,type ="both") #we are using both trend and intercept
lagselect$selection

#Building VAR
Model1 <- VAR(sv,p = 12,type = "const", season = 12)
#Convert to Strucure VAR
SVARmodel <- SVAR(Model1,Amat = amat)


#Impulse Response Functions
m2_m2<-irf(x = SVARmodel,impulse = "M2",response = "M2",n.ahead = 48,boot = TRUE,ci=0.95,cumulative = FALSE)
plot(m2_m2)

m2_cpi<-irf(x = SVARmodel,impulse = "M2",response = "CPI",n.ahead = 48,boot = TRUE,ci=0.95,cumulative = FALSE)
plot(m2_cpi)

m2_ipilsm<-irf(x = SVARmodel,impulse = "M2",response = "Industrial.Production",n.ahead = 48,boot = TRUE,ci=0.95,cumulative = FALSE)
plot(m2_ipilsm)


cpi_m2<-irf(x = SVARmodel,impulse = "CPI",response = "M2",n.ahead = 48,boot = TRUE,ci=0.95,cumulative = FALSE)
plot(cpi_m2)

#Interpretations of Plots with Theory : Monetary Shock will lead to decline money Supply in Short run but within a year Money Supplied is increased by State Bank
#This forces the State bank to increase it's policy rate to control Inflation but the effect is delayed as CPI increases in Short run and then slowly it is capped by Statebank by Policy Rate
#Furthermore, price shock leads to decrease in Money Supply in short run as Statebank perform Open Market Operations to reduce money Supply but after an year Money Supply tends to converge.
#Rise in CPI following decline in Money Supply means Industrial Production will decline because cost of borrowing is high and so is inflation, but as CPI normalizes so Production gets better after  second year.