#########################################################################################
#AUTHORS : MUHAMMAD UZAIR ASLAM(18097) MUSTAFA QAMAR SHAMIM (17988), SAHAD MOORANI (18000)
#TITLE  : VAR ANALYSIS ON PAKISTAN CPI , M2 and Industrial Production
#########################################################################################



library(urca)
library(vars)
library(mFilter)
library(tseries)
library(forecast)
library(tidyverse)
library(tsDyn)


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
ipilsm <- ts(df$IPILSM,frequency=12, start=c(1991,1))
tbr <- ts(df$TBR,frequency=12, start=c(1991,1))
exports <- ts(df$Exports,frequency=12, start=c(1991,1))
imports <- ts(df$Imports,frequency=12, start=c(1991,1))

#simple multivariate graphs

#plot as series
autoplot(cbind(exports,imports))
autoplot(ipilsm, 
         annotations = c("+", "title:when:where"))

#The series looks cointegrated let us bind the needed data and apply Johansen Cointegration test
#since all the series are I(1) process.

spec_1_data <- cbind(ipilsm,exports,imports)

#Select optimum lags 
lagselect <- VARselect(spec_1_data,lag.max = 12,type = "trend")
lagselect$selection


#Lets apply Johansen's method(TRACE) for testing whether or not cointegration exists.
#H0: r =  0 , one cointegration relation present

ctest1t <- ca.jo(spec_1_data, type = "trace", ecdet = "const", K = 11,spec = "longrun",season = 12)
summary(ctest1t)

#Interpretation: We see that null for zero cointegration is rejected at 10% since
#32.71 > 32.00 which falls in rejection region.So we move to r=1 here we sees
#that we are unable to reject null.Now this means we have atleast 1 cointegration 
#We conclude that atmost 1 conintegration relation exists in the system.

#The matrix of eigenvectors, normalised with respect to the Industrial Production
ctest1t@V
#The variance/covarinace matrix of V.
ctest1t@DELTA
#potential conintegration relation
plot(ctest1t)


#Lets check now with Maximum Eigen Approach of Johansen
#Johansen Testing (MaxEigen)
ctest2t <- ca.jo(spec_1_data, type = "eigen", ecdet = "const", K = 11)
summary(ctest2t)
#We get more strong result that at 5% from MaxEigen test also so we confirm that there are at least 1
#cointegration relationship 

#----------------------------------------------------------------------------------------------#

#After seeing the cointegration analysis we should now move to Vector Error Correction Model

#Build the VECM Model

Model1 <- VECM(spec_1_data, 11, r = 1, estim =("2OLS"))
summary(Model1)

#Interpretation for IPILSM: IPILSM has all lags significant
#Interpretation for Exports: Exports have short run relationship with Imports
#Interpretation for Imports: Imports have consistent short run relationship with IPILSM
#We dont get the required range for ECT terms which should be btw -1 and 0.
#--------------------------------------------------------------------------------#
#Diagnostic Tests

#Need to Transform VECM to VAR

Model1VAR <- vec2var(ctest1t, r = 1)

Model1VAR$deterministic
Model1VAR$A
Model1VAR$K

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

#Impulse Response Functions(Cumulative)
exports_irf_c<-irf(x = Model1VAR,impulse ="exports",n.ahead = 48,boot = TRUE,ci=0.95,cumulative = TRUE)
plot(exports_irf_c)

exports_irf<-irf(x = Model1VAR,impulse ="exports",n.ahead = 48,boot = TRUE,ci=0.95,cumulative = FALSE)
plot(exports_irf)


#Interpretations: We see that giving shock to Exports first declines exports for first 2 months 
#and then it rises in next 4 months. Then exports increase till next 12 months where we get decline at year closing
#But at cumulative level exports rise steadily compared to imports
#For imports shock in exports bring mix trend as it volatility increases but after 10 months we see that imports go negative and picks up in next 12 months
#So we see a delayed effect in imports

#Impulse Response Functions(Cumulative)
imports_irf_c<-irf(x = Model1VAR,impulse = "imports",n.ahead = 48,boot = TRUE,ci=0.95,cumulative = TRUE)
plot(imports_irf_c)

imports_irf<-irf(x = Model1VAR,impulse = "imports",n.ahead = 48,boot = TRUE,ci=0.95,cumulative = FALSE)
plot(imports_irf)
#Interpretations:We see that giving shock to imports make the exports negatively in long run.Cumulatively the exports decline
#Also imports itself increase for first 3 years with decreasing rate and then goes negative. But Cumulatively Imports Rise in long run

ipilsm_irf_c<-irf(x = Model1VAR,impulse = "ipilsm",n.ahead = 48,boot = TRUE,ci=0.95,cumulative = TRUE)
plot(ipilsm_irf_c)

ipilsm_irf<-irf(x = Model1VAR,impulse = "ipilsm",n.ahead = 48,boot = TRUE,ci=0.95,cumulative = FALSE)
plot(ipilsm_irf)
#We see that giving shock to industrial production brings more good to imports compared to exports as Imports rise faster compared to Exports
#Both Imports and Exports remain positive through out

#Variance Decomposition
FEVD1 <- fevd(Model1VAR, n.ahead = 48)
plot(FEVD1)
#Plot 1:
#For Industrial Production much of the variance is explained by Imports in future.
#This is evident because much of the raw material used are from imports so around 20% of variance will account for that.
#Fot exports it remain almost around 2-5% which means exports will play smaller role in future Industrial Production

#Plot 2:
#For exports we see an expected pattern as around 30% of the variance is expalined by Industrial Production
#This is natural because in Industrial Production Exports will not played a vital role but for exports Industrial Production is significant 
#particularly in Paksitan where much of the goods are primary/secondary goods
#Imports play a delayed role in Exports. This may be because Imported Raw material is purchased earlier in months and is processed by Industry and then Exported so it is intutive.

#Plot 3:
#For Imports we see that around 20% is explained in first year only which almost doubles at end of fourth year
#Interstingly,in short run more variance will be explained (around 15% )is explained by exports which gradually declines in long run as Industrial Production rises.
#This phenoma for Imports is intutive as in short run much of the raw material is imported for exports but as industry adjusts so imports are more derived by Production itself rather than exports.


#-----------------------------------------------------------------------------------------------------------------------------------------#
#Forecasting 
forecast<- predict(Model1VAR,n.ahead = 48,ci = 0.95)

#Build the fan chart 
fanchart(forecast,names = "exports",colors = "red")
fanchart(forecast,names = "imports",colors = "blue")
fanchart(forecast,names = "ipilsm",colors = "orange")
#Interpretation: 
#Exports are expected to follow seasonal trend with overall decline in next 4 years.
#For Imports we see similar pattern but decline in imports is much faster than exports
#Industrial Production will continue to bring seasonal trend in Pakistan with steady decrease in output.
