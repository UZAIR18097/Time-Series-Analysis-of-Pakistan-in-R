library(ggplot2)
library(TSstudio)


#clear global environment
rm(list = ls())

#below function reads the data from csv file and store it in time series object
read_data <- function(){
        data <- read.csv("AEdata.csv")
        return(data)
}

#below function converts the cpi into time series form
convert_cpi_to_ts <- function(){
        data <- read_data()
        cpi <- data$CPI
        cpi <- ts(cpi,frequency=12, start=c(1991,1))
        return(cpi)
}

#below function converts the m2 into time series form
convert_m2_to_ts <- function(){
        data <- read_data()
        m2 <- data$M2
        m2 <- ts(m2,frequency=12, start=c(1991,1))
        return(m2)
}

#below function plots the cpi series in Time Series Studio
plot_cpi<- function(){
        #get cpi
        cpi <- convert_cpi_to_ts()
        
        #plot cpi
        p <-    ts_plot(cpi,
                title = "Line graph of Consumer Price Index(January 1991- February 2021)",
                Xtitle = "Years",
                Ytitle = "CPI",
                line.mode = "lines",
                slider = TRUE,
                Xgrid = TRUE,
                Ygrid = TRUE)
        return(p)
}
#below function plots the m2 series in Time Series Studio
plot_m2<- function(){
        #get m2
        m2 <- convert_m2_to_ts()
        
        #plot m2
        p <-    ts_plot(cpi,
                        title = "Line graph of M2(January 1991- February 2021)",
                        Xtitle = "Years",
                        Ytitle = "M2",
                        line.mode = "lines",
                        slider = TRUE,
                        Xgrid = TRUE,
                        Ygrid = TRUE,
                        color = "Red")
        return(p)
}