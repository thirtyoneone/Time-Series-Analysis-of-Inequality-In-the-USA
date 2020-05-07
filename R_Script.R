#Dev Gupta
#2017B3A71082
#saving data as a time series
ratiots = ts(WID_Data_19042020_041554$`sptinc_z_US
Pre-tax national income 
Top 10% | share
USA`)

#EDA
ratiots
summary(ratiots)
plot(ratiots)

#Loading all required libraries
library(TTR)
library(forecast)
library(urca)
library(aTSA)

#Decomposition
#Since data isnt seasonal only SMA
ratiotsSMA3 = SMA(ratiots, n = 3)
plot(ratiotsSMA3)

ratiotsSMA4 = SMA(ratiots, n = 4)

#checking irregular compnent
plot(ratiots - ratiotsSMA4)

ratiotsSMA4

#stationarity test
stationary.test(ratiots, method = 'pp')
stationary.test(ratiots, method = 'kpss')

#using first difference series
ratiotsdiff1 = diff(ratiots, differences = 1)

stationary.test(ratiots, method = 'pp')
stationary.test(ratiots, method = 'kpss')

#ARIMA Model
acf(ratiotsdiff1, lag.max = 20)
pacf(ratiotsdiff1, lag.max = 20)

auto.arima(ratiots)

#forecast
ratiotsarima = arima(ratiots, order = c(0,1,0))
ratiotsarima

ratiotsforecast = forecast(ratiotsarima, h = 5)
plot(ratiotsforecast)

ratiotsforecast = forecast(ratiotsarima, h = 15)
plot(ratiotsforecast)

#Testing prediciton
#autocorelation of forecast errors
acf(ratiotsforecast$residuals,lag.max = 20)
Box.test(ratiotsforecast$residuals, lag = 20, type = 'Ljung-Box')

#test normality, 0 mean and const, sd
plot(ratiotsforecast$residuals)
hist(ratiotsforecast$residuals)
hist(ratiotsforecast$residuals, n = 10)