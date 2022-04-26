library(openxlsx)
library(forecast)
library(timeSeries)
library(ggpubr) # for ggarrange function - to arrange multiple ggplots

setwd("D:/")

#####
##Q1
#####

#a) 
SouvenirSales_df <- read.xlsx("./SouvenirSales.xlsx")
SouvenirSales_df
SouvenirSales_ts <- ts(SouvenirSales_df$Sales, start = c(1995,1), frequency = 12)
SouvenirSales_ts
autoplot(SouvenirSales_ts)

### Conclusion ###
# From the plot, we can see that the data has trend as well as Seasonality

#b) 
training <- window(SouvenirSales_ts,end=c(2000,12), frequency=12)
validation <- window(SouvenirSales_ts,start=c(2001,1), frequency=12)
complete_data <-  window(SouvenirSales_ts,end=c(2001,12), frequency=12)

SouvenirSales1 <- decompose(training, type= "additive")
g1 <- autoplot(SouvenirSales1 )
SouvenirSales2 <- decompose(training, type= "multiplicative")
g2 <- autoplot(SouvenirSales2 )
#??ggarrange
ggarrange(g1,g2)

## Model-A - Linear Trend with Additive Seasonality
linear.trend.add.season <- tslm(training ~ trend + season)
summary(linear.trend.add.season)
linear.trend.add.season.pred <- forecast(linear.trend.add.season, h=length(validation), level = 0) ## Forecasting on the validation test

plot(linear.trend.add.season.pred, xlab ="Time", ylab= "Sales", main="Linear Trend Model with Additive Seasonality")
lines(training)
lines(validation)
lines(linear.trend.add.season.pred$fitted,col="green",lwd=2, lty=1)
#lines(training)
legend(1995,80000, legend=c("Original", "Forecast"),  col=c("black", "blue"), lty=1:1, cex=0.35)


## Model-B - Exponential Trend with Multiplicative Seasonality
exp.trend.mul.season <- tslm(training ~ trend + season, lambda = 0)
summary(exp.trend.mul.season)
exp.trend.mul.season.pred <- forecast(exp.trend.mul.season, h=length(validation), level = 0) ## Forecasting on the validation test

plot(exp.trend.mul.season.pred, xlab ="Time", ylab= "Sales", main="Exponential Trend Model with Multiplicative Seasonality", ylim=c(0,80000))
#plot(exp.trend.mul.season.pred, xlab ="Time", ylab= "Sales", main="Exponential Trend Model with Multiplicative Seasonality")
lines(training)
lines(validation)
lines(exp.trend.mul.season.pred$fitted,col="green",lwd=2, lty=1)
legend(1995,80000, legend=c("Original", "Forecast"),  col=c("black", "blue"), lty=1:1, cex=0.35)



#c)
##########################
#RMSE.training = sqrt(mean(linear.trend.add.season$residuals^2))
#RMSE.training
##############################
RMSE.modelA.validation = sqrt(mean((validation-linear.trend.add.season.pred$mean)^2))
RMSE.modelA.validation
# RMSE for Model-A is 17451.55

RMSE.modelB.validation = sqrt(mean((validation-exp.trend.mul.season.pred$mean)^2))
RMSE.modelB.validation
# RMSE for Model-B is 7101.444

## Looking at the RMSE, we can say that Model-B is better than Model-A


#plot(training, xlab ="Time", ylab= "Sales", main="Souvenir Sales", bty="l", ylim=c(0,80000))
plot(complete_data, xlab ="Time", ylab= "Sales", main="Souvenir Sales", bty="l", ylim=c(0,80000))
#lines(validation)
#lines(exp.trend.mul.season.pred$fitted, col="blue", lwd=2)
lines(exp.trend.mul.season.pred$mean, col="blue", lwd=2)
#lines(linear.trend.add.season.pred$fitted, col="green", lwd=2)
lines(linear.trend.add.season.pred$mean, col="green", lwd=2)
legend(1995,80000, legend=c("Linear", "Exponential"),  col=c("green", "blue"), lty=1:1, cex=0.35)

## From the above plot, we can clearly see that Model-B (exp) is fitting the data
## more accurately than Model-A (linear)

## Let's plot residuals:
plot(training-exp.trend.mul.season$fitted.values, xlim = c(1995,2002), ylim= c(-30000,30000),main= "Residual Plot", ylab= "Residuals", col="blue",lwd=2)
lines(validation-exp.trend.mul.season.pred$mean, col="blue",lwd=2)
lines(linear.trend.add.season.pred$residuals,col="green",lwd=2)
lines(validation-linear.trend.add.season.pred$mean, col="green",lwd=2)
legend(1998,30000, legend=c("Linear", "Exponential"),  col=c("green", "blue"), lty=1:1, cex=0.25)

#d)
## From the additive model (Model-A) it is clearly visible that December month has 
## the highest average sales during the year

## The estimated trend coefficient from Model-A is 245.36 which means
## the average linear increase for each month over the entire period is $245.36 

#e)

## Coefficient of Oct is 0.729490 which means it is 27.06% lower than Jan month
## The estimated trend coefficient of Model-B is 0.021120. 
## That means the model has an overall increase of approx. 2.11% in sales 
#  each month over the previous month.


#f)
#As from c) we know that Model-B is better than Model-A, let's first train
# Model-B with complete data before doing any forecasting for Jan-2002


exp.trend.mul.season.complete <- tslm(SouvenirSales_ts ~ trend + season, lambda = 0)
summary(exp.trend.mul.season.complete)
exp.trend.mul.season.complete.pred <- forecast(exp.trend.mul.season.complete, h=1, level = 0) ## Forecasting for Jan-2002
exp.trend.mul.season.complete.pred
## Point forecast using Model-B for Jan 2002, considering Trend and Seasonality = 13484.06
#exp.trend.mul.season.complete.pred$fitted
plot(exp.trend.mul.season.complete.pred, xlab ="Time", ylab= "Sales", main="Exponential Trend Model with Multiplicative Seasonality", ylim=c(0,80000))
lines(complete_data)
lines(exp.trend.mul.season.complete.pred$fitted,col="green",lwd=2, lty=1)



#g)

#plot(training-exp.trend.mul.season$fitted.values, xlim = c(1995,2002), ylim= c(-10000,10000),main= "Residual Plot", ylab= "Residuals", col="blue",lwd=2)
#Acf(training-exp.trend.mul.season$fitted.values,lag.max = 20)
#Pacf(training-exp.trend.mul.season$fitted.values,lag.max = 20)

modelB.training.residuals=exp.trend.mul.season$residuals
Acf(modelB.training.residuals ,lag.max = 20)
Pacf(modelB.training.residuals ,lag.max = 20)

## From the PACF plot, we can see that first 2 vertical lines are crossing 
## line of significance so this is clearly AR(2) model.


#h)
#modelB.training.residuals.arima <- Arima(modelB.training.residuals, order = c(4,0,0))
#summary(modelB.training.residuals.arima)
summary(Arima(modelB.training.residuals, order = c(1,0,0)))
summary(Arima(modelB.training.residuals, order = c(2,0,0)))
summary(Arima(modelB.training.residuals, order = c(3,0,0)))
summary(Arima(modelB.training.residuals, order = c(4,0,0)))

# AICc value for AR(1) is coming as -61.41
# AICc value for AR(2) is coming as -69.46
# AICc value for AR(3) is coming as -68.09
# AICc value for AR(4) is coming as -67.39


## Looking at the above AICc values, we can conclude that our understanding 
## from PACF plot is correct and AR(2) model fits the data very well as AR(1), AR(3)
## and AR(4) model have higher AICc value than AR(2) model.


#i)
## Forecast for Trend and Seasonality on complete data using Model-B

exp.trend.mul.season.complete.residuals <- exp.trend.mul.season.complete$residuals
exp.trend.mul.season.complete.residuals.arima <- Arima(exp.trend.mul.season.complete.residuals, order = c(2,0,0))
predict_exp.trend.mul.season.complete.residuals.arima.pred <- predict(exp.trend.mul.season.complete.residuals.arima,n.ahead=1)
predict_exp.trend.mul.season.complete.residuals.arima.pred
#predict_exp.trend.mul.season.complete.residuals.arima.pred$pred[1]
#2nd method
predict_exp.trend.mul.season.complete.residuals.arima.forecast <- forecast(exp.trend.mul.season.complete.residuals.arima,h=1)
predict_exp.trend.mul.season.complete.residuals.arima.forecast$mean

#0.06501293
## Prediction on residuals using ARIMA model = 0.06501293

#predict_modelB.training.residuals.arima <- predict(modelB.training.residuals.arima)
#predict_modelB.training.residuals.arima$pred[1]


## Final prediction for Jan 2002 = Prediction for Trend and Seasonality for Jan 2002 + Prediction for Residuals for Jan 2002
## = 13484.06 (from f) + 0.06501293
## 13484.06 + 0.06501293
## = 13484.13

#####
##Q2
#####

#a)
# Time series focuses on the same variable over a period of time 
# whereas the cross sectional data focuses on several variables at the 
# same point of time. Moreover, the time series data consist of observations 
# of a single subject at multiple time intervals whereas, the cross sectional 
# data consist of observations of many subjects at the same point in time.

# b) 
# Seasonal effects are observed within one calendar year e.g. sales 
# increase during Diwali or Christmas whereas 
# cyclical effects can span over a period shorter or longer than a 
# calendar year e.g. pandemic disease, stock market collapse etc.
# Seasonality has symmetric ups and downs whereas cyclicality has non-symmetric
# ups and downs.

# c) 
# Centered Moving Average is not considered suitable for forecasting because
# it doesn't take few of the latest values (most recent past) into consideration
# while forecasting which ideally should be given the highest weightage.

#d) 
# When the mean, variance and auto-correlation structure of a time series 
# do not change over time, it is said to be stationary. 
# Stationarity is very important because in the absence of it, 
# a model describing the data will have different accuracy at different time points. 

#e) 
# A time series is said to be stationary when it doesn't not have any trend and seasonality.
# Or, if a time series has white noise then it is said to be stationary
# The ACF will drop to zero relatively quickly for a stationary time series, 
# while the ACF of non-stationary time series data decreases slowly.


#f)
# For a time series data, a better approach is to partition the data into 
# training and validation only. Firstly, we train different models with training
# data, secondly we run those models on validation data and pick up the model
# with the best accuracy. Thirdly, we train that best model on complete dataset 
# i.e train + validation before doing any forecasting.

# The validation period should be at least equal to the forecasting horizon because
# in case validation period is shorter than forecasting horizon, it will fail to mimic 
# the actual scenario, whereas a longer validation period will leave the training set
# without the latest (most recent) information. 


#g)
# It is true that both smoothing and ARIMA method of forecasting can handle time series 
# data with missing value. Missing values can be handled in few ways. Either, we could 
# just take the part of data after the last missing value, assuming there is a  
# enough series of observations to produce forecasts, or, we could replace the
# missing values with estimates.

#h) 
# False. Additive and Multiplicative decomposition differs in terms of 
# seasonality and not the trend

#i)
# Knowing that there is some correlation left in the data after accounting 
# for trend and seasonality is good news for Analyst as he/she can now 
# apply techniques like ARIMA on residuals and forecast for residuals
# separately which can then be added to forecast for data, capturing trend
# and seasonality, to make the final forecast.

