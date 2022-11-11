# Project 2022/6/18
# Reference: https://www.rpubs.com/AurelliaChristie/time-series-and-stock-analysis

# Load required libraries
# For get data from yahoo finance
if("quantmod" %in% rownames(installed.packages()) == FALSE) install.packages("quantmod")
library("quantmod")
# For transform to timeseries
if("tseries" %in% rownames(installed.packages()) == FALSE) install.packages("tseries")
library(tseries)
# For periodic analysis
if("TSA" %in% rownames(installed.packages()) == FALSE) install.packages("TSA")
library(TSA)
if("data.table" %in% rownames(installed.packages()) == FALSE) install.packages("data.table")
library(data.table)
# For ARIMA forecast
if("forecast" %in% rownames(installed.packages()) == FALSE) install.packages("forecast")
library(forecast)

# Get 10 year stock data from Yahoo 
code = "^SP500-40"
stock <- getSymbols(code, auto.assign=FALSE, src="yahoo", from="2010-01-01")
chartSeries(stock)

# Check the trend/seasonal/cycle charts
# stock <- na.omit(stock)
price <- Cl(to.monthly(stock))
price <- as.ts(price, start=c(2010,1))
plot(decompose(price))

############################################################
# Test various things 

# Check stationary
adf.test(na.omit(price))
# p-value = 0.02, cannot reject null hypothesis, price is non-stationary

# Test p,q,d value
acf(price)
acf(price, lag.max=30)
acf(diff(price)) # ok
acf(diff(price,differences = 2)) # Seems good!
acf(diff(price,differences = 3)) # Not good after 1.0
# From observation, lines up every 5 interval, take p=5 in AR

# Test q value
pacf(price) # ok
pacf(diff(price,differences=1))
# No diff already has a good result, so take q=0 in MA

# Test series has seasonal?
pg <- periodogram(na.omit(stock$`SP500-40.Adjusted`)) 
data.table(period=1/pg$freq, spec=pg$spec)[order(-spec)][1:5]

# In case the residual box cannot reject, do the fast fourier transform on the bottom 
# of this R script to find the seasonal factor.

# End testing
############################################################

# Prediction using time series
n <- 100 # predict days

# Split data into training and testing sets
train <- head(Cl(stock), length(Cl(stock))-n)
test <- tail(Cl(stock), n)

# Predict using native model
fc_na <- naive(train, h=n)

# Graph the prediction
autoplot(fc_na) +
  autolayer(ts(test, start=length(train)), series = "Test Data")

# Predict using non-seasonal ARIMA
# Create the Model
arima_ns <- auto.arima(train, seasonal=FALSE, test="adf", ic="aic")
Box.test(arima_ns$residuals)

# Forecast n periods of the data
fc_arima_ns <- forecast(arima_ns, h=n)

# Plot the result
autoplot(fc_arima_ns)+
  autolayer(ts(test, start= length(train)), series="Test Data")

# Predict using seasonal ARIMA
# Create the Model
arima_s <- auto.arima(train)
Box.test(arima_s$residuals)
# Forecast n periods of the data
fc_arima_s <- forecast(arima_s, h=n)

# Plot the result
autoplot(fc_arima_s)+
  autolayer(ts(test, start= length(train)), series="Test Data")

# Check residual
# Naive Method
checkresiduals(fc_na)

# ARIMA Model
checkresiduals(fc_arima_ns)

# Accuracy Metrics 
# Naive Method
accuracy(fc_na)

# ARIMA Model
accuracy(fc_arima_ns)

#############
# Fast Fourier Transform for getting seasonal data period
# Initialize
bestfit <- list(aic=arima_ns$aic, p=0, q=0, fit=arima_ns)

# Check if AIC after applying fast fourier transform is better or not
for (i in 1:3) {
  for (j in 1:3) {
    # Do transform
    z1 = fourier(ts(stock$`SP500-40.Adjusted`, frequency=800), K=i)
    z2 = fourier(ts(stock$`SP500-40.Adjusted`, frequency=640), K=j)
    # Rebuild the model
    fit = auto.arima(stock$`SP500-40.Adjusted`, xreg=cbind(z1,z2), seasonal=FALSE)
    if (fit$aic < bestfit$aic) {
      bestfit = list(aic=fit$aic, p=i, q=j, fit=fit)
    }
  }
}
############




