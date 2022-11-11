# Project 2022/6/18
# Reference: https://www.rpubs.com/AurelliaChristie/time-series-and-stock-analysis

# Load required libraries
if("quantmod" %in% rownames(installed.packages()) == FALSE) 
  install.packages("quantmod")
library("quantmod")

# Get 10 year stock data from Yahoo 
code = "AAPL"
stock <- getSymbols(code, auto.assign=FALSE, src="yahoo", from="2010-01-01")
chartSeries(stock)

# Check the trend/seasonal/cycle charts
# stock <- na.omit(stock)
price <- Cl(to.monthly(stock))
price <- as.ts(price, start=c(2010,1))
plot(decompose(price))

# Check stationary
library(tseries)
adf.test(na.omit(price))
# p-value = 0.8, cannot reject null hypothesis, price is non-stationary

# Test p,q,d value
acf(price)
acf(price, lag.max=30)
acf(diff(price))
acf(diff(price,differences = 2)) # Seems good!
acf(diff(price,differences = 3)) # Not good after 1.0
# From observation, lines up every 5 interval, take p=5 in AR

# Test q value
pacf(price)
pacf(diff(price,differences=2))
# No diff already has a good result, so take q=0 in MA

# Prediction using time series
n <- 100 # predict days

# Split data into training and testing sets
train <- head(Cl(stock), length(Cl(stock))-n)
test <- tail(Cl(stock), n)

# Predict using native model
library(forecast)
fc_na <- naive(train, h=n)

# Graph the prediction
autoplot(fc_na) +
  autolayer(ts(test, start=length(train)), series = "Test Data")

# Predict using non-seasonal ARIMA
# Create the Model
arima_ns <- auto.arima(train, seasonal=FALSE)
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





