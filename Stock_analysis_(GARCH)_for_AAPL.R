# Project 2022/6/18

# GARAH modelling

# Load required libraries
if("quantmod" %in% rownames(installed.packages()) == FALSE) 
  install.packages("quantmod")
library("quantmod")

# Get 10 year stock data from Yahoo 
code = "AAPL"
stock <- getSymbols(code, auto.assign=FALSE, src="yahoo", from="2010-01-01")

# Convert into Data Frame
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
data <- cbind(
  Price = stock[,4],
  Return = CalculateReturns(stock[,4], method = 'log'))  #Calculating Returns and transform into log values
colnames(data) <- c('Price','Return')
head(data)

plot(na.omit(data$Price), ylab='Price',main='Stock Closing Price',col='blue')
plot(na.omit(data$Return), main='Return of the stock')

# Check stationary
adf.test(na.omit(data$Price)) # p-value=0.8, price non-stationary
adf.test(na.omit(data$Return)) # p-value=0.01, return is stationary

# Check ACF
acf(na.omit(data$Return), lag.max = 30) # Ok, lag are independent each other
pacf(na.omit(data$Return), lag.max = 30) # Seems return is dependent on previous days

# Check normal dist or not
hist(data$Return) # Not normal dist
skewness(data$Return)
kurtosis(data$Return)

# Box test
Box.test(na.omit(data$Return), type = "Ljung-Box") # Not independent

acf(na.omit(data$Return), lag.max = 30)
acf(abs(na.omit(data$Return)), lag.max = 30)
pacf(abs(na.omit(data$Return)), lag.max = 30)
acf(na.omit(data$Return)^2, lag.max = 30)
pacf(na.omit(data$Return)^2, lag.max = 30)

# Use Garch model to predict
install.packages("rugarch")
library(rugarch)

# Fit multiple p,q and get the suitable model
# Use p=0, q=0
stock_garch_1 <- ugarchspec(mean.model = list(armaOrder=c(0,0)),variance.model = 
                            list(model = 'eGARCH', garchOrder = c(1, 1)),distribution = 'std')
fit_garch_1 <- ugarchfit(spec = stock_garch_1, data= na.omit(data$Return))

# Use p=1, q=1
stock_garch_2 <- ugarchspec(mean.model = list(armaOrder=c(1,1)),variance.model = 
                              list(model = 'eGARCH', garchOrder = c(1, 1)),distribution = 'std')
fit_garch_2 <- ugarchfit(spec = stock_garch_2, data= na.omit(data$Return))

# Use p=2, q=2
stock_garch_3 <- ugarchspec(mean.model = list(armaOrder=c(2,2)),variance.model = 
                              list(model = 'eGARCH', garchOrder = c(1, 1)),distribution = 'std')
fit_garch_3 <- ugarchfit(spec = stock_garch_3, data= na.omit(data$Return))

# Use p=3, q=1
stock_garch_4 <- ugarchspec(mean.model = list(armaOrder=c(3,1)),variance.model = 
                              list(model = 'eGARCH', garchOrder = c(1, 1)),distribution = 'std')
fit_garch_4 <- ugarchfit(spec = stock_garch_4, data= na.omit(data$Return))

# Use p=3, q=2
stock_garch_5 <- ugarchspec(mean.model = list(armaOrder=c(3,2)),variance.model = 
                              list(model = 'eGARCH', garchOrder = c(1, 1)),distribution = 'std')
fit_garch_5 <- ugarchfit(spec = stock_garch_5, data= na.omit(data$Return))

## Test use different p,q
stock_garch_t <- ugarchspec(mean.model = list(armaOrder=c(3,3)),variance.model = 
                              list(model = 'eGARCH', garchOrder = c(1, 1)),distribution = 'std')
fit_garch_t <- ugarchfit(spec = stock_garch_t, data= na.omit(data$Return))
fit_garch_t
# model 1 AIC: -5.4786
# model 2 AIC: -5.4783
# model 3 AIC: -5.4775
# model 4 AIC: -5.4772
# model 5 AIC: -5.4809 (*lowest AIC)
# Seems model 5 is the best fit model, use model 5 for prediction

plot(fit_garch_5,which='all')

print(convergence(fit_garch_5)) 

# Forecast
fc1 <-ugarchforecast(fit_garch_5, data=data, n.ahead=20)
fc1

# Rolling forecast
fit_roll <- ugarchfit(stock_garch_5, data=na.omit(data$Return), out.sample=500)
fc_roll <- ugarchforecast(fit_roll, n.ahead=20, n.roll=50)
fc_roll

par(mfrow=c(1,2))
plot(fc_roll,which=1)
plot(fc_roll,which=2)

par(mfrow=c(1,2))
plot(fc_roll,which=3)
plot(fc_roll,which=4)


# Forecasting using Bootstrap
par(mfrow=c(1,2))
fc_boot <- ugarchboot(fit_garch_5, data=na.omit(data$Return), method = c("Partial", "Full")[1], n.ahead=20, n.bootpred=500)
plot(fc_boot,which=2)
plot(fc_boot,which=3)

head(sigma(fc1))

