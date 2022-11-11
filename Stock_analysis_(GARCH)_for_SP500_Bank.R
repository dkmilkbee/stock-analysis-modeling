# Project 2022/6/18

# GARCH modelling

# Load required libraries
if("quantmod" %in% rownames(installed.packages()) == FALSE) install.packages("quantmod")
if("PerformanceAnalytics" %in% rownames(installed.packages()) == FALSE) install.packages("PerformanceAnalytics")
if("rugarch" %in% rownames(installed.packages()) == FALSE) install.packages("rugarch")
library("quantmod")
library("PerformanceAnalytics")
library("rugarch")


# Get 10 year stock data from Yahoo 
code = "^SP500-40" # Bank sector data
stock <- getSymbols(code, auto.assign=FALSE, src="yahoo", from="2000-01-01", to="2022-06-23")

# Convert into Data Frame
data <- cbind(
  Price = Cl(stock),
  Return = CalculateReturns(Cl(stock), method = 'log'))  #Calculating Returns and transform into log values?
colnames(data) <- c('Price','Return')
head(data)

par(mfrow=c(1,1))
plot(na.omit(data$Price), ylab='Price',main='Closing Price',col='blue')
plot(na.omit(data$Return), main='Return')

# Test various things

# Check stationary
adf.test(na.omit(data$Price)) # p-value=0.15, price non-stationary
adf.test(na.omit(data$Return)) # p-value=0.01, return is stationary

# Check ACF
acf(na.omit(data$Return)) # Ok, lag are independent each other
pacf(na.omit(data$Return)) # Seems return is dependent on previous days

# Check normal dist or not
hist(data$Return) # From observation, seems not normal dist.
jarque.bera.test(na.omit(data$Return)) # Not normal dist.
skewness(data$Return, na.rm=T) # Negative skew
kurtosis(data$Return, na.rm=T) # Heavy tail

# Since the data is skew and heavy tail, use T-dist in predict model

# Box test
Box.test(na.omit(data$Return), type = "Ljung-Box") # Not independent

# Observe model parameters
acf(na.omit(data$Return), lag.max = 40)
acf(abs(na.omit(data$Return)), lag.max = 40)
pacf(abs(na.omit(data$Return)), lag.max = 40)
acf(na.omit(data$Return)^2, lag.max = 40)
pacf(na.omit(data$Return)^2, lag.max = 40)

# Use EGarch model to do the prediction

# Fit multiple p,q and get the suitable model
# Use p=0, q=0 as initial model
stock_garch <- ugarchspec(mean.model = list(armaOrder=c(2,2), include.mean=TRUE), variance.model = 
                            list(model = 'eGARCH', garchOrder = c(1, 1)), distribution = 'std')
fit_garch <- ugarchfit(spec = stock_garch, data= na.omit(data$Return), solver="solnp")
aic_garch <- infocriteria(fit_garch)[1] # Use AIC to prevent model overfitting

# Initialize base variables
bestfit <- list(aic=0, p=0, q=0, garch=0, fit=0)

# Method 1
# Loop different ARMA model to get best AIC. and prevent p=q=0
for (i in 0:5) {
  for (j in 0:5) {
    if (i==0 & j==0) {next}
    stock_garch <- ugarchspec(mean.model=list(armaOrder=c(i,j), include.mean=TRUE), 
                              variance.model=list(model='eGARCH', garchOrder=c(1, 1)), distribution='std')
    fit_garch <- ugarchfit(spec=stock_garch, data=na.omit(data$Return), solver="solnp")
    new_aic <- infocriteria(fit_garch)[1]
    if (new_aic < bestfit$aic) {
      bestfit <- list(aic=new_aic, p=i, q=j, garch=stock_garch, fit=fit_garch)
    }
  }
}
# Method 2
# Loop different ARMA model to get best AIC. and prevent p=q=0
for (i in 0:5) {
  for (j in 0:5) {
    if (i==0 & j==0) {next}
    stock_garch <- ugarchspec(mean.model=list(armaOrder=c(i,j), include.mean=TRUE), 
                              variance.model=list(model='eGARCH', garchOrder=c(1, 1)), distribution='std')
    fit_roll <- ugarchfit(spec=stock_garch, data=na.omit(data$Return), out.sample=500)
    new_aic <- infocriteria(fit_roll)[1]
    if (new_aic < bestfit$aic) {
      bestfit <- list(aic=new_aic, p=i, q=j, garch=stock_garch, fit=fit_roll)
    }
  }
}
# Checking final result
bestfit$aic; bestfit$p; bestfit$q

##############################################
# Debug use, obsolete if above for loop work correctly 
stock_garch_1 <- ugarchspec(mean.model = list(armaOrder=c(0,0)),variance.model = 
                            list(model = 'eGARCH', garchOrder = c(1, 1)),distribution = 'std')
fit_garch_1 <- ugarchfit(spec = stock_garch_1, data= na.omit(data$Return))

# Use p=1, q=1
stock_garch_2 <- ugarchspec(mean.model = list(armaOrder=c(0,1)),variance.model = 
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
stock_garch_t <- ugarchspec(mean.model = list(armaOrder=c(0,0)),variance.model = 
                              list(model = 'eGARCH', garchOrder = c(1, 1)),distribution = 'std')
fit_garch_t <- ugarchfit(spec = stock_garch_t, data= na.omit(data$Return))

# model 1 AIC: -6.2245  (*lowest AIC)
# model 2 AIC: -6.2243
# model 3 AIC: -6.2231
# model 4 AIC: -6.2236
# model 5 AIC: -6.2225
# Seems model 1 is the best fit model, use model 1 for prediction
##############################################

# Plot the best fit model
plot(bestfit$fit,which='all')
bestfit$fit

# Check convergency
print(convergence(bestfit$fit))

# Forecast using best fit
fc1 <-ugarchforecast(bestfit$fit, data=data, n.ahead=20)
fc1
par(mfrow=c(1,2))
plot(fc1, which=1)
plot(fc1, which=3)

# Volatility of the fit
par(mfrow=c(1,2))
fit_vol <- sigma(bestfit$fit)
fit_return <- fitted(bestfit$fit)
plot(fit_vol["2022"])
plot(fit_return["2022"])

# Simulation return/volatility/price
simgarchspec <- bestfit$garch
setfixed(simgarchspec) <- as.list(coef(bestfit$fit))
simgarch <- ugarchpath(spec=simgarchspec, m.sim=4,
                       n.sim=250, rseed=10)
simreturn <- fitted(simgarch)
plot.zoo(simreturn)
plot.zoo(sigma(simgarch))
simprice <- exp(apply(simreturn, 2, "cumsum"))
lastprice <- as.numeric(tail(data$Price,1))
mat <- simprice*lastprice
nn <- ncol(mat)
matplot(mat, type = "l", lwd = 3, xlab="Days", ylab="Price", main="Forecast Financial Stock Price")
legend("bottomright",legend=c("Series 1","Series 2","Series 3","Series 4"),col=seq_len(nn),cex=0.8,fill=seq_len(nn))

# Var at risk
garchroll <- ugarchroll(bestfit$garch, data = na.omit(data$Return), n.start = 2500, 
                        refit.window = "moving", refit.every = 100)
garchVaR <- quantile(garchroll, probs = 0.05)

actual <- xts(as.data.frame(garchroll)$Realized, time(garchVaR))
VaRplot(alpha = 0.05, actual = actual, VaR = garchVaR)
mean(actual < garchVaR)

# Rolling forecast
fit_roll <- ugarchfit(bestfit$garch, data=na.omit(data$Return), out.sample=500)
fc_roll <- ugarchforecast(fit_roll, n.ahead=20, n.roll=50)
fc_roll

par(mfrow=c(1,2))
plot(fc_roll,which=1)
plot(fc_roll,which=3)

par(mfrow=c(1,2))
plot(fc_roll,which=3)
plot(fc_roll,which=4)


# Forecasting using Bootstrap
par(mfrow=c(1,2))
fc_boot <- ugarchboot(bestfit$fit, data=na.omit(data$Return), method = c("Partial", "Full")[1], n.ahead=20, n.bootpred=500)
plot(fc_boot,which=2)
plot(fc_boot,which=3)

head(sigma(fc1))



