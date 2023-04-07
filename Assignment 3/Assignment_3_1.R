library("forecast")
library(car)

par(mfrow=c(1,1))

A3Data <- read.csv("A3Data.csv", head=T)
A3Data <- A3Data[1:122,]

matplot(A3Data[,2:8], type="l")
legend("topleft", colnames(A3Data[1:122,2:8]),col=seq_len(8),cex=0.8,lty=seq_len(8))

HousePrices <- data.frame(A3Data[1:122,2:6])

diffedHousePrices <- data.frame(apply(HousePrices, 2, diff))
logdiffedHousePrices <- data.frame(apply(log(HousePrices), 2, diff))
sqrtdiffedHousePrices <- data.frame(apply(sqrt(HousePrices), 2, diff))
matplot(diffedHousePrices$Denmark, type="l")
matplot(logdiffedHousePrices, type="l")


acf(logdiffedHousePrices, lag.max = 25)

pacf(logdiffedHousePrices, lag.max = 25)

# Using the differenced data, it seems there is a model structure

# Using the log
par(mfrow=c(2,1))
pacf(logdiffedHousePrices$Denmark)
acf(logdiffedHousePrices$Denmark)

# (2,1,0) x (0,0,4)_4
# (2,1,1) x (0,0,4)_4
# (2,1,1) x (1,0,4)_4 - probably not this

m1 <- arima(x = log(HousePrices$Denmark), order = c(2,1,0), seasonal = c(order=c(0,0,4), seasonal=4))
tsdiag(m1) ## Still a strong dependence in lag 1
pacf(residuals(m1)) # There are more significant lags here than in ACF so trying to add MA(1) part
qqPlot(residuals(m1))
m1 # The paramaters

plot(1:length(HousePrices$Denmark),residuals(m1))
sum(residuals(m1) < 0)/length(HousePrices$Denmark)*100
# They are white noise :-3


m2 <- arima(x = log(HousePrices$Denmark), order = c(2,1,1), seasonal = c(order=c(0,0,4), seasonal=4))
tsdiag(m2) ## Still a strong dependence in lag 1
pacf(residuals(m2)) # There are more significant lags here than in ACF so trying to add MA(1) part
qqPlot(residuals(m2))
m2 # The paramaters

plot(1:length(HousePrices$Denmark),residuals(m2))
sum(residuals(m2) < 0)/length(HousePrices$Denmark)*100
# They are white noise :-3

m3 <- arima(x = log(HousePrices$Denmark), order = c(2,1,1), seasonal = c(order=c(1,0,4), seasonal=4))
AIC(m1, m2, m3)
BIC(m1, m2, m3)
# And it seems that m1 - (2,1,0) x (0,0,4)_4 performs the best
summary(m1)

