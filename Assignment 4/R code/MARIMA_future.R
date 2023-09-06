# Set working directory:
setwd('/Users/emilhaugstvedt/Desktop/DTU/TimeSeriesAnalysis/Time-Series-Analysis/Assignment4/R code')

# Load StepSlow function:
source("StepSlow.R")

# Load marima library:
library(marima)

# Load data:
data <- read.csv("../A4_data.csv")

# Add NaNs for prediction:
tempData <- ts(matrix(data = NaN, nrow = 7, ncol = 122), start = 1992, frequency = 4)
tempData[, 1:118] <- ts(t(data[, 2:8]), start = 1992, frequency = 4)

data <- tempData

# Log transform the data:
log.data <- data
log.data[1:4, ] <- log(data[1:4, ])

# Difference the data:
difference <- c(1, 1, 2, 1, 3, 1, 4, 1, 5, 4)
dlog.data <- define.dif(series = log.data[, 1:118], difference = difference)$y.dif
dlog.dif.poly <- define.dif(series = log.data[, 1:118], difference = difference)$dif.poly

##### MARIMA #####

# Create the model:
model <- define.model(kvar=7, ar= (1), ma= (1), reg.var= (5:7))

# Fit the model:
Marima <- marima(DATA = dlog.data,
                 ar.pattern = model$ar.pattern,
                 ma.pattern = model$ma.pattern,
                 Plot="log.det",
                 Check=FALSE)

# Use the step slow function to make better predictions
Marima_new <- step.slow(Marima, dlog.data, penalty=3.92)

# The estimated parameters for the ar and ma part of the model:
short.form(Marima_new$ar.estimates, leading = FALSE)
short.form(Marima_new$ma.estimates, leading = FALSE)


# Do the forecast:
forecast <- arma.forecast(log.data,
                          marima= Marima_new,
                          nstart= 118,
                          nstep= 4,
                          dif.poly = dlog.dif.poly)

# Get the forecasts:
transformed_forecast <- forecast$forecasts

# Get the variance for each prediction to calculate prediction intervals:
pred.var <- matrix(data = NaN, nrow = 4, ncol = 4)
pred.var[1, ] <- diag(matrix(forecast$pred.var[1:4,1:4,1], nrow = 4, ncol = 4))
pred.var[2, ] <- diag(matrix(forecast$pred.var[1:4,1:4,2], nrow = 4, ncol = 4))
pred.var[3, ] <- diag(matrix(forecast$pred.var[1:4,1:4,3], nrow = 4, ncol = 4))
pred.var[4, ] <- diag(matrix(forecast$pred.var[1:4,1:4,4], nrow = 4, ncol = 4))

# Define matrices for storing prediction intervals:
pred_interval.z2000 <- matrix(data = NaN, nrow = 2, ncol = 4)
pred_interval.z2800 <- matrix(data = NaN, nrow = 2, ncol = 4)
pred_interval.z4000 <- matrix(data = NaN, nrow = 2, ncol = 4)
pred_interval.z4200 <- matrix(data = NaN, nrow = 2, ncol = 4)

# Calculate prediction interval for all predicted lags:
for (prediction in 1:4) {
  # Upper:
  pred_interval.z2000[1,prediction] <- exp(transformed_forecast[1,(118 + prediction)] +
                                             1.96 * sqrt(pred.var[prediction, 1]))
  pred_interval.z2800[1,prediction] <- exp(transformed_forecast[2,(118 + prediction)] +
                                             1.96 * sqrt(pred.var[prediction, 2]))
  pred_interval.z4000[1,prediction] <- exp(transformed_forecast[3,(118 + prediction)] +
                                             1.96 * sqrt(pred.var[prediction, 3]))
  pred_interval.z4200[1,prediction] <- exp(transformed_forecast[4,(118 + prediction)] +
                                             1.96 * sqrt(pred.var[prediction, 4]))

  # Lower:
  pred_interval.z2000[2, prediction] <- exp(transformed_forecast[1,(118 + prediction)] -
                                              1.96 * sqrt(pred.var[prediction, 1]))
  pred_interval.z2800[2, prediction] <- exp(transformed_forecast[2,(118 + prediction)] -
                                              1.96 * sqrt(pred.var[prediction, 2]))
  pred_interval.z4000[2, prediction] <- exp(transformed_forecast[3,(118 + prediction)] -
                                              1.96 * sqrt(pred.var[prediction, 3]))
  pred_interval.z4200[2, prediction] <- exp(transformed_forecast[4,(118 + prediction)] -
                                              1.96 * sqrt(pred.var[prediction, 4]))
}

untransformed_forecast <- exp(transformed_forecast)

### Prediction for whole data set ###

# Extract the predictions for the different zip codes:
predictZ2000 <- ts(untransformed_forecast[1, ], start = 1992, frequency = 4)
predictZ2800 <- ts(untransformed_forecast[2, ], start = 1992, frequency = 4)
predictZ4000 <- ts(untransformed_forecast[3, ], start = 1992, frequency = 4)
predictZ4200 <- ts(untransformed_forecast[4, ], start = 1992, frequency = 4)

# Plot prediction and true values:
par(mfrow=c(2,2))
plot(ts(data[1, 1:118], start = 1992, frequency = 4), type= "l",
     main= "True vs. Pred z2000", col= "orange",
     ylab= "Price")
lines(predictZ2000, type= "l", col= "Blue")
legend("topleft", c("True", "Prediction"), fill= c("orange", "blue"))

plot(ts(data[2, 1:118], start = 1992, frequency = 4), type= "l",
     main= "True vs. Pred z2000", col= "orange",
     ylab= "Price")
lines(predictZ2800, type= "l", col= "Blue")
legend("topleft", c("True", "Prediction"), fill= c("orange", "blue"))

plot(ts(data[3, 1:118], start = 1992, frequency = 4), type= "l",
     main= "True vs. Pred z2000", col= "orange",
     ylab= "Price")
lines(predictZ4000, type= "l", col= "Blue")
legend("topleft", c("True", "Prediction"), fill= c("orange", "blue"))

plot(ts(data[4, 1:118], start = 1992, frequency = 4), type= "l",
     main= "True vs. Pred z2000", col= "orange",
     ylab= "Price")
lines(predictZ4200, type= "l", col= "Blue")
legend("topleft", c("True", "Prediction"), fill= c("orange", "blue"), )

### Predictions for future ###

# Get predictions only for test data:
predict_future_z2000 <- ts(untransformed_forecast[1, 119:122], start = 2021.5, frequency = 4)
predict_future_z2800 <- ts(untransformed_forecast[2, 119:122], start = 2021.5, frequency = 4)
predict_future_z4000 <- ts(untransformed_forecast[3, 119:122], start = 2021.5, frequency = 4)
predict_future_z4200 <- ts(untransformed_forecast[4, 119:122], start = 2021.5, frequency = 4)

# Plot predictions for the future:
par(mfrow=c(2,2))
plot(ts(data[1, 115:118], start = 2020.5, frequency = 4), type= "l",
     main= "Future predictions for z2000", col= "orange",
     ylabels= "Price", ylim = c(35000, 110000), xlim = c(2020.5, 2022.25))
lines(predict_future_z2000, type= "l", col= "Blue")
lines(ts(pred_interval.z2000[1,], start = 2021.5, frequency = 4), type = "l", lty=2, col = "red")
lines(ts(pred_interval.z2000[2,], start = 2021.5, frequency = 4), type = "l", lty=2, col = "red")
legend("topleft", c("True", "Prediction", "Prediction interval"), fill= c("orange", "blue", "red"))
grid()

plot(ts(data[2, 115:118], start = 2020.5, frequency = 4), type= "l",
     main= "Future predictions for z2000", col= "orange",
     ylabels= "Price", ylim = c(30000, 57000), xlim = c(2020.5, 2022.25))
lines(predict_future_z2800, type= "l", col= "Blue")
lines(ts(pred_interval.z2800[1,], start = 2021.5, frequency = 4), type = "l", lty=2, col = "red")
lines(ts(pred_interval.z2800[2,], start = 2021.5, frequency = 4), type = "l", lty=2, col = "red")
legend("topleft", c("True", "Prediction", "Prediction interval"), fill= c("orange", "blue", "red"))
grid()

plot(ts(data[3, 115:118], start = 2020.5, frequency = 4), type= "l",
     main= "Future predictions for z4000", col= "orange",
     ylabels= ("Price"), ylim = c(17500, 40000), xlim = c(2020.5, 2022.25))
lines(predict_future_z4000, type= "l", col= "Blue")
lines(ts(pred_interval.z4000[1,], start = 2021.5, frequency = 4), type = "l", lty=2, col = "red")
lines(ts(pred_interval.z4000[2,], start = 2021.5, frequency = 4), type = "l", lty=2,  col = "red")
legend("topleft", c("True", "Prediction", "Prediction interval"), fill= c("orange", "blue", "red"))
grid()

plot(ts(data[4, 115:118], start = 2020.5, frequency = 4), type= "l",
     main= "Future predictions for z4200", col= "orange",
     ylab = "Price", ylim = c(8000, 15000), xlim = c(2020.5, 2022.25))
lines(predict_future_z4200, type= "l", col= "Blue")
lines(ts(pred_interval.z4200[1,], start = 2021.5, frequency = 4), type = "l", lty=2, col = "red")
lines(ts(pred_interval.z4200[2,], start = 2021.5, frequency = 4), type = "l", lty=2, col = "red")
legend("topleft", c("True", "Prediction", "Prediction interval"), fill= c("orange", "blue", "red"))
grid()
