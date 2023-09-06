# Set working directory:
setwd('/Users/emilhaugstvedt/Desktop/DTU/TimeSeriesAnalysis/Time-Series-Analysis/Assignment4/R code')

# Load StepSlow function:
source("StepSlow.R")

# Load marima library:
library(marima)

# Load data:
data <- read.csv("../A4_data.csv")

# Separate into training and test:
training_data <- t(ts(data = data[,2:8], start = 1992, frequency = 4))
test_data <- training_data[,115:118]
training_data[1:4, 115:118] <- matrix(data = NaN, nrow = 4, ncol = 4)

# Log transform the data:
log.training_data <- training_data
log.training_data[1:4, 1:114] <- log(training_data[1:4, 1:114])

# Difference the data:
difference <- c(1, 1, 2, 1, 3, 1, 4, 1, 5, 4)
dlog.training_data <- define.dif(series = log.training_data[, 1:114], difference = difference)$y.dif
dlog.dif.poly <- define.dif(series = log.training_data[, 1:114], difference = difference)$dif.poly

##### MARIMA #####

# Create the model:
model <- define.model(kvar=7, ar= (1), ma= (1), reg.var= (5:7))

# Fit the model:
Marima <- marima(DATA = dlog.training_data,
                 ar.pattern = model$ar.pattern,
                 ma.pattern = model$ma.pattern,
                 Plot="log.det",
                 Check=FALSE)

# Use the step slow function to make better predictions
Marima_new <- step.slow(Marima, dlog.training_data, penalty=3.92)

# The estimated parameters for the ar and ma part of the model:
short.form(Marima_new$ar.estimates, leading = FALSE)
short.form(Marima_new$ma.estimates, leading = FALSE)


# Do the forecast:
forecast <- arma.forecast(log.training_data,
                          marima= Marima_new,
                          nstart= 114,
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
  pred_interval.z2000[1,prediction] <- exp(transformed_forecast[1,(114 + prediction)] +
                                             1.96 * sqrt(pred.var[prediction, 1]))
  pred_interval.z2800[1,prediction] <- exp(transformed_forecast[2,(114 + prediction)] +
                                             1.96 * sqrt(pred.var[prediction, 2]))
  pred_interval.z4000[1,prediction] <- exp(transformed_forecast[3,(114 + prediction)] +
                                             1.96 * sqrt(pred.var[prediction, 3]))
  pred_interval.z4200[1,prediction] <- exp(transformed_forecast[4,(114 + prediction)] +
                                             1.96 * sqrt(pred.var[prediction, 4]))

  # Lower:
  pred_interval.z2000[2, prediction] <- exp(transformed_forecast[1,(114 + prediction)] -
                                              1.96 * sqrt(pred.var[prediction, 1]))
  pred_interval.z2800[2, prediction] <- exp(transformed_forecast[2,(114 + prediction)] -
                                              1.96 * sqrt(pred.var[prediction, 2]))
  pred_interval.z4000[2, prediction] <- exp(transformed_forecast[3,(114 + prediction)] -
                                              1.96 * sqrt(pred.var[prediction, 3]))
  pred_interval.z4200[2, prediction] <- exp(transformed_forecast[4,(114 + prediction)] -
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
plot(ts(data$z2000, start = 1992, frequency = 4), type= "l",
     main= "True vs. Pred z2000", col= "orange",
     ylab= "Price")
lines(predictZ2000, type= "l", col= "Blue")
legend("topleft", c("True", "Prediction"), fill= c("orange", "blue"))

plot(ts(data$z2800, start = 1992, frequency = 4), type= "l",
     main= "True vs. Pred z2000", col= "orange",
     ylab= "Price")
lines(predictZ2800, type= "l", col= "Blue")
legend("topleft", c("True", "Prediction"), fill= c("orange", "blue"))

plot(ts(data$z4000, start = 1992, frequency = 4), type= "l",
     main= "True vs. Pred z2000", col= "orange",
     ylab= "Price")
lines(predictZ4000, type= "l", col= "Blue")
legend("topleft", c("True", "Prediction"), fill= c("orange", "blue"))

plot(ts(data$z4200, start = 1992, frequency = 4), type= "l",
     main= "True vs. Pred z2000", col= "orange",
     ylab= "Price")
lines(predictZ4200, type= "l", col= "Blue")
legend("topleft", c("True", "Prediction"), fill= c("orange", "blue"), )

### Predictions for test data ###

# Get predictions only for test data:
predict_test_z2000 <- ts(untransformed_forecast[1, 115:118], start = 2020.5, frequency = 4)
predict_test_z2800 <- ts(untransformed_forecast[2, 115:118], start = 2020.5, frequency = 4)
predict_test_z4000 <- ts(untransformed_forecast[3, 115:118], start = 2020.5, frequency = 4)
predict_test_z4200 <- ts(untransformed_forecast[4, 115:118], start = 2020.5, frequency = 4)

# Plot predictions on test data:
par(mfrow=c(2,2))
plot(ts(test_data[1, ], start = 2020.5, frequency = 4), type= "l",
     main= "True vs. Pred z2000 test data", col= "orange",
     ylabels= "Price", ylim = c(40000, 90000))
lines(predict_test_z2000, type= "l", col= "Blue")
lines(ts(pred_interval.z2000[1,], start = 2020.5, frequency = 4), type = "l", lty=2, col = "red")
lines(ts(pred_interval.z2000[2,], start = 2020.5, frequency = 4), type = "l", lty=2, col = "red")
legend("topleft", c("True", "Prediction", "Prediction interval"), fill= c("orange", "blue", "red"))
grid()

plot(ts(test_data[2, ], start = 2020.5, frequency = 4), type= "l",
     main= "True vs. Pred z2800 test data", col= "orange",
     ylabels= "Price", ylim = c(30000, 55000))
lines(predict_test_z2800, type= "l", col= "Blue")
lines(ts(pred_interval.z2800[1,], start = 2020.5, frequency = 4), type = "l", lty=2, col = "red")
lines(ts(pred_interval.z2800[2,], start = 2020.5, frequency = 4), type = "l", lty=2, col = "red")
legend("topleft", c("True", "Prediction", "Prediction interval"), fill= c("orange", "blue", "red"))
grid()

plot(ts(test_data[3, ], start = 2020.5, frequency = 4), type= "l",
     main= "True vs. Pred z4000 test data", col= "orange",
     ylabels= ("Price"), ylim = c(17500, 30000))
lines(predict_test_z4000, type= "l", col= "Blue")
lines(ts(pred_interval.z4000[1,], start = 2020.5, frequency = 4), type = "l", lty=2, col = "red")
lines(ts(pred_interval.z4000[2,], start = 2020.5, frequency = 4), type = "l", lty=2,  col = "red")
legend("topleft", c("True", "Prediction", "Prediction interval"), fill= c("orange", "blue", "red"))
grid()

plot(ts(test_data[4, ], start = 2020.5, frequency = 4), type= "l",
     main= "True vs. Pred z4200 test data", col= "orange",
     ylab = "Price", xlab = "Quarter", ylim = c(8000, 15000))
lines(predict_test_z4200, type= "l", col= "Blue")
lines(ts(pred_interval.z4200[1,], start = 2020.5, frequency = 4), type = "l", lty=2, col = "red")
lines(ts(pred_interval.z4200[2,], start = 2020.5, frequency = 4), type = "l", lty=2, col = "red")
legend("topleft", c("True", "Prediction", "Prediction interval"), fill= c("orange", "blue", "red"))
grid()
