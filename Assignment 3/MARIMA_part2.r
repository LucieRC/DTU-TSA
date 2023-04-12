library("forecast")
library(car)
library(marima)
dev.new()
par("mar")
setwd("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/Assignments/GitHub/Assignment 3")
par(mfrow=c(2,1))

source("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/09/step.slow.marima_2017.R")
source("C:/Users/Lucie/Documents/ECOLES/DTU/Time series analysis/09/step.slow.p.marima_2017.R")


A3Data <- read.csv("A3Data.csv", head=T)
A3Data_regions <- A3Data[1:122,3:8]
matplot(A3Data[,2:8], type="l")
legend("topleft", colnames(A3Data[1:122,2:8]),col=seq_len(8),cex=0.8,lty=seq_len(8))



# HousePrices <- data.frame(A3Data[1:122,2:6])
regions_rates <- data.frame(A3Data_regions)
diffedregions_rates <- data.frame(apply(regions_rates[1:5], 2, diff))
diffedregions_rates$InterestRate <- regions_rates$InterestRate[2:122]
diffedregions_rates$InflationRate <- regions_rates$InflationRate[2:122]
diffedregions_rates

# diffedHousePrices <- data.frame(apply(HousePrices, 2, diff))
# logdiffedHousePrices <- data.frame(apply(log(HousePrices), 2, diff))
# sqrtdiffedHousePrices <- data.frame(apply(sqrt(HousePrices), 2, diff))
# par(mfrow=c(3,1))
# matplot(diffedHousePrices, type="l")
# legend("topleft", colnames(HousePrices),col=seq_len(8),cex=0.8,lty=seq_len(8))
# matplot(logdiffedHousePrices, type="l")
# legend("topleft", colnames(HousePrices),col=seq_len(8),cex=0.8,lty=seq_len(8))
# matplot(sqrtdiffedHousePrices, type="l")
# legend("topleft", colnames(HousePrices),col=seq_len(8),cex=0.8,lty=seq_len(8))


# test1 <- Arima(log(data$Denmark[1:122]),order = c(2,1,0), seasonal = list(order = c(0, 0, 4), period = 4),
#                method = "ML",xreg=cbind(diffinv(data$InflationRate)[1:122],diffinv(data$InterestRate)[1:122])) # How do I infer which integration/summation to use?

diffedEverything <- data.frame(apply(Everything[1:5], 2, diff))
# diffedEverything$InterestRate <- diffinv(diffedEverything$InterestRate)[1:121]
# diffedEverything$InflationRate <- diffinv(diffedEverything$InflationRate)[1:121]

# Try deleting the first lign
diffedEverything$InterestRate <- Everything$InterestRate[2:122]
diffedEverything$InflationRate <- Everything$InflationRate[2:122]
diffedEverything

time <- A3Data$X[2:122]
plot(seq(1,121,1),diffedEverything[,2])

ar = c(1)
ma = c(1)
Model_part2_1 <- define.model(kvar=6, ar=ar, ma=ma, rem.var=c(5,6), indep=c(1:4)) #
# Marima1 <- marima(t(diffedEverything), mean=1, ar=Model_part2$ar.pattern, ma=Model_part2$ma.pattern, Check=FALSE, Plot='log.det', penalty=0)
Marima_part2_1 <- marima(t(diffedEverything), means=1,
                  ar.pattern=Model_part2_1$ar.pattern, ma.pattern=Model_part2_1$ma.pattern,
                  Check=TRUE, Plot='log.det', penalty=0.0)
short.form(Marima_part2_1$ar.estimates, leading=FALSE) # print estimates
short.form(Marima_part2_1$ma.estimates, leading=FALSE)

sl <- step.slow(Marima_part2_1, Everything)
Forecasts <-  arma.forecast(t(Everything[1:122,]), nstart=0, nstep=122, marima=sl)

time <- A3Data$X[2:122]
# pred <- Forecasts$forecasts[2,91:100]
plot(seq(1,122,1), Forecasts$forecasts[2,])#, type="l", xlab="Time",
     #ylab="Denmark house prices", main="...")#,
     #ylim=c(0.0,4.1))

Forecasts$forecasts[2,]

# Using regressor variables
ar = c(1)
ma = c(1)
Model_part2_2 <- define.model(kvar=7, ar=ar, ma=ma, rem.var=c(1), reg.var=c(6,7), indep=NULL) #
# Marima1 <- marima(t(diffedEverything), mean=1, ar=Model_part2$ar.pattern, ma=Model_part2$ma.pattern, Check=FALSE, Plot='log.det', penalty=0)
Marima_part2_2 <- marima(t(diffedEverything), means=1,
                  ar.pattern=Model_part2_2$ar.pattern, ma.pattern=Model_part2_2$ma.pattern,
                  Check=TRUE, Plot='log.det', penalty=0.0)
short.form(Marima_part2_2$ar.estimates, leading=FALSE) # print estimates
short.form(Marima_part2_2$ma.estimates, leading=FALSE)

s2 <- step.slow(Marima_part2_2, Everything)
Forecasts2 <-  arma.forecast(t(diffedEverything[1:122,]), nstart=0, nstep=122, marima=s2)

time <- A3Data$X[2:122]
# pred <- Forecasts$forecasts[2,91:100]
plot(seq(1,122,1), Forecasts2$forecasts[5,])




# # Using regressor variables and the original dataset (not the diffed one):
# ar = c(1)
# ma = c(1)
# Model_part2_3 <- define.model(kvar=7, ar=ar, ma=ma, rem.var=c(1), reg.var=c(6,7), indep=NULL) #
# # Marima1 <- marima(t(diffedEverything), mean=1, ar=Model_part2$ar.pattern, ma=Model_part2$ma.pattern, Check=FALSE, Plot='log.det', penalty=0)
# Marima_part2_3 <- marima(t(Everything), means=1,
#                   ar.pattern=Model_part2_2$ar.pattern, ma.pattern=Model_part2_2$ma.pattern,
#                   Check=TRUE, Plot='log.det', penalty=0.0)
# short.form(Marima_part2_3$ar.estimates, leading=FALSE) # print estimates
# short.form(Marima_part2_3$ma.estimates, leading=FALSE)

# s3 <- step.slow(Marima_part2_3, Everything)
# Forecasts2 <-  arma.forecast(t(Everything[1:122,]), nstart=0, nstep=122, marima=s3)



# Using regressor variables, without diffed
ar = c(1)
ma = c(1)
Model_part2_4 <- define.model(kvar=6, ar=ar, ma=ma, rem.var=c(), reg.var=c(5,6), indep=NULL) #
# Marima1 <- marima(t(diffedEverything), mean=1, ar=Model_part2$ar.pattern, ma=Model_part2$ma.pattern, Check=FALSE, Plot='log.det', penalty=0)
Marima_part2_4 <- marima(t(diffedregions_rates), means=1,
                  ar.pattern=Model_part2_4$ar.pattern, ma.pattern=Model_part2_4$ma.pattern,
                  Check=TRUE, Plot='log.det', penalty=0.0)
# short.form(Marima_part2_4$ar.estimates, leading=FALSE) # print estimates
# short.form(Marima_part2_4$ma.estimates, leading=FALSE)

s4 <- step.slow(Marima_part2_4, regions_rates)
Forecasts4 <-  arma.forecast(t(diffedregions_rates), nstart=0, nstep=122, marima=s4)

# time <- A3Data$X[2:122]
# pred <- Forecasts$forecasts[2,91:100]
plot(seq(1,122,1), Forecasts4$forecasts[5,])
