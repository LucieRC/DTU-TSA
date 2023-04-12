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
Everything <- A3Data[1:122,2:8]

diffed_Everything <- data.frame(apply(Everything[1:5], 2, diff))
diffed_Everything$InterestRate <- Everything$InterestRate[2:122]
diffed_Everything$InflationRate <- Everything$InflationRate[2:122]

ln_Everything <- log(Everything[1:5])
ln_Everything$InterestRate <- Everything$InterestRate[1:122]
ln_Everything$InflationRate <- Everything$InflationRate[1:122]

diffed_ln_Everything <- log(diffed_Everything[1:5])
diffed_ln_Everything$InterestRate <- diffed_Everything$InterestRate[1:121]
diffed_ln_Everything$InflationRate <- diffed_Everything$InflationRate[1:121]

ln_diffed_Everything <- log(Everything[1:5])
ln_diffed_Everything <- data.frame(apply(ln_diffed_Everything, 2, diff))
ln_diffed_Everything$InterestRate <- Everything$InterestRate[1:121]
ln_diffed_Everything$InflationRate <- Everything$InflationRate[1:121]


# ar = c(1)
# ma = c(0)
# model_structure <- define.model(kvar=7, ar=ar, ma=ma, rem.var=c(1), reg.var=c(6,7), indep=NULL) #
# marima_model <- marima(t(Everything), means=1,
#                   ar.pattern=model_structure$ar.pattern, ma.pattern=model_structure$ma.pattern,
#                   Check=FALSE, Plot='log.det', penalty=0.0)
# sl <- step.slow(marima_model, Everything)
# arma_forecast <-  arma.forecast(t(Everything[1:122,]), nstart=0, nstep=122, marima=sl)
# plot(seq(1,122,1), arma_forecast$forecasts[2,])


# ar = c(1)
# ma = c(1)
# model_structure <- define.model(kvar=7, ar=ar, ma=ma, rem.var=c(1), reg.var=c(6,7), indep=NULL) #
# marima_model <- marima(t(Everything), means=1,
#                   ar.pattern=model_structure$ar.pattern, ma.pattern=model_structure$ma.pattern,
#                   Check=FALSE, Plot='log.det', penalty=0.0)
# sl <- step.slow(marima_model, Everything)
# arma_forecast <-  arma.forecast(t(Everything[1:122,]), nstart=0, nstep=122, marima=sl)
# plot(seq(1,122,1), arma_forecast$forecasts[2,])



# ar = c(1)
# ma = c(0)
# model_structure <- define.model(kvar=7, ar=ar, ma=ma, rem.var=c(1), reg.var=c(6,7), indep=NULL) #
# marima_model <- marima(t(diffed_Everything), means=1,
#                   ar.pattern=model_structure$ar.pattern, ma.pattern=model_structure$ma.pattern,
#                   Check=FALSE, Plot='log.det', penalty=0.0)
# sl <- step.slow(marima_model, diffed_Everything)
# arma_forecast_no_ma_all_diffed <- arma.forecast(t(Everything[1:122,]), nstart=0, nstep=122, marima=sl)
# plot(seq(1,121,1), arma_forecast_no_ma_all_diffed$forecasts[2,1:121])
# plot(seq(1,121,1), diffed_Everything[,2])
# #check with the real distrib



# ar = c(1)
# ma = c(0)
# model_structure <- define.model(kvar=7, ar=ar, ma=ma, rem.var=c(1), reg.var=c(6,7), indep=NULL) #
# marima_model <- marima(t(diffed_Everything), means=1,
#                   ar.pattern=model_structure$ar.pattern, ma.pattern=model_structure$ma.pattern,
#                   Check=FALSE, Plot='log.det', penalty=0.0)
# sl <- step.slow(marima_model, diffed_Everything)
# arma_forecast_no_ma_all_diffed <- arma.forecast(t(Everything[1:122,]), nstart=0, nstep=122, marima=sl)
# plot(seq(1,121,1), arma_forecast_no_ma_all_diffed$forecasts[2,1:121])
# plot(seq(1,121,1), diffed_Everything[,2])




ar = c(1)
ma = c(0)
model_structure <- define.model(kvar=7, ar=ar, ma=ma, rem.var=c(1), reg.var=c(6,7), indep=NULL) #
marima_model <- marima(t(ln_diffed_Everything), means=1,
                  ar.pattern=model_structure$ar.pattern, ma.pattern=model_structure$ma.pattern,
                  Check=FALSE, Plot='log.det', penalty=0.0)
sl <- step.slow(marima_model, ln_diffed_Everything)
arma_forecast <- arma.forecast(t(ln_diffed_Everything[1:122,]), nstart=0, nstep=122, marima=sl)
# plot(seq(1,121,1), arma_forecast$forecasts[2,1:121])
# plot(seq(1,121,1), ln_diffed_Everything[,2])

# forecast_2 <- exp(diffinv(arma_forecast$forecasts[2,1:121], xi=ln_Everything[1,2]))

forecast_original <- list()
for (i in seq(2,5,1)) {
    len <- length(forecast_original)
    forecast_original[[len+1]] <- exp(diffinv(arma_forecast$forecasts[i,1:121], xi=ln_Everything[1,i]))
}


# plot(seq(1,122,1), forecast_2)
# plot(seq(1,122,1), Everything[,2])

# typeof(arma_forecast$forecasts[2,1:121])

# (data.frame(ln_diffed_Everything))
# c()
# diffinv(ln_diffed_Everything[,2:5],xi=ln_Everything[1,2:5])
# ln_Everything[,2]
# ln_Everything




# line(seq(1,122,1), Everything[,2])
ggplot(seq(1,122,1), unlist(forecast_original[1]))
plot(seq(1,122,1), unlist(forecast_original[2]))
line(seq(1,122,1), unlist(forecast_original[3]))
plot(seq(1,122,1), Everything[,5])

