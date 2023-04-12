library("forecast")
library(car)
library(marima)
library(readr)

source("C:/Users/magar/OneDrive - Danmarks Tekniske Universitet/Dokumenter/Time Series Analysis/A3/step_slow_marima_2017.R")

A3Data <- read_csv("Time Series Analysis/A3/A3Data.csv")
A3Data <- A3Data[1:122,]

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
marima_model <- marima(t(ln_diffed_Everything),
                       ar.pattern=model_structure$ar.pattern, ma.pattern=model_structure$ma.pattern,
                       Check=FALSE, Plot='log.det', penalty=0)
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

Everything$time <- A3Data$...1
Everything$Capital_forecast <- unlist(forecast_original[1])
Everything$Sealand_forecast <- unlist(forecast_original[2])
Everything$MidJutland_forecast <- unlist(forecast_original[3])
Everything$Rural_forecast <- unlist(forecast_original[4])


ggplot(Everything, aes(time)) + 
  geom_point(aes(y=Capital),col="red") +
  geom_line(aes(y = Capital_forecast,group = 1),col = "red") +
  geom_point(aes(y=Sealand),col="green") +
  geom_line(aes(y = Sealand_forecast,group = 1),col = "green") +
  geom_point(aes(y=MidJutland),col="blue") +
  geom_line(aes(y = MidJutland_forecast,group = 1),col = "blue") +
  geom_point(aes(y=Rural),col="purple") +
  geom_line(aes(y = Rural_forecast,group = 1),col = "purple") +
  scale_x_discrete(labels = everysecond(data$time)) +
  ylim(500,4000) +
  ylab("Price") +
  xlab("Time") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))





